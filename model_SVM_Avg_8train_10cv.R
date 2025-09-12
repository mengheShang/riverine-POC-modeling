setwd("E:\\POC research\\data")

library(tidyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(sf)
library(caret)
library(e1071)
library(purrr)
library(patchwork)
library(grid)
library(gridExtra)
library(fastshap)
library(shapviz)

# ---- 数据预处理 ----
basin_name_mapping <- c(
  "东北诸河流域" = "Northeast Rivers Basin",
  "长江流域" = "Yangtze River Basin",
  "黄河流域" = "Yellow River Basin",
  "珠江流域" = "Pearl River Basin",
  "淮河流域" = "Huaihe Basin",
  "海河流域" = "Haihe Basin",
  "东南沿海诸河流域" = "Southeast Coastal Rivers Basin"
)

poc_data <- read_excel("2_inputData_watershed\\input_new.xlsx") %>% as.data.frame()

basins <- st_read("1_basin\\8basin.shp")
if (any(!st_is_valid(basins))) {
  basins <- st_make_valid(basins)
}

coords <- poc_data %>% select(Latitude, Longitude)
points_sf <- st_as_sf(coords, coords = c("Longitude", "Latitude"), crs = st_crs(basins))
matched_data <- st_join(points_sf, basins, join = st_within)
if (any(is.na(matched_data$NAME))) {
  unmatched_points <- matched_data %>% filter(is.na(NAME))
  nearest_features <- st_nearest_feature(unmatched_points, basins)
  matched_data$NAME[is.na(matched_data$NAME)] <- basins$NAME[nearest_features]
}
poc_data <- cbind(poc_data, Basin_Name = matched_data$NAME)

cols <- c(
  "Avg_DamHeight_m", "Max_DamHeight_m", "Avg_CatchmentArea_km2", "Sum_CatchmentArea_km2",
  "Avg_Depth_m", "Sum_Depth_m", "Avg_Capacity_Mm3", "Sum_Capacity_Mm3",
  "Avg_Discharge_ls", "Sum_Discharge_ls", "Avg_DOR_pc", "Nearest_Dam_Height_m",
  "Nearest_CatchmentArea_km2", "Nearest_Depth_m", "Nearest_Capacity_Mm3",
  "Nearest_Discharge_ls", "Nearest_DOR_pc", "Distance_to_Nearest_Dam_km"
)

poc_data[cols] <- poc_data[cols] %>%
  mutate(across(everything(), ~ ifelse(. == "NA" | is.na(.), 0, as.numeric(.))))

poc_data_clean <- poc_data %>%  
  mutate(across(-Basin_Name, ~ suppressWarnings(as.numeric(.)))) %>%
  mutate(GDP_std = (GDP - mean(GDP, na.rm = TRUE)) / sd(GDP, na.rm = TRUE)) %>%
  select(- GDP) %>%
  filter_all(all_vars(!is.na(.)))

# ---- 80/20划分 ----
set.seed(123)
train_index <- createDataPartition(poc_data_clean$`POC(mg/L)`, p = 0.8, list = FALSE)
train_data <- poc_data_clean[train_index, ]
test_data  <- poc_data_clean[-train_index, ]

X_train <- train_data %>% select(-ID, -Month, -`POC(mg/L)`, -`Water_discharge(m3/s)`, -`POC_flux(Tg/year)`, -Basin_Name)
y_train <- train_data$`POC(mg/L)`
X_test  <- test_data %>% select(-ID, -Month, -`POC(mg/L)`, -`Water_discharge(m3/s)`, -`POC_flux(Tg/year)`, -Basin_Name)
y_test  <- test_data$`POC(mg/L)`

# ---- 10折交叉验证，保存10个模型 ----
set.seed(123)
folds <- createFolds(y_train, k = 10, list = TRUE, returnTrain = TRUE)
svm_models <- list()

for (i in seq_along(folds)) {
  cat("Training fold", i, "\n")
  fold_train_idx <- folds[[i]]
  X_fold <- X_train[fold_train_idx, ]
  y_fold <- y_train[fold_train_idx]
  
  svm_model <- svm(x = as.matrix(X_fold), y = y_fold, kernel = "radial", cost = 1, gamma = 0.05)
  svm_models[[i]] <- svm_model
}

# ---- 平均预测：训练集和测试集 ----
predict_ensemble <- function(models, newdata) {
  preds <- sapply(models, function(model) {
    predict(model, as.matrix(newdata))
  })
  rowMeans(preds)
}

train_data$Predicted_Concentration <- predict_ensemble(svm_models, X_train)
test_data$Predicted_Concentration  <- predict_ensemble(svm_models, X_test)

# ---- 通量换算 ----
conversion_factor <- 3.1536e-5
train_data$Predicted_Flux_Tg_year <- train_data$Predicted_Concentration *
  train_data$`Water_discharge(m3/s)` * conversion_factor
test_data$Predicted_Flux_Tg_year <- test_data$Predicted_Concentration *
  test_data$`Water_discharge(m3/s)` * conversion_factor
train_data$Observed_Flux_Tg_year <- train_data$`POC_flux(Tg/year)`
test_data$Observed_Flux_Tg_year  <- test_data$`POC_flux(Tg/year)`

# ---- 评估指标 ----
calc_metrics <- function(obs, pred) {
  fit <- lm(obs ~ pred)
  r2  <- summary(fit)$r.squared
  rmse <- sqrt(mean((obs - pred)^2, na.rm = TRUE))
  data.frame(R2 = r2, RMSE = rmse)
}

metrics_conc_train <- calc_metrics(train_data$`POC(mg/L)`, train_data$Predicted_Concentration)
metrics_conc_test  <- calc_metrics(test_data$`POC(mg/L)`, test_data$Predicted_Concentration)

metrics_flux_train <- calc_metrics(train_data$Observed_Flux_Tg_year, train_data$Predicted_Flux_Tg_year)
metrics_flux_test  <- calc_metrics(test_data$Observed_Flux_Tg_year, test_data$Predicted_Flux_Tg_year)

# ---- Prepare for plotting (add DataSource column) ----
train_data$DataSource <- "Train"
test_data$DataSource  <- "Test"
plot_data_all <- bind_rows(train_data, test_data) %>%
  mutate(
    Basin_EN = ifelse(Basin_Name %in% names(basin_name_mapping),
                      basin_name_mapping[Basin_Name],
                      Basin_Name)
  )

# Add "All basins"
plot_data_all <- bind_rows(
  plot_data_all,
  plot_data_all %>% mutate(Basin_EN = "All basins")
)

# ---- Plotting function ----
custom_theme <- theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    panel.grid = element_blank(),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(-0.15, "cm"),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    legend.position = "none"
  )

create_basin_plot_color <- function(data, basin_name, x_var, y_var) {
  plot_data <- data %>% filter(Basin_EN == basin_name)
  
  # 计算训练集/测试集的 R2 和 RMSE
  metrics_train <- calc_metrics(
    plot_data %>% filter(DataSource == "Train") %>% pull(.data[[x_var]]),
    plot_data %>% filter(DataSource == "Train") %>% pull(.data[[y_var]])
  )
  metrics_test <- calc_metrics(
    plot_data %>% filter(DataSource == "Test") %>% pull(.data[[x_var]]),
    plot_data %>% filter(DataSource == "Test") %>% pull(.data[[y_var]])
  )
  
  txt_label <- paste0("Train R²=", round(metrics_train$R2, 2),
                      " RMSE=", round(metrics_train$RMSE, 2),
                      "\nTest R²=", round(metrics_test$R2, 2),
                      " RMSE=", round(metrics_test$RMSE, 2))
  
  axis_max <- max(plot_data[[x_var]], plot_data[[y_var]], na.rm = TRUE) * 1.05
  
  ggplot(plot_data, aes(x = .data[[x_var]], y = .data[[y_var]], color = DataSource)) +
    geom_point(size = 2, alpha = 0.7) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40") +
    annotate("text", x = 0.05 * axis_max, y = 0.95 * axis_max,
             label = txt_label, hjust = 0, vjust = 1, size = 3) +
    coord_fixed(ratio = 1, xlim = c(0, axis_max), ylim = c(0, axis_max)) +
    scale_color_manual(values = c("Train" = "steelblue", "Test" = "firebrick")) +
    labs(x = NULL, y = NULL, color = NULL) +
    ggtitle(basin_name) +
    custom_theme
}

# ---- Arrange plots ----
arrange_plots <- function(plot_list, title, x_label, y_label) {
  layout <- "
  AAB
  AAC
  DEF
  G..
  "
  combined <- wrap_plots(
    A = plot_list[[1]], B = plot_list[[2]], C = plot_list[[3]],
    D = plot_list[[4]], E = plot_list[[5]], F = plot_list[[6]],
    G = plot_list[[7]], design = layout
  )
  # 添加整体标签
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(2, 1, heights = unit(c(0.95, 0.05), "npc"))))
  pushViewport(viewport(layout.pos.row = 1))
  print(combined, newpage = FALSE)
  popViewport()
  # X label
  pushViewport(viewport(layout.pos.row = 2))
  grid.text(x_label, x = 0.5, y = 0.5, gp = gpar(fontsize = 12))
  popViewport()
  # Y label
  pushViewport(viewport(x = 0.05, y = 0.5, angle = 90))
  grid.text(y_label, gp = gpar(fontsize = 12))
  popViewport()
}

# ---- Basin order ----
basin_order <- c("All basins",
                 "Northeast Rivers Basin",
                 "Haihe Basin",
                 "Yellow River Basin",
                 "Yangtze River Basin",
                 "Southeast Coastal Rivers Basin",
                 "Pearl River Basin")

# ---- Generate plots for concentration and flux ----
conc_plots <- map(basin_order, ~ create_basin_plot_color(
  plot_data_all, .x, "POC(mg/L)", "Predicted_Concentration"
))

flux_plots <- map(basin_order, ~ create_basin_plot_color(
  plot_data_all, .x, "Observed_Flux_Tg_year", "Predicted_Flux_Tg_year"
))

# ---- Final arrangement ----
arrange_plots(conc_plots,
              title = "POC Concentration",
              x_label = "Observed POC Concentration (mg/L)",
              y_label = "Predicted POC Concentration (mg/L)")

arrange_plots(flux_plots,
              title = "POC Flux",
              x_label = "Observed POC Flux (Tg/year)",
              y_label = "Predicted POC Flux (Tg/year)")

# ---- 10折平均 SHAP 可视化 ----
predict_function <- function(object, newdata) {
  predict(object, as.matrix(newdata))
}
shap_list <- list()
for (i in seq_along(svm_models)) {
  shap_i <- explain(
    object = svm_models[[i]],
    X = X_train,
    pred_wrapper = predict_function,
    nsim = 20
  )
  shap_list[[i]] <- shap_i
}
shap_avg <- Reduce("+", shap_list) / length(shap_list)
sv_avg <- shapviz(shap_avg, X = X_train)

sv_importance(sv_avg, kind = "bee", max_display = Inf) + 
  ggtitle("10-fold Averaged SHAP (SVM) - Bee Swarm Plot") +
  theme(plot.title = element_text(hjust = 0.5))

# 平均绝对 SHAP 柱状图
sv_importance(sv_avg, kind = "bar", max_display = Inf) + 
  ggtitle("10-fold Averaged Mean Absolute SHAP (SVM)") +
  theme(plot.title = element_text(hjust = 0.5))


