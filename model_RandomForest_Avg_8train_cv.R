#----Data Preparation & Model Training----

library(tidyr)
library(dplyr)
library(randomForest)
library(caret)
library(readxl)
library(writexl)
library(ggplot2)
library(sf)
library(grid)
library(cowplot)
library(patchwork)

# 自定义英文名称映射
basin_name_mapping <- c(
  "东北诸河流域"      = "Northeast Rivers Basin",
  "长江流域"          = "Yangtze River Basin",
  "黄河流域"          = "Yellow River Basin",
  "珠江流域"          = "Pearl River Basin",
  "淮河流域"          = "Huaihe Basin",
  "海河流域"          = "Haihe Basin",
  "东南沿海诸河流域"  = "Southeast Coastal Rivers Basin"
)

setwd("E:\\POC research\\data")
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

sum(is.na(poc_data_clean))

write_xlsx(poc_data_clean, "2_inputData_watershed\\input_new_clean.xlsx")

# 划分 80% 训练集和 20% 测试集
set.seed(123)
train_index <- createDataPartition(poc_data_clean$`POC(mg/L)`, p = 0.8, list = FALSE)
train_data <- poc_data_clean[train_index, ]
test_data  <- poc_data_clean[-train_index, ]

X_train <- train_data %>% select(-ID,
                                 -`POC(mg/L)`, -`Water_discharge(m3/s)`, -`POC_flux(Tg/year)`, 
                                 -Basin_Name)
y_train <- train_data$`POC(mg/L)`

X_test <- test_data %>% select(-ID,
                               -`POC(mg/L)`, -`Water_discharge(m3/s)`, -`POC_flux(Tg/year)`, 
                               -Basin_Name)
y_test <- test_data$`POC(mg/L)`


# 1. 定义参数网格
mtry_grid <- expand.grid(mtry = c(10, 15, 20, 25, 30, 35, 40, 45, 50, 60))  # 你可以结合你数据的p值来设

# 2. 用 caret 包的 train() 做 10折交叉验证
control <- trainControl(method = "cv", number = 10)
set.seed(123)
rf_tuned <- train(
  x = X_train, y = y_train,
  method = "rf",
  tuneGrid = mtry_grid,
  trControl = control,
  ntree = 500
)

# 3. 查看最优 mtry
print(rf_tuned$bestTune)
print(rf_tuned$results)


set.seed(123)
rf_model <- randomForest(
  x = X_train,
  y = y_train,
  ntree = 1000,
  importance = TRUE
)

# 查看 OOB误差随ntree变化
plot(rf_model, main = "OOB Error vs Number of Trees")


#----10-Fold Cross-Validation on Train Set (Random Forest)----

set.seed(123)
folds <- createFolds(y_train, k = 10, list = TRUE, returnTrain = TRUE)

rf_models_list <- list()

for (i in seq_along(folds)) {
  cat("Training fold", i, "\n")
  
  fold_train_idx <- folds[[i]]
  X_train_fold <- X_train[fold_train_idx, ]
  y_train_fold <- y_train[fold_train_idx]
  
  rf_model <- randomForest(
    x = X_train_fold,
    y = y_train_fold,
    ntree = 500
  )
  
  rf_models_list[[i]] <- rf_model
}

best_rf_ensemble <- rf_models_list

#----Function to Average Predictions from 10 Models----
predict_rf_ensemble <- function(newdata, models) {
  preds <- sapply(models, function(model) {
    predict(model, newdata)
  })
  rowMeans(preds)  # 平均预测
}

#----Predictions----
train_data$Predicted_Concentration <- predict_rf_ensemble(X_train, rf_models_list)
test_data$Predicted_Concentration  <- predict_rf_ensemble(X_test, rf_models_list)

# Unit Conversion for Flux
conversion_factor <- 3.1536e-5
train_data$Predicted_Flux_Tg_year <- train_data$Predicted_Concentration *
  train_data$`Water_discharge(m3/s)` * conversion_factor

test_data$Predicted_Flux_Tg_year <- test_data$Predicted_Concentration *
  test_data$`Water_discharge(m3/s)` * conversion_factor

train_data$Observed_Flux_Tg_year <- train_data$`POC_flux(Tg/year)`
test_data$Observed_Flux_Tg_year  <- test_data$`POC_flux(Tg/year)`

#----Evaluation Metrics----
calc_metrics <- function(obs, pred) {
  fit <- lm(obs ~ pred)
  r2  <- summary(fit)$r.squared
  rmse <- sqrt(mean((obs - pred)^2, na.rm = TRUE))
  data.frame(R2 = r2, RMSE = rmse)
}

train_metrics_conc <- calc_metrics(train_data$`POC(mg/L)`, train_data$Predicted_Concentration)
test_metrics_conc  <- calc_metrics(test_data$`POC(mg/L)`,  test_data$Predicted_Concentration)
train_metrics_flux <- calc_metrics(train_data$Observed_Flux_Tg_year, train_data$Predicted_Flux_Tg_year)
test_metrics_flux  <- calc_metrics(test_data$Observed_Flux_Tg_year,  test_data$Predicted_Flux_Tg_year)

print(train_metrics_conc)
print(test_metrics_conc)
print(train_metrics_flux)
print(test_metrics_flux)

#----Visualization with grid layout----
library(purrr)

custom_theme <- theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    panel.grid = element_blank(),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(-0.15, "cm"),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    legend.position = "none"
  )

## Cpoc ####
evaluation_data <- bind_rows(
  train_data %>% mutate(Set = "Train"),
  test_data  %>% mutate(Set = "Test")
) %>%
  mutate(
    Basin_EN = ifelse(Basin_Name %in% names(basin_name_mapping),
                      basin_name_mapping[Basin_Name],
                      Basin_Name)
  )

all_data <- evaluation_data %>% mutate(Basin_EN = "All basins")
evaluation_data_final <- bind_rows(evaluation_data, all_data)

metrics_by_basin_set <- evaluation_data_final %>%
  group_by(Basin_EN, Set) %>%
  do({
    conc_res <- calc_metrics(.$`POC(mg/L)`, .$Predicted_Concentration)
    data.frame(R2 = conc_res$R2, RMSE = conc_res$RMSE)
  }) %>%
  ungroup()



create_basin_plot <- function(data, basin_name) {
  plot_data <- data %>% filter(Basin_EN == basin_name)
  
  train_metrics <- metrics_by_basin_set %>% 
    filter(Basin_EN == basin_name, Set == "Train")
  test_metrics <- metrics_by_basin_set %>% 
    filter(Basin_EN == basin_name, Set == "Test")
  
  txt_label <- paste0("Train: R²=", round(train_metrics$R2, 2),
                      " RMSE=", round(train_metrics$RMSE, 2),
                      "\nTest: R²=", round(test_metrics$R2, 2),
                      " RMSE=", round(test_metrics$RMSE, 2))
  
  axis_max <- max(plot_data$`POC(mg/L)`, plot_data$Predicted_Concentration, na.rm = TRUE) * 1.05
  
  ggplot(plot_data, aes(x = `POC(mg/L)`, y = Predicted_Concentration, color = Set)) +
    geom_point(size = 2, alpha = 0.7) +
    scale_color_manual(values = c("Train" = "steelblue", "Test" = "firebrick")) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40") +
    annotate("text", x = 0.05*axis_max, y = 0.95*axis_max,
             label = txt_label, hjust = 0, vjust = 1, size = 3.5) +
    coord_fixed(ratio = 1, xlim = c(0, axis_max), ylim = c(0, axis_max)) +
    labs(x = NULL, y = NULL) +
    ggtitle(basin_name) +
    custom_theme
}

basin_order <- c("All basins",
                 "Northeast Rivers Basin",
                 "Haihe Basin",
                 "Yellow River Basin",
                 "Yangtze River Basin",
                 "Southeast Coastal Rivers Basin",
                 "Pearl River Basin")

conc_plots <- map(basin_order, ~ create_basin_plot(evaluation_data_final, .x))

## Fpoc ####
evaluation_data_flux <- bind_rows(
  train_data %>% mutate(Set = "Train"),
  test_data  %>% mutate(Set = "Test")
) %>%
  mutate(
    Basin_EN = ifelse(Basin_Name %in% names(basin_name_mapping),
                      basin_name_mapping[Basin_Name],
                      Basin_Name)
  )
all_data_flux <- evaluation_data_flux %>% mutate(Basin_EN = "All basins")
evaluation_data_flux_final <- bind_rows(evaluation_data_flux, all_data_flux)

metrics_by_basin_set_flux <- evaluation_data_flux_final %>%
  group_by(Basin_EN, Set) %>%
  do({
    flux_res <- calc_metrics(.$Observed_Flux_Tg_year, .$Predicted_Flux_Tg_year)
    data.frame(R2 = flux_res$R2, RMSE = flux_res$RMSE)
  }) %>%
  ungroup()

create_basin_plot_flux <- function(data, basin_name) {
  plot_data <- data %>% filter(Basin_EN == basin_name)
  
  train_metrics <- metrics_by_basin_set_flux %>% 
    filter(Basin_EN == basin_name, Set == "Train")
  test_metrics <- metrics_by_basin_set_flux %>% 
    filter(Basin_EN == basin_name, Set == "Test")
  
  txt_label <- paste0("Train: R²=", round(train_metrics$R2, 2),
                      " RMSE=", round(train_metrics$RMSE, 2),
                      "\nTest: R²=", round(test_metrics$R2, 2),
                      " RMSE=", round(test_metrics$RMSE, 2))
  
  axis_max <- max(plot_data$Observed_Flux_Tg_year, plot_data$Predicted_Flux_Tg_year, na.rm = TRUE) * 1.05
  
  ggplot(plot_data, aes(x = Observed_Flux_Tg_year, y = Predicted_Flux_Tg_year, color = Set)) +
    geom_point(size = 2, alpha = 0.7) +
    scale_color_manual(values = c("Train" = "steelblue", "Test" = "firebrick")) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40") +
    annotate("text", x = 0.05*axis_max, y = 0.95*axis_max,
             label = txt_label, hjust = 0, vjust = 1, size = 3.5) +
    coord_fixed(ratio = 1, xlim = c(0, axis_max), ylim = c(0, axis_max)) +
    labs(x = NULL, y = NULL) +
    ggtitle(basin_name) +
    custom_theme
}

flux_plots <- map(basin_order, ~ create_basin_plot_flux(evaluation_data_flux_final, .x))

# 自定义图形布局函数
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
  
  # Add global labels using grid
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(2, 1, heights = unit(c(0.95, 0.05), "npc"))))
  
  # Main plots
  pushViewport(viewport(layout.pos.row = 1))
  print(combined, newpage = FALSE)
  popViewport()
  
  # X-axis label
  pushViewport(viewport(layout.pos.row = 2))
  grid.text(x_label, x = 0.5, y = 0.5, gp = gpar(fontsize = 12))
  popViewport()
  
  # Y-axis label
  pushViewport(viewport(x = 0.05, y = 0.5, angle = 90))
  grid.text(y_label, gp = gpar(fontsize = 12))
  popViewport()
}

arrange_plots(conc_plots, x_label = "Observed C_POC (mg/L)", y_label = "Predicted C_POC (mg/L)")
arrange_plots(flux_plots, x_label = "Observed F_POC (TgC/year)", y_label = "Predicted F_POC (TgC/year)")

# Variable Importance Plot ####
# 
# var_imp <- importance(rf_cv_model)
# var_imp_df <- data.frame(
#   Variable = rownames(var_imp),
#   Importance = var_imp[, "%IncMSE", drop=TRUE]
# )
# 
# var_importance_plot <- ggplot(var_imp_df, aes(x=reorder(Variable, Importance), y=Importance)) +
#   geom_bar(stat="identity", fill="steelblue") +
#   coord_flip() +
#   labs(x = "Variables", y = "%IncMSE", title = "Variable Importance") +
#   custom_theme
# 
# print(var_importance_plot)


# Feature Importance ####

# Function to calculate permutation importance
calculate_rf_permutation_importance <- function(models, X, y, n_repeats = 5, metric = "RMSE") {
  # Get baseline predictions from ensemble
  baseline_pred <- predict_rf_ensemble(X, models)
  
  if (metric == "RMSE") {
    baseline_score <- sqrt(mean((y - baseline_pred)^2))
  } else if (metric == "R2") {
    baseline_score <- summary(lm(y ~ baseline_pred))$r.squared
  }
  
  importance_df <- data.frame(Feature = colnames(X), Importance = 0, stringsAsFactors = FALSE)
  
  for (feature in colnames(X)) {
    feature_scores <- numeric(n_repeats)
    
    for (i in 1:n_repeats) {
      # Create permuted version of the data
      X_permuted <- X
      X_permuted[[feature]] <- sample(X_permuted[[feature]])
      
      # Get predictions from ensemble
      permuted_pred <- predict_rf_ensemble(X_permuted, models)
      
      # Calculate score
      if (metric == "RMSE") {
        feature_scores[i] <- sqrt(mean((y - permuted_pred)^2))
      } else if (metric == "R2") {
        feature_scores[i] <- summary(lm(y ~ permuted_pred))$r.squared
      }
    }
    
    # Calculate importance
    if (metric == "RMSE") {
      importance <- mean(feature_scores) - baseline_score  # Higher RMSE = more important
    } else if (metric == "R2") {
      importance <- baseline_score - mean(feature_scores)  # Larger R2 drop = more important
    }
    
    importance_df[importance_df$Feature == feature, "Importance"] <- importance
  }
  
  # Normalize importance to 0-100 scale
  importance_df$Importance <- 100 * importance_df$Importance / max(importance_df$Importance)
  
  return(importance_df[order(-importance_df$Importance), ])
}

set.seed(123)

perm_importance_train <- calculate_rf_permutation_importance(
  models = best_rf_ensemble,
  X = X_train,
  y = y_train,
  n_repeats = 5,
  metric = "RMSE"
)

ggplot(perm_importance_train, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(x = "Feature",
       y = "Permutation Importance (RMSE-based, % of max)",
       title = "Random Forest Feature Importance (Train set)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


## SHAP ####
# 取训练集
X_shap <- X_train

# 定义预测函数
predict_function <- function(object, newdata) {
  predict(object, newdata = as.matrix(newdata))
}

# 计算 10 折模型的 SHAP 矩阵
library(fastshap)
shap_list <- list()
for (i in seq_along(best_rf_ensemble)) {
  cat("Computing SHAP for fold", i, "\n")
  model <- best_rf_ensemble[[i]]
  shap_i <- explain(
    object = model,
    X = X_shap,
    pred_wrapper = predict_function,
    nsim = 20
  )
  shap_list[[i]] <- shap_i
}

# 计算 SHAP 平均值矩阵
shap_avg <- Reduce("+", shap_list) / length(shap_list)

# 创建 shapviz 对象
library(shapviz)
sv_avg <- shapviz(shap_avg, X = X_shap)

# ---- 可视化 ----

# 蜂群图
sv_importance(sv_avg, kind = "bee", max_display = Inf) + 
  ggtitle("10-fold Averaged SHAP (Random Forest) - Bee Swarm Plot") +
  theme(plot.title = element_text(hjust = 0.5))

# 平均绝对 SHAP 柱状图
sv_importance(sv_avg, kind = "bar", max_display = Inf) + 
  ggtitle("10-fold Averaged Mean Absolute SHAP (Random Forest)") +
  theme(plot.title = element_text(hjust = 0.5))

# SHAP Dependence Plot (示例：Temperature)
sv_dependence(sv_avg, v = "Temperature") + 
  ggtitle("10-fold Averaged SHAP Dependence for Temperature") +
  theme(plot.title = element_text(hjust = 0.5))

# 瀑布图 (示例：第2行)
sv_waterfall(sv_avg, row_id = 2, max_display = 10) + 
  ggtitle("10-fold Averaged SHAP Waterfall (Sample 2)") +
  theme(plot.title = element_text(hjust = 0.5))

