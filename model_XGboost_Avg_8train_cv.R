# Preparation ####
# Load libraries
library(tidyr)
library(dplyr)
library(randomForest)
library(caret)
library(readxl)
library(ggplot2)
library(sf)
library(patchwork)
library(grid)

library(xgboost)
library(purrr)

# Set working directory
setwd("E:\\POC research\\data")

# Load Data
basin_name_mapping <- c(
  "东北诸河流域"      = "Northeast Rivers Basin",
  "长江流域"          = "Yangtze River Basin",
  "黄河流域"          = "Yellow River Basin",
  "珠江流域"          = "Pearl River Basin",
  "淮河流域"          = "Huaihe Basin",
  "海河流域"          = "Haihe Basin",
  "东南沿海诸河流域"  = "Southeast Coastal Rivers Basin"
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

sum(is.na(poc_data_clean))

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

sum(is.na(X_train))

# Model Building ####
# 10-fold Cross-validation splitting
set.seed(123)
folds <- createFolds(y_train, k = 10, list = TRUE, returnTrain = TRUE)

# Define a parameter set manually
xgb_params <- list(
  objective = "reg:squarederror",
  max_depth = 6,
  eta = 0.1,
  gamma = 0,
  colsample_bytree = 0.7,
  min_child_weight = 1,
  subsample = 1
)

# Train a model on each fold
xgb_models_list <- list()

for (i in seq_along(folds)) {
  cat("Training fold", i, "\n")
  
  train_idx <- folds[[i]]
  X_train_fold <- X_train[train_idx, ]
  y_train_fold <- y_train[train_idx]
  
  dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
  
  xgb_model <- xgb.train(
    params = xgb_params,
    data = dtrain,
    nrounds = 100,
    verbose = 0
  )
  
  xgb_models_list[[i]] <- xgb_model
}

# Function to predict by averaging all 10 models
predict_xgb_ensemble <- function(newdata, models) {
  dnew <- xgb.DMatrix(data = as.matrix(newdata))
  preds <- sapply(models, function(model) {
    predict(model, dnew)
  })
  rowMeans(preds)  # Average across models
}

# Save your final "ensemble" object
best_xgb_ensemble <- xgb_models_list

# ---- Test Predictions (separately for train/test) ----
train_data$Predicted_Concentration <- predict_xgb_ensemble(X_train, best_xgb_ensemble)
test_data$Predicted_Concentration  <- predict_xgb_ensemble(X_test, best_xgb_ensemble)

# 计算 Flux（单位换算）
conversion_factor <- 3.1536e-5
train_data$Predicted_Flux_Tg_year <- train_data$Predicted_Concentration * train_data$`Water_discharge(m3/s)` * conversion_factor
train_data$Observed_Flux_Tg_year  <- train_data$`POC_flux(Tg/year)`

test_data$Predicted_Flux_Tg_year <- test_data$Predicted_Concentration * test_data$`Water_discharge(m3/s)` * conversion_factor
test_data$Observed_Flux_Tg_year  <- test_data$`POC_flux(Tg/year)`

# ---- Model Evaluation (train/test separately) ----
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

print(metrics_conc_train)
print(metrics_conc_test)
print(metrics_flux_train)
print(metrics_flux_test)

# ---- Prepare for plotting (add a column indicating data source) ----
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

# ---- Plotting function with color by DataSource ----
custom_theme <- theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    panel.grid = element_blank(),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(-0.15, "cm"),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    legend.position = "none"
  )

create_basin_plot_color <- function(data, basin_name, x_var, y_var, is_flux = FALSE) {
  plot_data <- data %>% filter(Basin_EN == basin_name)
  
  # Get metrics for train/test separately
  metrics_train <- calc_metrics(plot_data %>% filter(DataSource == "Train") %>% pull(.data[[x_var]]),
                                plot_data %>% filter(DataSource == "Train") %>% pull(.data[[y_var]]))
  metrics_test <- calc_metrics(plot_data %>% filter(DataSource == "Test") %>% pull(.data[[x_var]]),
                               plot_data %>% filter(DataSource == "Test") %>% pull(.data[[y_var]]))
  
  txt_label <- paste0("Train R²=", round(metrics_train$R2, 2),
                      " RMSE=", round(metrics_train$RMSE, 2),
                      "\nTest R²=", round(metrics_test$R2, 2),
                      " RMSE=", round(metrics_test$RMSE, 2))
  
  axis_max <- max(plot_data[[x_var]], plot_data[[y_var]], na.rm = TRUE) * 1.05
  
  ggplot(plot_data, aes(x = .data[[x_var]], y = .data[[y_var]], color = DataSource)) +
    geom_point(size = 2, alpha = 0.7) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40") +
    annotate("text", x = 0.05*axis_max, y = 0.95*axis_max,
             label = txt_label, hjust = 0, vjust = 1, size = 3) +
    coord_fixed(ratio = 1, xlim = c(0, axis_max), ylim = c(0, axis_max)) +
    scale_color_manual(values = c("Train" = "steelblue", "Test" = "firebrick")) +
    labs(x = NULL, y = NULL, color = NULL) +
    ggtitle(basin_name) +
    custom_theme
}

# ---- Generate plots for basin_order ----
basin_order <- c("All basins",
                 "Northeast Rivers Basin",
                 "Haihe Basin",
                 "Yellow River Basin",
                 "Yangtze River Basin",
                 "Southeast Coastal Rivers Basin",
                 "Pearl River Basin")

conc_plots <- map(basin_order, ~ create_basin_plot_color(plot_data_all, .x, "POC(mg/L)", "Predicted_Concentration"))
flux_plots <- map(basin_order, ~ create_basin_plot_color(plot_data_all, .x, "Observed_Flux_Tg_year", "Predicted_Flux_Tg_year"))

# ---- Arrange plots (same layout) ----
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

# Cpoc 浓度
arrange_plots(conc_plots,
              title = "POC Concentration",
              x_label = "Observed POC Concentration (mg/L)",
              y_label = "Predicted POC Concentration (mg/L)")

# Fpoc 通量
arrange_plots(flux_plots,
              title = "POC Flux",
              x_label = "Observed POC Flux (Tg/year)",
              y_label = "Predicted POC Flux (Tg/year)")


# Feature Importance (using only training set!) ####
# Function to calculate permutation importance
calculate_xgb_permutation_importance <- function(models, X, y, n_repeats = 5, metric = "RMSE") {
  # Get baseline predictions from ensemble
  baseline_pred <- predict_xgb_ensemble(X, models)
  
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
      permuted_pred <- predict_xgb_ensemble(X_permuted, models)
      
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

perm_importance_train <- calculate_xgb_permutation_importance(
  models = best_xgb_ensemble,
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
       title = "XGBoost Feature Importance (Train set)") +
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
for (i in seq_along(best_xgb_ensemble)) {
  cat("Computing SHAP for fold", i, "\n")
  model <- best_xgb_ensemble[[i]]
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
  ggtitle("10-fold Averaged SHAP (XGBoost) - Bee Swarm Plot") +
  theme(plot.title = element_text(hjust = 0.5))

# 平均绝对 SHAP 柱状图
sv_importance(sv_avg, kind = "bar", max_display = Inf) + 
  ggtitle("10-fold Averaged Mean Absolute SHAP (XGBoost)") +
  theme(plot.title = element_text(hjust = 0.5))

# SHAP Dependence Plot (示例：Temperature)
sv_dependence(sv_avg, v = "Temperature") + 
  ggtitle("10-fold Averaged SHAP Dependence for Temperature") +
  theme(plot.title = element_text(hjust = 0.5))

# 瀑布图 (示例：第2行)
sv_waterfall(sv_avg, row_id = 2, max_display = 10) + 
  ggtitle("10-fold Averaged SHAP Waterfall (Sample 2)") +
  theme(plot.title = element_text(hjust = 0.5))


# Variable Importance ####
# # Calculate feature importance for each model
# var_imp_list <- lapply(best_xgb_ensemble, function(model) {
#   xgb.importance(model = model) %>% arrange(Feature)
# })
# 
# # Merge importance scores
# all_features <- unique(unlist(lapply(var_imp_list, function(df) df$Feature)))
# 
# # Create a dataframe to hold average Gain
# avg_imp <- data.frame(Feature = all_features, Gain = 0)
# 
# for (imp in var_imp_list) {
#   imp <- imp %>% filter(Feature %in% all_features)
#   avg_imp <- avg_imp %>%
#     left_join(imp[, c("Feature", "Gain")], by = "Feature") %>%
#     mutate(Gain = Gain.x + ifelse(is.na(Gain.y), 0, Gain.y)) %>%
#     select(Feature, Gain)
# }
# 
# # Average the Gain
# avg_imp$Gain <- avg_imp$Gain / length(var_imp_list)
# 
# # Plot
# ggplot(avg_imp, aes(x = reorder(Feature, Gain), y = Gain)) +
#   geom_col(fill = "steelblue") +
#   coord_flip() +
#   labs(x = NULL, y = "Importance (Gain)", title = "Average XGBoost Feature Importance") +
#   custom_theme +
#   theme(plot.title = element_text(hjust = 0.5))

# Outlets Prediction #### 
# Read input variables
new_outlet_data <- read_excel("E:/POC research/data/5_Prediction/Input_Variables.xlsx") %>% as.data.frame()

# Standardize GDP
new_outlet_data$GDP_std <- (new_outlet_data$GDP - mean(new_outlet_data$GDP, na.rm = TRUE)) / sd(new_outlet_data$GDP, na.rm = TRUE)

cols <- c(
  "Avg_DamHeight_m", "Max_DamHeight_m", "Avg_CatchmentArea_km2", "Sum_CatchmentArea_km2",
  "Avg_Depth_m", "Sum_Depth_m", "Avg_Capacity_Mm3", "Sum_Capacity_Mm3",
  "Avg_Discharge_ls", "Sum_Discharge_ls", "Avg_DOR_pc", "Nearest_Dam_Height_m",
  "Nearest_CatchmentArea_km2", "Nearest_Depth_m", "Nearest_Capacity_Mm3",
  "Nearest_Discharge_ls", "Nearest_DOR_pc", "Distance_to_Nearest_Dam_km"
)

new_outlet_data[cols] <- new_outlet_data[cols] %>%
  mutate(across(everything(), ~ ifelse(. == "NA" | is.na(.), 0, as.numeric(.)))) %>%
  mutate(across(where(is.numeric),  ~ replace(.x, is.na(.x) | is.infinite(.x), 0)))

new_outlet_data <- new_outlet_data %>%
  filter_all(all_vars(!is.na(.)))

# Use the same name as Training
new_X <- new_outlet_data %>%
  filter_all(all_vars(!is.na(.))) %>%
  select(all_of(colnames(X))) %>%  # X 是你训练集的特征数据框
  mutate_all(as.numeric)

# Predict Cpoc
new_outlet_data$Predicted_Cpoc <- predict_xgb_ensemble(new_X, best_xgb_ensemble)

# Predict Fpoc
conversion_factor <- 3.1536e-5  # m3/s to Tg/year
new_outlet_data$Predicted_Fpoc_Tg_year <- new_outlet_data$Predicted_Cpoc *
  new_outlet_data$`Q (m3/s)` * conversion_factor

xgb_result <- new_outlet_data %>%
  select(Gridcode, WatershedID, Longitude, Latitude, time, Year, Month, `Q (m3/s)`, Predicted_Cpoc, Predicted_Fpoc_Tg_year)

library(writexl)
write_xlsx(
  xgb_result,
  "E:/POC research/data/5_Prediction/XGBoost_Predicted_Values.xlsx"
)

## Time series ####
library(dplyr)

monthly_avg <- new_outlet_data %>%
  group_by(Month) %>%
  summarise(
    Avg_Concentration = mean(Predicted_Cpoc, na.rm = TRUE),
    Avg_Flux = mean(Predicted_Fpoc_Tg_year, na.rm = TRUE)
  )

library(ggplot2)
ggplot() +
  geom_line(
    data = monthly_avg,
    aes(x = Month, y = Avg_Concentration),
    color = "black", size = 1.5, linetype = "solid"
  ) +
  geom_point(
    data = monthly_avg,
    aes(x = Month, y = Avg_Concentration),
    color = "red", size = 3, shape = 18
  ) +
  # 调整坐标轴和图例
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(
    x = "Month", 
    y = "POC Concentration (mg/L)",
    title = "Monthly Change in POC Concentration"
  ) +
  theme_minimal()

ggplot() +
  geom_line(
    data = monthly_avg,
    aes(x = Month, y = Avg_Flux),
    color = "black", size = 1.5, linetype = "solid"
  ) +
  geom_point(
    data = monthly_avg,
    aes(x = Month, y = Avg_Flux),
    color = "red", size = 3, shape = 18
  ) +
  # 调整坐标轴和图例
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(
    x = "Month", 
    y = "POC Flux (Tg/year)",
    title = "Monthly Change in POC Flux"
  ) +
  theme_minimal()