#----Data Preparation & Model Training----

library(tidyr)
library(dplyr)
library(randomForest)
library(caret)
library(readxl)
library(ggplot2)
library(sf)
library(patchwork)  # 用于组合图像
library(purrr)      # 用于函数式编程
library(grid)       # 用于添加全局标签

# 自定义英文名称映射
basin_name_mapping <- c(
  "东北诸河流域"      = "Northeast Rivers Basin",
  "长江流域"          = "Yangtze River Basin",
  "黄河流域"          = "Yellow River Basin",
  "珠江流域"          = "Pearl River Basin",
  # "淮河流域"          = "Huaihe Basin",
  "海河流域"          = "Haihe Basin",
  "东南沿海诸河流域"  = "Southeast Coastal Rivers Basin",
  "西南国际河流流域"  = "Southwest Rivers Basin"
)

setwd("E:\\POC research\\data")
poc_data <- read_excel("2_inputData_watershed\\Input_features.xlsx") %>% as.data.frame() |>
  mutate(
    `POC_flux(Tg/year)` = `POC_flux(g/s)` * 3.15576e7 / 1e12,
    `POC_flux(Tg/year)` = round(`POC_flux(Tg/year)`, 6)
  ) %>%
  dplyr::select(-`POC_flux(g/s)`) %>%
  relocate(`POC_flux(Tg/year)`, .after = last_col())

# 读取流域边界并确保其有效
basins <- st_read("1_basin\\8basin.shp")
if (any(!st_is_valid(basins))) {
  basins <- st_make_valid(basins)
}

# 将经纬度数据转换为空间点
coords <- poc_data %>% dplyr::select(Latitude, Longitude)
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
  dplyr::select(- GDP) %>%
  filter_all(all_vars(!is.na(.)))

sum(is.na(poc_data_clean))

# 准备特征和目标变量
X <- poc_data_clean %>% dplyr::select(-ID, -Month, 
                               -Year, -Longitude, -Latitude,
                               -`POC(mg/L)`, -`Water_discharge(m3/s)`, -`POC_flux(Tg/year)`, 
                               -Basin_Name,
                               -Discharge_filled_by_model)
y <- poc_data_clean$`POC(mg/L)`

sum(is.na(X))

#----10-Fold Cross-Validation----
## parameters ####
# # 定义控制参数用于交叉验证
# cv_control <- trainControl(
#   method = "cv",          # 交叉验证
#   number = 10,            # 10 折
#   verboseIter = TRUE,      # 显示每次迭代过程
#   savePredictions = "final"
# )
# 
# # 使用 caret 的 train() 函数执行随机森林模型的交叉验证
# set.seed(123)
# rf_cv_model <- train(
#   x = X,
#   y = y,
#   method = "rf",
#   trControl = cv_control,
#   tuneLength = 30,  
#   importance = TRUE
# )
# 
# # 打印交叉验证结果
# print(rf_cv_model)
# plot(rf_cv_model)

## all data together ####
# 
# # 用最优参数在所有数据上重新训练
# best_mtry <- rf_cv_model$bestTune$mtry
# library(randomForest)
# final_rf_model <- randomForest(
#   x = X,
#   y = y,
#   mtry = best_mtry,
#   importance = TRUE
# )
# 
# # 用新训练的模型预测
# poc_data_clean$Predicted_Concentration <- predict(final_rf_model, X)

## average ####
set.seed(123)
folds <- createFolds(y, k = 10, list = TRUE, returnTrain = TRUE)

rf_models_list <- list()

for (i in seq_along(folds)) {
  cat("Training fold", i, "\n")
  
  fold_train_idx <- folds[[i]]
  X_fold <- X[fold_train_idx, ]
  y_fold <- y[fold_train_idx]
  
  rf_model <- randomForest(
    x = X_fold,
    y = y_fold,
    ntree = 500, 
    mtry = 18
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
poc_data_clean$Predicted_Concentration <- predict_rf_ensemble(X, rf_models_list)

#----Unit Conversion----

conversion_factor <- 3.1536e-5
poc_data_clean$Predicted_Flux_Tg_year <- poc_data_clean$Predicted_Concentration *
  poc_data_clean$`Water_discharge(m3/s)` * conversion_factor
poc_data_clean$Observed_Flux_Tg_year <- poc_data_clean$`POC_flux(Tg/year)`

#----Evaluation Metrics----

calc_metrics <- function(obs, pred) {
  fit <- lm(obs ~ pred)
  r2  <- summary(fit)$r.squared
  rmse <- sqrt(mean((obs - pred)^2, na.rm = TRUE))
  data.frame(R2 = r2, RMSE = rmse)
}

# 计算整体指标
metrics_conc <- calc_metrics(poc_data_clean$`POC(mg/L)`, poc_data_clean$Predicted_Concentration)
metrics_flux <- calc_metrics(poc_data_clean$Observed_Flux_Tg_year, poc_data_clean$Predicted_Flux_Tg_year)

# Combine & Prepare Plot Data

evaluation_data_final <- poc_data_clean %>%
  mutate(
    Set = "Full",  # 所有数据用于交叉验证
    Basin_EN = ifelse(Basin_Name %in% names(basin_name_mapping),
                      basin_name_mapping[Basin_Name],
                      Basin_Name)
  )

# 增加一个"Basin_EN"叫做"All basins"来表示整体
all_data <- evaluation_data_final %>% mutate(Basin_EN = "All basins")
evaluation_data_final <- bind_rows(evaluation_data_final, all_data)

# 计算按流域划分的评估指标
metrics_by_basin <- evaluation_data_final %>%
  group_by(Basin_EN) %>%
  summarise(
    R2 = calc_metrics(`POC(mg/L)`, Predicted_Concentration)$R2,
    RMSE = calc_metrics(`POC(mg/L)`, Predicted_Concentration)$RMSE,
    .groups = "drop"
  )
flux_metrics <- evaluation_data_final %>%
  group_by(Basin_EN) %>%
  summarise(
    R2 = calc_metrics(Observed_Flux_Tg_year, Predicted_Flux_Tg_year)$R2,
    RMSE = calc_metrics(Observed_Flux_Tg_year, Predicted_Flux_Tg_year)$RMSE,
    .groups = "drop"
  )

#----Plotting Configuration----

# 自定义主题
custom_theme <- theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    panel.grid = element_blank(),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(-0.15, "cm"),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold")
  )

#----Generate Plots----

create_basin_plot <- function(data, basin_name, x_var, y_var, metrics) {
  plot_data <- data %>% filter(Basin_EN == basin_name)

  # 提取指标
  basin_metrics <- metrics %>% filter(Basin_EN == basin_name)

  if (nrow(basin_metrics) == 0 || is.na(basin_metrics$R2) || is.na(basin_metrics$RMSE)) {
    txt_label <- "R² = NA\nRMSE = NA"
  } else {
    txt_label <- paste0("R² = ", round(basin_metrics$R2, 2),
                        "\nRMSE = ", round(basin_metrics$RMSE, 2))
  }

  # 动态坐标轴范围
  axis_max <- max(plot_data[[x_var]], plot_data[[y_var]], na.rm = TRUE) * 1.05

  ggplot(plot_data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_point(color = "steelblue", size = 2, alpha = 0.7) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40") +
    annotate("text", x = 0.05*axis_max, y = 0.95*axis_max,
             label = txt_label, hjust = 0, vjust = 1, size = 3.5) +
    coord_fixed(ratio = 1, xlim = c(0, axis_max), ylim = c(0, axis_max)) +
    labs(x = NULL, y = NULL) +
    ggtitle(basin_name) +
    custom_theme
}

# 定义流域顺序
basin_order <- c("All basins",
                 "Northeast Rivers Basin",
                 "Haihe Basin",
                 # "Huaihe Basin", 
                 "Yellow River Basin",
                 "Yangtze River Basin",
                 "Southeast Coastal Rivers Basin",
                 "Southwest Rivers Basin",
                 "Pearl River Basin")

# 生成浓度图
conc_plots <- map(basin_order, ~ create_basin_plot(
  evaluation_data_final, .x,
  "POC(mg/L)", "Predicted_Concentration", metrics_by_basin
))

# 生成通量图
flux_plots <- map(basin_order, ~ create_basin_plot(
  evaluation_data_final, .x,
  "Observed_Flux_Tg_year", "Predicted_Flux_Tg_year", flux_metrics
))

#----Arrange Plots with Proper Labeling----

# 自定义布局函数
arrange_plots <- function(plot_list, title) {
  layout <- "
  AAB
  AAC
  DEF
  GH.
  "
  
  combined <- wrap_plots(
    A = plot_list[[1]], B = plot_list[[2]], C = plot_list[[3]],
    D = plot_list[[4]], E = plot_list[[5]], F = plot_list[[6]],
    G = plot_list[[7]], H = plot_list[[8]], design = layout
  )

  # 添加全局标签
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(2, 1, heights = unit(c(0.95, 0.05), "npc"))))
  
  # 主图
  pushViewport(viewport(layout.pos.row = 1))
  print(combined, newpage = FALSE)
  popViewport()
  
  # X 轴标签
  pushViewport(viewport(layout.pos.row = 2))
  grid.text("Observed Values", x = 0.5, y = 0.5, gp = gpar(fontsize = 12))
  popViewport()
  
  # Y 轴标签
  pushViewport(viewport(x = 0.05, y = 0.5, angle = 90))
  grid.text("Estimated Values", gp = gpar(fontsize = 12))
  popViewport()
}

# 生成最终输出
arrange_plots(conc_plots, "POC Concentration")
arrange_plots(flux_plots, "POC Flux")

#----Interactive plots----
library(plotly)

create_basin_plot_interactive <- function(data, basin_name, x_var, y_var, metrics) {
  plot_data <- data %>% 
    filter(Basin_EN == basin_name) %>%
    mutate(Discharge_filled_by_model = factor(
      Discharge_filled_by_model,
      levels = c(0, 1),
      labels = c("No", "Yes")
    ))
  
  basin_metrics <- metrics %>% filter(Basin_EN == basin_name)
  
  txt_label <- if (nrow(basin_metrics) == 0 || is.na(basin_metrics$R2) || is.na(basin_metrics$RMSE)) {
    "R² = NA\nRMSE = NA"
  } else {
    paste0("R² = ", round(basin_metrics$R2, 2),
           "\nRMSE = ", round(basin_metrics$RMSE, 2))
  }
  
  axis_max <- max(plot_data[[x_var]], plot_data[[y_var]], na.rm = TRUE) * 1.05
  
  p <- ggplot(plot_data, aes(
    x = .data[[x_var]],
    y = .data[[y_var]],
    shape = Discharge_filled_by_model,
    text = paste("Station ID:", ID,  # <--- 替换为你实际的 ID 列名
                 "<br>Obs:", .data[[x_var]],
                 "<br>Pred:", .data[[y_var]])
  )) +
    geom_point(color = "steelblue", size = 2, alpha = 0.7) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40") +
    annotate("text", x = 0.05*axis_max, y = 0.95*axis_max,
             label = txt_label, hjust = 0, vjust = 1, size = 3.5) +
    coord_fixed(ratio = 1, xlim = c(0, axis_max), ylim = c(0, axis_max)) +
    scale_shape_manual(
      values = c("No" = 16, "Yes" = 17),
      name = "Filled by Model"
    ) +
    labs(x = NULL, y = NULL, shape = "Filled by Model", title = basin_name) +
    custom_theme
  
  ggplotly(p, tooltip = "text")  # Convert to interactive
}

# 生成交互式浓度图
conc_plots_interactive <- map(basin_order, ~ create_basin_plot_interactive(
  evaluation_data_final, .x,
  "POC(mg/L)", "Predicted_Concentration", metrics_by_basin
))

# 生成交互式通量图
flux_plots_interactive <- map(basin_order, ~ create_basin_plot_interactive(
  evaluation_data_final, .x,
  "Observed_Flux_Tg_year", "Predicted_Flux_Tg_year", flux_metrics
))

conc_plots_interactive[[1]]  # 查看第一个流域浓度图
flux_plots_interactive[[1]]  # 查看第一个流域通量图

#----Feature Permutation Importance----

# Function to calculate permutation importance
calculate_permutation_importance <- function(model, X, y, n_repeats = 10, metric = "RMSE") {
  baseline_pred <- predict_rf_ensemble(X, model)
  baseline_pred <- as.numeric(baseline_pred)
  if (metric == "RMSE") {
    baseline_score <- sqrt(mean((y - baseline_pred)^2))
  } else if (metric == "R2") {
    baseline_score <- summary(lm(y ~ baseline_pred))$r.squared
  }
  
  importance_df <- data.frame()
  
  for (feature in colnames(X)) {
    feature_scores <- numeric(n_repeats)
    
    for (i in 1:n_repeats) {
      X_permuted <- X
      X_permuted[[feature]] <- sample(X_permuted[[feature]])
      
      permuted_pred <- predict(model, X_permuted)
      permuted_pred <- rowMeans(do.call(cbind, permuted_pred))
      
      if (metric == "RMSE") {
        feature_scores[i] <- sqrt(mean((y - permuted_pred)^2))
      } else if (metric == "R2") {
        feature_scores[i] <- summary(lm(y ~ permuted_pred))$r.squared
      }
    }
    
    if (metric == "RMSE") {
      importance <- mean(feature_scores) - baseline_score
    } else if (metric == "R2") {
      importance <- baseline_score - mean(feature_scores)
    }
    
    importance_df <- rbind(importance_df, 
                           data.frame(Feature = feature, 
                                      Importance = importance,
                                      stringsAsFactors = FALSE))
  }
  
  # Normalize importance to 0-100 scale
  importance_df$Importance <- 100 * importance_df$Importance / max(importance_df$Importance)
  
  return(importance_df[order(-importance_df$Importance), ])
}

# Calculate permutation importance using RMSE metric
perm_importance <- calculate_permutation_importance(
  model = best_rf_ensemble,
  X = X,
  y = y,
  n_repeats = 10,
  metric = "RMSE"
)

# Plot the importance
ggplot(perm_importance, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(x = "Feature", y = "Permutation Importance (RMSE-based, % of max)", 
       title = "Random Forest Model Feature Importance by Permutation") +
  theme_minimal()

# Variable Importance Visualization ####

# var_imp <- importance(final_rf_model)
# var_imp_df <- data.frame(
#   Variable = rownames(var_imp),
#   Importance = var_imp[, "%IncMSE", drop = TRUE]
# )
# 
# # 绘制特征重要性图
# var_importance_plot <- ggplot(var_imp_df, aes(x = reorder(Variable, Importance), y = Importance)) +
#   geom_col(fill = "steelblue") +
#   coord_flip() +
#   labs(x = NULL, y = "Importance", title = "Random Forest Feature Importance") +
#   custom_theme +
#   theme(plot.title = element_text(hjust = 0.5))
# print(var_importance_plot)

## SHAP ####
# 取训练集
X_shap <- X

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
  mutate(across(where(is.numeric),  ~ replace(.x, is.na(.x) | is.infinite(.x), 0))) %>% 
  filter_all(all_vars(!is.na(.)))

new_outlet_data <- new_outlet_data %>%
  filter_all(all_vars(!is.na(.)))

# Use the same name as Training
new_X <- new_outlet_data %>%
  dplyr::select(all_of(colnames(X))) %>%  # X 是你训练集的特征数据框
  mutate_all(as.numeric)

# Predict Cpoc
new_outlet_data$Predicted_Cpoc <- predict(final_rf_model, new_X)

# Predict Fpoc
conversion_factor <- 3.1536e-5  # m3/s to Tg/year
new_outlet_data$Predicted_Fpoc_Tg_year <- new_outlet_data$Predicted_Cpoc *
  new_outlet_data$`Q (m3/s)` * conversion_factor

rf_result <- new_outlet_data %>%
  dplyr::select(Gridcode, WatershedID, time, Year, Month, `Q (m3/s)`, Predicted_Cpoc, Predicted_Fpoc_Tg_year)

library(writexl)
write_xlsx(
  rf_result,
  "E:/POC research/data/5_Prediction/RandomForest_Predicted_Values.xlsx"
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
  # 绘制跨年月均值（黑色粗线 + 点突出显示）
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
  scale_color_viridis_d(name = "Year") +
  labs(
    x = "Month", 
    y = "POC Concentration (mg/L)",
    title = "Monthly Change in POC Concentration"
  ) +
  theme_minimal()

ggplot() +
  # 绘制跨年月均值（黑色粗线 + 点突出显示）
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
  scale_color_viridis_d(name = "Year") +
  labs(
    x = "Month", 
    y = "POC Flux (Tg/year)",
    title = "Monthly Change in POC Flux"
  ) +
  theme_minimal()
