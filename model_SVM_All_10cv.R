setwd("E:\\POC research\\data")

library(tidyr)       # 数据整理
library(dplyr)       # 数据操作
library(readxl)      # 读取Excel文件
library(ggplot2)     # 数据可视化
library(sf)          # 空间数据处理
library(caret)       # 机器学习建模
library(e1071)       # SVM实现
library(purrr)       # 函数式编程工具
library(patchwork)   # 图形组合
library(grid)        # 图形系统
library(gridExtra)   # 图形布局

# Data Preparation & Model Training ####
# Custom English name mapping
basin_name_mapping <- c(
  "东北诸河流域"      = "Northeast Rivers Basin",
  "长江流域"          = "Yangtze River Basin",
  "黄河流域"          = "Yellow River Basin",
  "珠江流域"          = "Pearl River Basin",
  "淮河流域"          = "Huaihe Basin",
  "海河流域"          = "Haihe Basin",
  "东南沿海诸河流域"  = "Southeast Coastal Rivers Basin"
)

# Load data
poc_data <- read_excel("2_inputData_watershed\\Input_features.xlsx") %>% as.data.frame() |>
  mutate(
    `POC_flux(Tg/year)` = `POC_flux(g/s)` * 3.15576e7 / 1e12,
    `POC_flux(Tg/year)` = round(`POC_flux(Tg/year)`, 6)
  ) %>%
  select(-`POC_flux(g/s)`) %>%
  relocate(`POC_flux(Tg/year)`, .after = last_col())

# Load basin boundaries and ensure validity
basins <- st_read("1_basin\\8basin.shp")
if (any(!st_is_valid(basins))) {
  basins <- st_make_valid(basins)
}

# Convert coordinates to spatial points
coords <- poc_data %>% select(Latitude, Longitude)
points_sf <- st_as_sf(coords, coords = c("Longitude", "Latitude"), crs = st_crs(basins))
matched_data <- st_join(points_sf, basins, join = st_within)
if (any(is.na(matched_data$NAME))) {
  unmatched_points <- matched_data %>% filter(is.na(NAME))
  nearest_features <- st_nearest_feature(unmatched_points, basins)
  matched_data$NAME[is.na(matched_data$NAME)] <- basins$NAME[nearest_features]
}
poc_data <- cbind(poc_data, Basin_Name = matched_data$NAME)

# Remove missing values and Standardize GDP
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

# Prepare features and target variables
X <- poc_data_clean %>% select(-ID, 
                               -Month, 
                               -`POC(mg/L)`, 
                               -`Water_discharge(m3/s)`, 
                               -`POC_flux(Tg/year)`, 
                               -Basin_Name)
y <- poc_data_clean$`POC(mg/L)`

# Model Selection & Cross-Validation ####
# # Cross-validation control
# cv_control <- trainControl(
#   method = "cv",           
#   number = 10,             
#   verboseIter = TRUE,      
#   search = "grid"          
# )
# 
# # Hyperparameter grid for SVM (Radial)
# svm_grid <- expand.grid(
#   sigma = 2^seq(-6, 0, by = 1),  
#   C     = 2^seq(0, 7, by = 1)
# )
# 
# # Train SVM model with 10-fold CV
# set.seed(123)
# svm_cv_model <- train(
#   x = as.matrix(X),
#   y = y,
#   method = "svmRadial",
#   trControl = cv_control,
#   tuneGrid = svm_grid
# )
# 
# # Print CV results
# print(svm_cv_model)
# 
# final_svm_model <- svm_cv_model$finalModel
# 
# ## check ####
# plot(svm_cv_model)

library(e1071)
library(caret)
set.seed(123)

folds <- createFolds(y, k = 10, list = TRUE, returnTrain = TRUE)

svm_models <- list()

for (i in seq_along(folds)) {
  cat("Training fold", i, "\n")
  
  fold_train_idx <- folds[[i]]
  X_fold <- X[fold_train_idx, ]
  y_fold <- y[fold_train_idx]
  
  svm_model <- svm(
    x = as.matrix(X_fold),
    y = y_fold,
    kernel = "radial",
    cost = 8,       
    gamma = 0.03125  
  )
  
  svm_models[[i]] <- svm_model
}

predict_ensemble <- function(models, newdata) {
  preds <- sapply(models, function(model) {
    predict(model, as.matrix(newdata))
  })
  rowMeans(preds)
}

poc_data_clean$Predicted_Concentration <- predict_ensemble(svm_models, X)
poc_data_clean$Predicted_Concentration  <- predict_ensemble(svm_models, X)

# Make Predictions ####
# Predict Cpoc using the best model
poc_data_clean$Predicted_Concentration <- kernlab::predict(final_svm_model, as.matrix(X))

# Unit conversion for Fpoc
conversion_factor <- 3.1536e-5
poc_data_clean$Predicted_Flux_Tg_year <- poc_data_clean$Predicted_Concentration *
  poc_data_clean$`Water_discharge(m3/s)` * conversion_factor
poc_data_clean$Observed_Flux_Tg_year <- poc_data_clean$`POC_flux(Tg/year)`


# Model Evaluation ####
## Total Evaluation Metrics ####
calc_metrics <- function(obs, pred) {
  fit <- lm(obs ~ pred)
  r2  <- summary(fit)$r.squared
  rmse <- sqrt(mean((obs - pred)^2, na.rm = TRUE))
  data.frame(R2 = r2, RMSE = rmse)
}

# Calculate metrics for concentration and flux
metrics_conc <- calc_metrics(poc_data_clean$`POC(mg/L)`, poc_data_clean$Predicted_Concentration)
metrics_flux <- calc_metrics(poc_data_clean$Observed_Flux_Tg_year, poc_data_clean$Predicted_Flux_Tg_year)

# Print metrics
print(metrics_conc)
print(metrics_flux)

## Evaluation by basin ####
# Prepare evaluation data with English basin names
evaluation_data_final <- poc_data_clean %>%
  mutate(
    Basin_EN = ifelse(Basin_Name %in% names(basin_name_mapping),
                      basin_name_mapping[Basin_Name],
                      Basin_Name)
  )

# Remove the Huaihe Basin if it is causing issues
evaluation_data_final <- evaluation_data_final %>%
  filter(Basin_EN != "Huaihe Basin")

# Add "All basins" category
all_data <- evaluation_data_final %>% mutate(Basin_EN = "All basins")
evaluation_data_final <- bind_rows(evaluation_data_final, all_data)

# Calculate metrics by basin for both concentration and flux
metrics_conc_basin <- evaluation_data_final %>%
  group_by(Basin_EN) %>%
  summarise(
    R2 = calc_metrics(`POC(mg/L)`, Predicted_Concentration)$R2,
    RMSE = calc_metrics(`POC(mg/L)`, Predicted_Concentration)$RMSE,
    .groups = "drop"
  )

metrics_flux_basin <- evaluation_data_final %>%
  group_by(Basin_EN) %>%
  summarise(
    R2 = calc_metrics(Observed_Flux_Tg_year, Predicted_Flux_Tg_year)$R2,
    RMSE = calc_metrics(Observed_Flux_Tg_year, Predicted_Flux_Tg_year)$RMSE,
    .groups = "drop"
  )


# Visualization ####
## plot ####
custom_theme <- theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    panel.grid = element_blank(),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(-0.15, "cm"),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold")
  )

## plotting function with proper axis handling ####
create_basin_plot <- function(data, basin_name, x_var, y_var, metrics, is_flux = FALSE) {
  plot_data <- data %>% filter(Basin_EN == basin_name)
  
  # Get metrics
  basin_metrics <- metrics %>% filter(Basin_EN == basin_name)
  txt_label <- paste0("R² = ", round(basin_metrics$R2, 2),
                      "\nRMSE = ", round(basin_metrics$RMSE, 2))
  
  # Dynamic axis limits
  axis_max <- max(plot_data[[x_var]], plot_data[[y_var]], na.rm = TRUE) * 1.05
  
  ggplot(plot_data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_point(color = "steelblue", size = 2, alpha = 0.7) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40") +
    annotate(
      "text", x = 0.05 * axis_max, y = 0.95 * axis_max,
      label = txt_label, hjust = 0, vjust = 1, size = 3.5
    ) +
    coord_fixed(ratio = 1, xlim = c(0, axis_max), ylim = c(0, axis_max)) +
    labs(x = NULL, y = NULL) +
    ggtitle(basin_name) +
    custom_theme
}

## Generate Plots ####
library(purrr)
# Common basin order
basin_order <- c(
  "All basins",
  "Northeast Rivers Basin",
  "Haihe Basin",
  "Yellow River Basin",
  "Yangtze River Basin",
  "Southeast Coastal Rivers Basin",
  "Pearl River Basin"
)

conc_plots <- map(basin_order, ~ create_basin_plot(
  evaluation_data_final, .x,
  "POC(mg/L)", "Predicted_Concentration", metrics_conc_basin
))

flux_plots <- map(basin_order, ~ create_basin_plot(
  evaluation_data_final, .x,
  "Observed_Flux_Tg_year", "Predicted_Flux_Tg_year", metrics_flux_basin, TRUE
))

## Arrange Plots with Proper Labeling ####
library(grid)
arrange_plots <- function(plot_list, title) {
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
  
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(2, 1, heights = unit(c(0.95, 0.05), "npc"))))
  
  # Main plot area
  pushViewport(viewport(layout.pos.row = 1))
  print(combined, newpage = FALSE)
  popViewport()
  
  # X-axis label
  pushViewport(viewport(layout.pos.row = 2))
  grid.text("Observed Values", x = 0.5, y = 0.5, gp = gpar(fontsize = 12))
  popViewport()
  
  # Y-axis label
  pushViewport(viewport(x = 0.05, y = 0.5, angle = 90))
  grid.text("Estimated Values", gp = gpar(fontsize = 12))
  popViewport()
}

arrange_plots(conc_plots, "POC Concentration")
arrange_plots(flux_plots, "POC Flux")


# Feature Importance ####
# Function to calculate permutation importance for SVM
calculate_svm_permutation_importance <- function(model, X, y, n_repeats = 10, metric = "RMSE") {
  # Get baseline predictions
  baseline_pred <- kernlab::predict(model, as.matrix(X))
  
  if (metric == "RMSE") {
    baseline_score <- sqrt(mean((y - baseline_pred)^2))
  } else if (metric == "R2") {
    baseline_score <- cor(baseline_pred, y)^2
  }
  
  importance_df <- data.frame(Feature = colnames(X), Importance = 0, stringsAsFactors = FALSE)
  
  for (feature in colnames(X)) {
    feature_scores <- numeric(n_repeats)
    
    for (i in 1:n_repeats) {
      # Create permuted version of the data
      X_permuted <- X
      X_permuted[[feature]] <- sample(X_permuted[[feature]])
      
      # Get predictions with permuted data
      permuted_pred <- kernlab::predict(model, as.matrix(X_permuted))
      
      # Calculate score
      if (metric == "RMSE") {
        feature_scores[i] <- sqrt(mean((y - permuted_pred)^2))
      } else if (metric == "R2") {
        feature_scores[i] <- cor(permuted_pred, y)^2
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

# Calculate permutation importance (using RMSE metric)
set.seed(123)  # For reproducibility
svm_perm_importance <- calculate_svm_permutation_importance(
  model = final_svm_model,
  X = X,
  y = y,
  n_repeats = 10,
  metric = "RMSE"
)

# Plot the importance
ggplot(svm_perm_importance, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(x = "Feature", y = "Permutation Importance (RMSE-based, % of max)", 
       title = "SVM Model Feature Importance by Permutation") +
  theme_minimal()

library(shapviz)
predict_function <- function(object, newdata) {
  predict(object, as.matrix(newdata))
}
shap_list <- list()
for (i in seq_along(svm_models)) {
  shap_i <- explain(
    object = svm_models[[i]],
    X = X,
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

sv_importance(sv_avg, kind = "bar", max_display = Inf) + 
  ggtitle("10-fold Averaged Mean Absolute SHAP (SVM)") +
  theme(plot.title = element_text(hjust = 0.5))

# Outlets Prediction ####
# Read input variables
new_outlet_data <- read_excel("E:/POC research/data/5_Prediction/input_features/Input_Features.xlsx") %>% as.data.frame()

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
  select(all_of(colnames(X))) %>%  # X 是你训练集的特征数据框
  mutate_all(as.numeric)

# Predict Cpoc
new_outlet_data$Predicted_Cpoc <- kernlab::predict(final_svm_model, as.matrix(new_X))

# Predict Fpoc
conversion_factor <- 3.1536e-5  # m3/s to Tg/year
new_outlet_data$Predicted_Fpoc_Tg_year <- new_outlet_data$Predicted_Cpoc *
  new_outlet_data$`Q (m3/s)` * conversion_factor

svm_result <- new_outlet_data %>%
  select(Gridcode, WatershedID, time, Year, Month, `Q (m3/s)`, Predicted_Cpoc, Predicted_Fpoc_Tg_year)

library(writexl)
write_xlsx(
  svm_result,
  "E:/POC research/data/5_Prediction/Predicted_Values/SVM.xlsx"
)