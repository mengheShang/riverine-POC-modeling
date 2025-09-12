
setwd("E:\\POC research\\data")

library(tidyr)
library(dplyr)
library(randomForest)
library(caret)
library(readxl)
library(ggplot2)
library(sf)
library(patchwork)
library(xgboost)

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
poc_data <- read_excel("2_inputData_watershed\\input_new.xlsx") %>% as.data.frame()

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

# Remove missing values
poc_data_clean <- poc_data %>% filter_all(all_vars(!is.na(.)))

# Prepare features and target
X <- poc_data_clean %>% select(-ID, 
                               -Sampling_month, 
                               -`POC(mg/L)`, 
                               -`Water_discharge(m3/s)`, 
                               -`POC_flux(Tg/year)`, 
                               -Basin_Name)
y <- poc_data_clean$`POC(mg/L)`

# Feature scaling (standardization)
X <- X %>% 
  mutate_all(as.numeric) %>% 
  mutate(GDP_std = (GDP_CNY10k - mean(GDP_CNY10k, na.rm = TRUE)) / sd(GDP_CNY10k, na.rm = TRUE)) %>%
  select(-GDP_CNY10k)

## Model Selection & Cross-Validation ####

# Cross-validation control
cv_control <- trainControl(
  method = "cv",          # Cross-validation
  number = 10,            # 10-fold
  verboseIter = TRUE,     # Show progress
  search = "grid"         # Grid search
)

# Hyperparameter grid for xgboost
grid <- expand.grid(
  nrounds = 100, 
  max_depth = c(4, 6, 8), 
  eta = c(0.01, 0.1, 0.2), 
  gamma = c(0, 1), 
  colsample_bytree = c(0.5, 0.7),
  min_child_weight = c(1, 2),
  subsample = c(0.7, 1)
)

# Train xgboost model with 10-fold CV
set.seed(123)
xgb_cv_model <- train(
  x = as.matrix(X),
  y = y,
  method = "xgbTree",
  trControl = cv_control,
  tuneGrid = grid
)

# Print CV results
print(xgb_cv_model)

# Extract best model
best_xgb_model <- xgb_cv_model$finalModel

## Make Predictions ####

# Predict Cpoc using the best model
poc_data_clean$Predicted_Concentration <- predict(best_xgb_model, as.matrix(X))

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


### plotting function with proper axis handling ####
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
    annotate("text", x = 0.05*axis_max, y = 0.95*axis_max,
             label = txt_label, hjust = 0, vjust = 1, size = 3.5) +
    coord_fixed(ratio = 1, xlim = c(0, axis_max), ylim = c(0, axis_max)) +
    labs(x = NULL, y = NULL) +  # Hide individual axis titles
    ggtitle(basin_name) +
    custom_theme
}

### Generate Plots ####
library(purrr)
# Common basin order
basin_order <- c("All basins",
                 "Northeast Rivers Basin",
                 "Haihe Basin",
                 "Yellow River Basin",
                 "Yangtze River Basin",
                 "Southeast Coastal Rivers Basin",
                 "Pearl River Basin")

# Generate plots for both concentration and flux
conc_plots <- map(basin_order, ~ create_basin_plot(
  evaluation_data_final, .x,
  "POC(mg/L)", "Predicted_Concentration", metrics_conc_basin
))

flux_plots <- map(basin_order, ~ create_basin_plot(
  evaluation_data_final, .x,
  "Observed_Flux_Tg_year", "Predicted_Flux_Tg_year", metrics_flux_basin, TRUE
))

### Arrange Plots with Proper Labeling ####
library(grid)
# Custom layout function
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
  
  # Add global labels using grid
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(2, 1, heights = unit(c(0.95, 0.05), "npc"))))
               
  # Main plots
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

# Generate final outputs
arrange_plots(conc_plots, "POC Concentration")
arrange_plots(flux_plots, "POC Flux")

## Variable Importance ####
var_imp <- xgb.importance(model = best_xgb_model)
ggplot(var_imp, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(x = NULL, y = "Importance (Gain)", title = "XGBoost Feature Importance") +
  custom_theme +
  theme(plot.title = element_text(hjust = 0.5))