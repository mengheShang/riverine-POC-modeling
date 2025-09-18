
## Normalized Annual Trend ####
library(dplyr)
library(ggplot2)
library(purrr)
library(patchwork)
library(lubridate)

# Purpose: Show normalized annual trends for Cpoc, Q, and Fpoc
# Use mean for Cpoc and Q, sum for Fpoc.

library(readxl)

# Purpose: Show monthly average Cpoc and Fpoc for each basin, using correct logic (sum for Fpoc, mean for Cpoc).
basin_result <- read_excel("E:/POC research/data/5_Prediction/Outlets/Fpoc_XGBoost_Predicted_Values.xlsx")

# Compute monthly flux per basin (Tg/month) with SE
basin_monthly <- basin_result %>%
  mutate(
    Date = as.Date(paste(Year, sprintf("%02d", Month), "01", sep = "-")),
    Days_in_month = days_in_month(Date),
    Sec_in_month  = Days_in_month * 24 * 3600,
    # Flux
    Fpoc_basin_month = Avg_Cpoc * Sum_Q * Sec_in_month / 1e12,   # Tg/month
    
  )

# 1) Aggregate to annual flux per basin (sum of months)
annual_data <- basin_monthly %>%
  group_by(BasinName, Year) %>%
  summarise(
    mean_Cpoc = mean(Avg_Cpoc, na.rm = TRUE),
    mean_Q    = mean(Sum_Q, na.rm = TRUE),
    sum_Fpoc  = sum(Fpoc_basin_month, na.rm = TRUE),  # Tg/year
    .groups = "drop"
  )

# 2) National annual flux (sum over basins)
annual_all <- annual_data %>%
  group_by(Year) %>%
  summarise(
    mean_Cpoc = mean(mean_Cpoc, na.rm = TRUE),
    mean_Q    = mean(mean_Q, na.rm = TRUE),
    sum_Fpoc  = sum(sum_Fpoc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(BasinName = "All")

# 3) Combine national and basin-level data
annual_data <- bind_rows(annual_all, annual_data)

# 4. long format for plotting
annual_long <- annual_data %>%
  pivot_longer(
    cols = c(mean_Cpoc, mean_Q, sum_Fpoc),
    names_to = "Metric",
    values_to = "value"
  ) %>%
  mutate(
    Metric = recode(Metric,
                    "mean_Cpoc" = "Cpoc",
                    "mean_Q"    = "Q",
                    "sum_Fpoc"  = "Fpoc"
    )
  ) %>%
  group_by(BasinName, Metric) %>%
  mutate(
    norm_value = value / max(value, na.rm = TRUE)
  ) %>%
  ungroup()

# 5. plotting function
make_plot <- function(basin_name, show_legend = FALSE) {
  df <- annual_long %>% filter(BasinName == basin_name)
  p <- ggplot(df, aes(x = Year, y = norm_value, color = Metric, fill = Metric)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black") +
    scale_color_manual(values = c("Q"="blue","Cpoc"="darkgreen","Fpoc"="red")) +
    scale_fill_manual(values = c("Q"="blue","Cpoc"="darkgreen","Fpoc"="red")) +
    labs(title = basin_name, x = NULL, y = NULL,
         subtitle = paste0("Trend slope: ",
                           paste(sapply(unique(df$Metric), function(m) {
                             slope <- coef(lm(norm_value ~ Year, data = df[df$Metric == m,]))[2]
                             paste(m, round(slope, 4))
                           }), collapse = ", ")
         )) +
    theme_minimal(base_size = 12)
  
  if (!show_legend) {
    p <- p + theme(legend.position = "none")
  }
  return(p)
}

# 6. Basin order
basin_order <- c("All",
                 "Songliao River","Haihe River","Yellow River",
                 "Huaihe River","Yangtze River","Southwest Rivers",
                 "Southeast Rivers","Pearl River","Hainan Island")

plots <- c(
  list(make_plot("All", show_legend = TRUE)),
  map(basin_order[-1], ~ make_plot(.x, show_legend = FALSE))
)

# 7. final arrangement function
library(patchwork)
library(grid)
library(cowplot)
library(purrr)

plots <- c(
  list(make_plot("All", show_legend = TRUE)),
  map(basin_order[-1], ~ make_plot(.x, show_legend = FALSE))
)

# 7. final arrangement function with left column for y-axis
arrange_basin_plots <- function(plot_list, ylab) {
  
  # Define patchwork layout
  layout <- "
  AAA
  AAA
  BCD
  EFG
  HIJ
  "
  
  # Combine individual plots according to layout
  combined <- wrap_plots(
    A = plot_list[[1]],
    B = plot_list[[2]], C = plot_list[[3]], D = plot_list[[4]],
    E = plot_list[[5]], F = plot_list[[6]], G = plot_list[[7]],
    H = plot_list[[8]], I = plot_list[[9]], J = plot_list[[10]],
    design = layout
  )
  
  # Create left y-axis label grob
  left_lab <- textGrob(ylab, rot = 90, gp = gpar(fontsize = 12))
  
  # Combine left label and plots using cowplot
  final <- plot_grid(
    left_lab, combined,  # left column for y-axis, main plots
    ncol = 2,
    rel_widths = c(0.05, 0.95)  # small column for y-axis, large for plots
  )
  
  # Print the final layout
  print(final)
}

arrange_basin_plots(plots, "Normalized values")