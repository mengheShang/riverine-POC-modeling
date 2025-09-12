## Seasonal Change ####
library(dplyr)
library(ggplot2)
library(purrr)
library(patchwork)

# Purpose: Show monthly average Cpoc and Fpoc for each basin, using correct logic (sum for Fpoc, mean for Cpoc).

# 1. Calculate monthly average Cpoc and total Fpoc for each basin
monthly_avg <- basin_monthly %>%
  group_by(BasinName, Month) %>%
  summarise(
    Avg_Cpoc = mean(Avg_Cpoc, na.rm = TRUE),  # Mean Cpoc
    Sum_Fpoc = sum(Fpoc_basin_month, na.rm = TRUE),  # Total Fpoc
    .groups = "drop"
  )

# 2. National monthly average
monthly_all <- monthly_avg %>%
  group_by(Month) %>%
  summarise(
    Avg_Cpoc = mean(Avg_Cpoc, na.rm = TRUE),
    Sum_Fpoc = sum(Sum_Fpoc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(BasinName = "All")

monthly_avg <- bind_rows(monthly_avg, monthly_all)

# 3. Plotting function
make_month_plot <- function(basin_name) {
  df <- monthly_avg %>% filter(BasinName == basin_name)
  scale_factor <- max(df$Avg_Cpoc, na.rm = TRUE) / max(df$Sum_Fpoc, na.rm = TRUE)
  offset <- 0  # Adjust if needed
  
  ggplot(df, aes(x = Month)) +
    geom_line(aes(y = Sum_Fpoc), color = "sienna", size = 1) +
    geom_point(aes(y = Sum_Fpoc), color = "sienna", size = 2) +
    geom_smooth(aes(y = Sum_Fpoc), method = "lm", se = FALSE, linetype = "dashed", color = "black") +
    geom_line(aes(y = Avg_Cpoc / scale_factor + offset), color = "steelblue", size = 1) +
    geom_point(aes(y = Avg_Cpoc / scale_factor + offset), color = "steelblue", size = 2) +
    geom_smooth(aes(y = Avg_Cpoc / scale_factor + offset), method = "lm", se = FALSE, linetype = "dashed", color = "black") +
    scale_y_continuous(
      name = expression(F[POC]~"(Tg/month)"),
      sec.axis = sec_axis(~ . * scale_factor + offset, name = expression(C[POC]~"(mg/L)"))
    ) +
    labs(title = basin_name, x = "Month",
         subtitle = paste0(
           "Fpoc trend slope: ", round(coef(lm(Sum_Fpoc ~ Month, data = df))[2], 4),
           ", Cpoc trend slope: ", round(coef(lm(Avg_Cpoc ~ Month, data = df))[2], 4)
         )) +
    theme_minimal(base_size = 12)
}

# 4. Combine overall plot + subplots
basin_order <- c("All",
                 "Songliao River","Haihe River","Yellow River",
                 "Huaihe River","Yangtze River","Southwest Rivers",
                 "Southeast Rivers","Pearl River","Hainan Island")

plots <- map(basin_order, make_month_plot)

# Layout for combined plots
layout <- "
AAA
AAA
BCD
EFG
HIJ
"
wrap_plots(
  A = plots[[1]],
  B = plots[[2]], C = plots[[3]], D = plots[[4]],
  E = plots[[5]], F = plots[[6]], G = plots[[7]],
  H = plots[[8]], I = plots[[9]], J = plots[[10]],
  design = layout
)
