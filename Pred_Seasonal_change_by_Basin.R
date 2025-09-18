## Seasonal Change ####
library(dplyr)
library(ggplot2)
library(purrr)
library(lubridate)
library(patchwork)
library(readxl)
library(cowplot) 

# Purpose: Show monthly average Cpoc and Fpoc for each basin, using correct logic (sum for Fpoc, mean for Cpoc).
basin_monthly <- read_excel("E:/POC research/data/5_Prediction/Outlets/Fpoc_XGBoost_Predicted_Values.xlsx")

# 1. Calculate monthly average Cpoc and total Fpoc for each basin
monthly_avg <- basin_monthly %>%
  group_by(BasinName, Month) %>%
  summarise(
    Avg_Cpoc = mean(Avg_Cpoc, na.rm = TRUE),  # Mean Cpoc
    Fpoc = mean(Predicted_Fpoc_Tg_month, na.rm = TRUE), 
    .groups = "drop"
  )

# 2. National monthly average
monthly_all <- monthly_avg %>%
  group_by(Month) %>%
  summarise(
    Avg_Cpoc = mean(Avg_Cpoc, na.rm = TRUE),
    Fpoc = sum(Fpoc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(BasinName = "All")

monthly_avg <- bind_rows(monthly_avg, monthly_all)

# 3. Plotting function
make_month_plot <- function(basin_name, show_legend = FALSE) {
  df <- monthly_avg %>% filter(BasinName == basin_name)
  scale_factor <- max(df$Avg_Cpoc, na.rm = TRUE) / max(df$Fpoc, na.rm = TRUE)
  offset <- 0

  p <- ggplot(df, aes(x = Month)) +
    geom_line(aes(y = Fpoc, color = "Fpoc"), size = 1, show.legend = show_legend) +
    geom_point(aes(y = Fpoc, color = "Fpoc"), size = 2, show.legend = show_legend) +
    geom_line(aes(y = Avg_Cpoc / scale_factor + offset, color = "Cpoc"), size = 1, show.legend = show_legend) +
    geom_point(aes(y = Avg_Cpoc / scale_factor + offset, color = "Cpoc"), size = 2, show.legend = show_legend) +
    scale_y_continuous(
      name = NULL,
      sec.axis = sec_axis(~ . * scale_factor + offset, name = NULL)
    ) +
    scale_x_continuous(
      breaks = 1:12,
      labels = 1:12,
      limits = c(1, 12)
    ) +
    scale_color_manual(
      values = c("Fpoc" = "sienna", "Cpoc" = "steelblue"),
      name = NULL
    ) +
    labs(title = basin_name, x = "Month") +
    theme_minimal(base_size = 12) +
    theme(
      axis.title.y = element_blank(),
      axis.title.y.right = element_blank(),
      plot.margin = ggplot2::margin(t = 5, b = 5, l = 5, r = 5),
      legend.position = ifelse(show_legend, "top", "none")
    )
  return(p)
}

# 绘制所有图
plots <- map(basin_order, ~ make_month_plot(.x, show_legend = (.x == "All")))

# patchwork 合并布局
layout <- "
AAA
AAA
BCD
EFG
HIJ
"

combined <- wrap_plots(
  A = plots[[1]],
  B = plots[[2]], C = plots[[3]], D = plots[[4]],
  E = plots[[5]], F = plots[[6]], G = plots[[7]],
  H = plots[[8]], I = plots[[9]], J = plots[[10]],
  design = layout
)

# cowplot 添加左右全局纵坐标
left_lab  <- grid::textGrob(expression(F[POC]~"(Tg/month)"), rot = 90, gp = gpar(fontsize = 12))
right_lab <- grid::textGrob(expression(C[POC]~"(mg/L)"), rot = -90, gp = gpar(fontsize = 12))

final <- plot_grid(
  left_lab, combined, right_lab,
  ncol = 3,
  rel_widths = c(0.05, 0.9, 0.05)
)

print(final)
