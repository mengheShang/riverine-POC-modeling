## National Annual Change ####
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(patchwork)
library(grid)
library(lubridate)

basin_monthly <- read_excel("E:/POC research/data/5_Prediction/Outlets/Fpoc_XGBoost_Predicted_Values.xlsx")

# 2) Aggregate to annual flux per basin (sum of months)
#    Variances add when summing independent months
basin_annual <- basin_monthly %>%
  group_by(BasinName, Year) %>%
  summarise(
    Fpoc_basin_year = sum(Predicted_Fpoc_Tg_month, na.rm = TRUE),
    .groups = "drop"
  )

# 3) Aggregate to national annual flux (sum of basins)
national_annual <- basin_annual %>%
  group_by(Year) %>%
  summarise(
    Fpoc_all    = sum(Fpoc_basin_year, na.rm = TRUE),
    .groups = "drop"
  )

# 4) Plot with uncertainty ribbon
ggplot(national_annual, aes(x = Year, y = Fpoc_all)) +
  geom_line(color = "red", linewidth = 1) +
  labs(title = "Time series of national POC flux",
       subtitle = paste0("Trend slope: ", round(coef(lm(Fpoc_all ~ Year, data = national_annual))[2], 4)),
       x = "Year",
       y = "Fpoc (Tg/year)") +
  theme_minimal(base_size = 14)
