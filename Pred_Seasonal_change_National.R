## National Annual Change ####
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(patchwork)
library(grid)
library(lubridate)

basin_result <- read_excel("E:/POC research/data/5_Prediction/Outlets/Fpoc_XGBoost_Predicted_Values.xlsx")

# 1) Compute monthly flux per basin (Tg/month) with SE
basin_monthly <- basin_result %>%
  mutate(
    Date = as.Date(paste(Year, sprintf("%02d", Month), "01", sep = "-")),
    Days_in_month = days_in_month(Date),
    Sec_in_month  = Days_in_month * 24 * 3600,
    # Flux
    Fpoc_basin_month = Avg_Cpoc * Sum_Q * Sec_in_month / 1e12,   # Tg/month
    
  )

# 2) Aggregate to annual flux per basin (sum of months)
#    Variances add when summing independent months
basin_annual <- basin_monthly %>%
  group_by(BasinName, Year) %>%
  summarise(
    Fpoc_basin_year    = sum(Fpoc_basin_month, na.rm = TRUE),
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
