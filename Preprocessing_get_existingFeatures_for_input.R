# 加载必要的库
library(readxl)
library(dplyr)
library(lubridate)
library(writexl)

input <- read_excel("E:\\POC research\\data\\2_inputData_watershed\\input.xlsx")
monthly_records <- read_excel("E:\\POC research\\data\\3_POC records\\clean_records.xlsx")

# 在 input 表格中，将 year 和 month 合并为 year_month 列
input <- input %>%
  mutate(year_month = make_date(Sampling_year, Sampling_month, 1))  # 创建一个年月日期，日期为每月的第一天

# 在 monthly_records 表格中，将 Sampling_Time 转换为 year_month 格式
monthly_records <- monthly_records %>%
  mutate(year_month = floor_date(as.Date(Sampling_time, format = "%Y-%m-%d"), "month"))

# 通过 ID 和 year_month 进行合并，将 input 表格中的变量映射到 monthly_records 表格
merged_data <- monthly_records %>%
  left_join(input, by = c("ID", "year_month", "Longitude", "Latitude"))

# 选择所需的列：ID、输入特征和目标特征
final_data <- merged_data %>%
  select(
    ID,
    everything(),  # 保留输入特征
    `Water_discharge(m3/s).x`,  # 水流量
    `POC(mg/L).x`,  # POC 浓度
    `POC_flux(Tg/year).x`,  # POC 通量
    `SSC(mg/L)`  # SSC
  )

# 保存合并后的数据
write_xlsx(merged_data, "E:\\POC research\\data\\2_inputData_watershed\\input_1.xlsx")
