# 加载必要的包
library(openxlsx)
library(dplyr)

# 读取SMU数据
smu_data <- read.xlsx("D:/POC research/data/2_HWSD2/HWSD2.xlsx", sheet = "HWSD2_SMU")

# 读取LAYERS数据
layers_data <- read.xlsx("D:/POC research/data/2_HWSD2/HWSD2.xlsx", sheet = "HWSD2_LAYERS")

# 根据HWSD2_SMU_ID分组，选择每组中SHARE最大且SEQUENCE=1的行
result <- layers_data %>%
  group_by(HWSD2_SMU_ID) %>%
  filter(SHARE == max(SHARE, na.rm = TRUE) & SEQUENCE == 1)

# 导出全部的数据
write.xlsx(result, "D:/POC research/data/2_HWSD2/HWSD2_LAYERS_Max.xlsx")  

# 分层导出
result_D1 <- result[result$LAYER == "D1",]
result_D2 <- result[result$LAYER == "D2",]
result_D3 <- result[result$LAYER == "D3",]
result_D4 <- result[result$LAYER == "D4",]
result_D5 <- result[result$LAYER == "D5",]
result_D6 <- result[result$LAYER == "D6",]
result_D7 <- result[result$LAYER == "D7",]

# 写入不同的sheet
sheets <- list("LAYERS_Max_All" = result,
               "D1" = result_D1, "D2" = result_D2,
               "D3" = result_D3, "D4" = result_D4,
               "D5" = result_D5, "D6" = result_D6,
               "D7" = result_D7)

# 保存最终结果到指定路径
write.xlsx(sheets, "D:/POC research/data/2_HWSD2/HWSD2_LAYERS_Max_7layers.xlsx")