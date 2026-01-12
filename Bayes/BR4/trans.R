# 1. 加载必要的包
library(tidyr)
library(dplyr)
library(readr)
# 2. 读取数据
wide_data <- read.csv("/home/user/Downloads/cherry_blossom_sample变形2.csv", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
# 查看列名
colnames(wide_data)
# 3. 重命名列（第一列是跑步者ID，第二列是初始年龄和年份）
colnames(wide_data)[1] <- "runner"
colnames(wide_data)[2] <- "age_year_info"
# 4. 提取初始年份和年龄
# 格式如：53(2002) 表示在2002年时年龄为53岁
wide_data <- wide_data %>%
  mutate(
    # 提取括号内的年份
    initial_year = as.numeric(gsub(".*\\((\\d+)\\).*", "\\1", age_year_info)),
    # 提取年龄（括号前的数字）
    initial_age = as.numeric(gsub("\\(.*", "", age_year_info))
  )
# 5. 转换为长格式
# 保留跑步者ID和年份列（1999-2008）
long_data <- wide_data %>%
  select(runner, initial_year, initial_age, `1999`:`2008`) %>%
  pivot_longer(
    cols = `1999`:`2008`,
    names_to = "year",
    values_to = "net"
  ) %>%
  mutate(
    year = as.numeric(year),
    # 计算每个年份的年龄：年龄 = 初始年龄 + (当前年份 - 初始年份)
    age = initial_age + (year - initial_year)
  ) %>%
  # 只保留有net值的行（去除NA）
  filter(!is.na(net)) %>%
  # 选择并重排列
  select(runner, age, net, year) %>%
  # 按跑步者和年份排序
  arrange(runner, year)
# 6. 查看转换后的数据
head(long_data, 20)
# 7. 保存为CSV文件
write_csv(long_data, "cherry_blossom_sample_converted.csv")
# 8. 也可以保存为与原格式完全相同的格式
# 如果需要与原文件完全相同的runner编号（从1开始），可以添加编号
long_data_numbered <- long_data %>%
  mutate(runner = as.numeric(factor(runner, levels = unique(wide_data$runner))))
# 查看编号后的数据
head(long_data_numbered, 20)
# 保存编号版本
write_csv(long_data_numbered, "cherry_blossom_sample_converted_numbered.csv")
# 9. 验证数据
cat("转换后的数据行数:", nrow(long_data), "\n")
cat("转换后的跑步者数量:", length(unique(long_data$runner)), "\n")
cat("年份范围:", range(long_data$year), "\n")
cat("年龄范围:", range(long_data$age), "\n")
