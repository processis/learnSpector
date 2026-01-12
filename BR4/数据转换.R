# 加载必要的包
library(tidyverse)
library(readr)

# 方法1：如果数据已经是CSV格式
# 读取图1数据
library(data.table)
data1 <- fread("/home/user/Downloads/1.csv", encoding = "UTF-8")

# 查看数据结构
print("原始数据 (图1):")
print(data1)






# 转换数据：从宽格式变为长格式
data_tidy <- data1 %>%
  # 将数据转换为长格式（tidy data）
  pivot_longer(
    cols = -age,  # 排除age列
    names_to = "year",  # 列名变为年份
    values_to = "net"  # 值变为跑步数据
  ) %>%
  # 过滤掉缺失值
  filter(!is.na(net)) %>%
  # 创建跑者ID（假设每行是一个跑者）
  mutate(
    runner = as.integer(factor(age, levels = unique(age))),  # 根据age创建跑者ID
    year = as.integer(year),  # 确保year是整数
    age_at_year = as.integer(age)  # 确保age是整数
  ) %>%
  # 重新排列列
  select(runner, age = age_at_year, net, year) %>%
  # 按跑者和年份排序
  arrange(runner, year)

print("转换后的数据 (图2格式):")
print(data_tidy)

# 保存转换后的数据
write_csv(data_tidy, "running_data_tidy.csv")

# 方法2：如果数据有额外的列（如姓名）
# 假设图1有姓名列，格式为：
# name,age,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008
# 那么代码调整为：
if("name" %in% names(data1)) {
  data_tidy_with_name <- data1 %>%
    pivot_longer(
      cols = c(-name, -age),  # 排除name和age列
      names_to = "year",
      values_to = "net"
    ) %>%
    filter(!is.na(net)) %>%
    mutate(
      runner = as.integer(factor(name, levels = unique(name))),  # 使用姓名创建ID
      year = as.integer(year),
      age_at_year = as.integer(age)
    ) %>%
    select(runner, name, age = age_at_year, net, year) %>%
    arrange(runner, year)
  
  print("带姓名的转换后数据:")
  print(data_tidy_with_name)
  
  write_csv(data_tidy_with_name, "running_data_tidy_with_names.csv")
}

# 方法3：如果数据格式更复杂（如图片中的格式）
# 您可能需要先进行数据清洗
clean_and_convert <- function(data1) {
  # 移除空行
  data1 <- data1 %>% filter_all(any_vars(!is.na(.)))
  
  # 检查列名，确保它们是年份
  # 有时第一行可能包含元数据
  if(!any(grepl("^\\d{4}$", names(data1)))) {
    # 如果列名不是年份，可能需要使用第一行作为数据
    # 这里假设第二行开始是数据
    col_names <- as.character(data1[1, ])
    data1 <- data1[-1, ]
    names(data1) <- col_names
  }
  
  # 继续转换
  data_tidy <- data1 %>%
    pivot_longer(
      cols = -age,
      names_to = "year",
      values_to = "net"
    ) %>%
    filter(!is.na(net)) %>%
    mutate(
      runner = as.integer(factor(age, levels = unique(age))),
      year = as.integer(year),
      age = as.integer(age),
      net = as.numeric(net)
    ) %>%
    select(runner, age, net, year) %>%
    arrange(runner, year)
  
  return(data_tidy)
}

# 使用清洗函数
if(ncol(data1) > 10) {  # 如果有许多列，可能需要清洗
  cleaned_data <- clean_and_convert(data1)
  print("清洗并转换后的数据:")
  print(cleaned_data)
  
  write_csv(cleaned_data, "cleaned_running_data.csv")
}

# 可视化转换后的数据
if(nrow(data_tidy) > 0) {
  library(ggplot2)
  
  ggplot(data_tidy, aes(x = year, y = net, color = factor(runner))) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(
      title = "跑步成绩趋势",
      x = "年份",
      y = "成绩 (net)",
      color = "跑者"
    ) +
    theme_minimal() +
    scale_x_continuous(breaks = unique(data_tidy$year))
}

# 统计摘要
print("数据统计摘要:")
summary(data_tidy)