# 读取数据（假设数据已保存为CSV文件）
# 注意：原始数据中的空单元格应保存为NA


# 方法2：如果从CSV文件读取
data <- read.csv("/home/user/Downloads/1.csv", stringsAsFactors = FALSE)

 library(tidyr)
 library(dplyr)
 # 读取数据

# 1. 创建示例数据函数（模拟图一的数据格式）
create_example_data <- function() {
  # 创建图一格式的数据
  data_fig1 <- data.frame(
    age_year = c("53(2002)", "52(2001)", "51(2001)", "2003", "2004", "2005", "2006", "2007", "2008"),
    X1999 = c(NA, NA, NA, 74.3, 75.15, 74.21667, 74.25, NA, NA),
    X2000 = c(NA, NA, NA, 88.11667, 80.93333, 88.1, NA, NA, NA),
    X2001 = c(83.98333, 82.66667, 89.38333, 89.71667, 88.68333, 97.55, 85.05, NA, NA),
    X2002 = c(NA, 80.03333, 89.23333, NA, NA, NA, NA, NA, NA),
    stringsAsFactors = FALSE
  )
  return(data_fig1)
}
# 2. 数据转换函数
convert_to_long_format <- function(data) {
  # 初始化结果数据框
  result <- data.frame(
    runner = integer(),
    age = integer(),
    net = numeric(),
    year = integer(),
    stringsAsFactors = FALSE
  )
  
  # 提取跑步者信息（前三行）
  runners_info <- data[1:3, ]
  
  # 为每个跑步者处理数据
  for (runner_id in 1:nrow(runners_info)) {
    runner_row <- runners_info[runner_id, ]
    
    # 解析年龄和基准年份
    age_year_str <- runner_row$age_year
    base_info <- strsplit(age_year_str, "\\(")[[1]]
    base_age <- as.numeric(base_info[1])
    base_year <- as.numeric(gsub("\\)", "", base_info[2]))
    
    # 处理1999-2002年的数据
    for (year in 1999:2002) {
      col_name <- paste0("X", year)
      net_value <- runner_row[[col_name]]
      
      if (!is.na(net_value)) {
        # 计算该年的年龄
        age <- base_age + (year - base_year)
        
        result <- rbind(result, data.frame(
          runner = runner_id,
          age = age,
          net = net_value,
          year = year
        ))
      }
    }
    
    # 处理2003-2008年的数据（从第4行开始）
    for (row_idx in 4:nrow(data)) {
      year_row <- data[row_idx, ]
      year_str <- year_row$age_year
      
      if (grepl("^\\d{4}$", year_str)) {
        year <- as.numeric(year_str)
        
        # 获取该跑步者在当前年份的数据
        # 根据示例数据，2003年及以后的数据在X1999、X2000、X2001列
        # 跑步者1在X1999列，跑步者2在X2000列，跑步者3在X2001列
        if (runner_id == 1) {
          net_value <- year_row$X1999
        } else if (runner_id == 2) {
          net_value <- year_row$X2000
        } else if (runner_id == 3) {
          net_value <- year_row$X2001
        }
        
        if (!is.na(net_value)) {
          # 计算年龄
          age <- base_age + (year - base_year)
          
          result <- rbind(result, data.frame(
            runner = runner_id,
            age = age,
            net = net_value,
            year = year
          ))
        }
      }
    }
  }
  
  # 按runner和year排序
  result <- result[order(result$runner, result$year), ]
  
  # 重置行名
  rownames(result) <- NULL
  
  return(result)
}
# 3. 主程序：执行转换并保存结果
main <- function() {
  # 创建示例数据（您可以用自己的数据替换这部分）
  data_fig1 <- create_example_data()
  
  # 查看原始数据
  print("原始数据（图一格式）:")
  print(data_fig1)
  
  # 转换数据
  long_format_data <- convert_to_long_format(data_fig1)
  
  # 查看转换结果
  print("转换后的数据（图二格式）:")
  print(long_format_data)
  
  # 保存为CSV文件
  write.csv(long_format_data, "converted_data.csv", row.names = FALSE)
  cat("\n数据已保存为 'converted_data.csv'\n")
  
  return(long_format_data)
}
# 4. 运行主程序
result_data <- main()
# 5. 如果要从CSV文件读取数据，可以使用以下代码
read_and_convert_data <- function(file_path) {
  # 读取CSV文件
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # 如果CSV文件有图一格式，直接转换
  # 注意：您可能需要根据实际CSV文件的结构调整列名
  # 假设CSV文件的第一列名为"age.year."（对应"age(year)"）
  # 其他列名为"X1999"、"X2000"等
  
  # 重命名列以匹配我们的函数期望的格式
  colnames(data)[1] <- "age_year"
  
  # 转换数据
  long_format_data <- convert_to_long_format(data)
  
  return(long_format_data)
}
# 6. 使用示例（如果要读取实际CSV文件）
# 假设您的CSV文件名为"input_data.csv"
# converted_data <- read_and_convert_data("input_data.csv")
# write.csv(converted_data, "converted_data.csv", row.names = FALSE)
# 7. 打印使用说明
cat("
使用说明：
1. 如果您有实际的CSV文件，请确保它有以下列：
   - 第一列：age(year)格式，如'53(2002)'
   - 其他列：年份数据，列名为'X1999'、'X2000'等
2. 使用以下代码读取和转换您的数据：
   data <- read.csv('您的文件路径.csv', stringsAsFactors = FALSE)
   colnames(data)[1] <- 'age_year'
   result <- convert_to_long_format(data)
   write.csv(result, 'converted_data.csv', row.names = FALSE)
3. 如果您的数据列名不同，请调整convert_to_long_format函数中的列名。
")

 