
#########deepseek example
# Sample long format data
df_long <- data.frame(
  student_id = c(1, 1, 1, 2, 2, 2),
  subject = c("math", "science", "english", "math", "science", "english"),
  score = c(90, 85, 88, 92, 87, 91),
  semester = c("Fall", "Fall", "Fall", "Spring", "Spring", "Spring")
)

# Convert to wide format
wide_df <- df_long %>%
  pivot_wider(
    id_cols = student_id,
    names_from = subject,
    values_from = score
  )

# Result:
#   student_id math science english
# 1          1   90      85      88
# 2          2   92      87      91


##################################

# 加载必要的包
library(tidyr)
library(dplyr)
library(readr)
# 假设数据已读入为data.frame，名为df
# 如果从文件读取，可以使用：
# df <- read.csv("your_data.csv", header = TRUE)
# 数据整理（基于您提供的图1格式）
# 注意：您的图1中列名有重复，需要先处理列名
# 假设数据已经正确读入，列名为: name_index, age, Year, Net
# 1. 创建唯一标识符：将name_index和起始年份组合
# 先计算每个name_index的起始年份（最小年份）

df<- read_csv("runner_realage.csv")

df <- df %>%
  group_by(name_index) %>%
  mutate(start_year = min(Year),
         start_age = age[which.min(Year)]) %>%
  ungroup()
# 2. 创建行标识符（如153(2002)格式）
df <- df %>%
  mutate(row_id = paste0(name_index, start_age, "(", start_year, ")"))
# 3. 转换为宽格式
wide_df <- df %>%
  pivot_wider(
    id_cols = row_id,
    names_from = Year,
    values_from = Net,
    names_sort = TRUE  # 按年份排序
  )
# 4. 重排列顺序（按起始年份降序，起始年龄降序）
wide_df <- wide_df %>%
  separate(row_id, into = c("name_age", "year_info"), sep = "\\(", remove = FALSE) %>%
  mutate(start_year = as.numeric(gsub("\\)", "", year_info)),
         name_num = as.numeric(substr(name_age, 1, 1)),
         age_num = as.numeric(substr(name_age, 2, nchar(name_age)))) %>%
  arrange(desc(start_year), desc(age_num)) %>%
  select(row_id, everything(), -name_age, -year_info, -start_year, -name_num, -age_num)
# 5. 添加年份列名（1999-2008）
# 确保所有年份列都存在
all_years <- as.character(1999:2008)
existing_years <- intersect(all_years, colnames(wide_df))
# 添加缺失的年份列
for(year in all_years) {
  if(!year %in% colnames(wide_df)) {
    wide_df[[year]] <- NA
  }
}
# 按正确顺序排列列
wide_df <- wide_df %>%
  select(row_id, all_of(all_years))
# 6. 重命名第一列
colnames(wide_df)[1] <- "name age(year)"
# 7. 导出为CSV文件
write_csv(wide_df, "formatted_data.csv", na = "")
# 8. 如果需要更接近图2的格式（空格分隔，无引号），可以使用：
write.table(wide_df, "formatted_data.txt", 
            sep = " ", 
            row.names = FALSE, 
            col.names = TRUE,
            quote = FALSE,
            na = "")
# 显示结果
print(wide_df)



