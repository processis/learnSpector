# 读取数据
data <- read.csv("/media/user/娱乐/learnTitanic/the big R/dashar/!desharnais123fillMedLog71TrainSet-loglength.csv")

# 计算Pearson相关性矩阵
cor_matrix <- cor(data, method = "pearson", use = "complete.obs")

# 打印相关性矩阵
print(cor_matrix)

# 选择特定变量
selected_vars <- data[, c("LogLength", "LogEffort", "LogPtsAjust","TeamExp","ManagerExp")]

# 计算相关性矩阵
cor_matrix <- cor(selected_vars, method = "pearson", use = "complete.obs")

# 打印相关性矩阵
print(cor_matrix)

# 可视化相关性矩阵（可选）
library(corrplot)
corrplot(cor_matrix, method = "circle")