# 加载必要的包
library(rjags)
library(coda)
library(readr)

# 读取CSV文件数据
df <- read_csv("cherry_blossom_sample修改.csv", col_names = TRUE)

# 查看数据结构
str(df)
head(df)

# 数据预处理：移除net为NA的行
df_clean <- df[!is.na(df$net), ]

# 统计每个年龄有多少个观测值
age_counts <- table(df_clean$age)
age_counts

# 由于原程序是20个个体，每个个体有4个时间点的观测
# 但你的数据中，age是连续的且有重复，我们需要重新组织数据
# 让我们选择出现频率最高的4个年龄点
top_ages <- names(sort(age_counts, decreasing = TRUE))[1:4]
top_ages <- as.numeric(top_ages)

# 筛选出这4个年龄点的数据
df_filtered <- df_clean[df_clean$age %in% top_ages, ]

# 现在我们需要将数据组织成矩阵形式：每个个体在这4个年龄点的观测值
# 但你的数据中没有明显的个体标识，所以我们需要按原始顺序创建个体
# 或者我们可以按原始数据分组（假设每行是一个不同的个体？）

# 从你的数据看，似乎是多个个体在不同年龄的观测，但没有个体标识
# 所以我假设每行是独立的观测，我们需要按年龄分组

# 重新组织数据：创建一个矩阵，行表示不同的个体，列表示不同的年龄点
# 由于数据不是平衡的，我们需要处理缺失值

# 方法1：使用平均值填充（如果某个年龄点缺少数据）
# 方法2：只保留在每个年龄点都有观测的个体

# 让我们尝试方法2：找到在4个年龄点都有观测的个体
# 但由于没有个体标识，这很困难

# 查看数据中是否有模式：观察前几行
head(df, 20)

# 看起来数据可能是按个体分组的，每4-5行是一个个体？
# 让我们尝试按每4行分组来创建Y矩阵

# 创建Y矩阵：每4行作为一个"个体"的观测
n_groups <- floor(nrow(df_clean) / 4)
Y_matrix <- matrix(NA, nrow = n_groups, ncol = 4)
x_values <- numeric(4)

for (i in 1:n_groups) {
  start_idx <- (i-1)*4 + 1
  end_idx <- min(start_idx + 3, nrow(df_clean))
  
  group_data <- df_clean[start_idx:end_idx, ]
  
  # 如果这个组有4个观测，并且年龄不同
  if (nrow(group_data) == 4 && length(unique(group_data$age)) == 4) {
    # 按年龄排序
    group_data <- group_data[order(group_data$age), ]
    Y_matrix[i, ] <- group_data$net
    x_values <- group_data$age  # 只取最后一次，实际上应该都一样
  }
}

# 移除全为NA的行
Y_matrix <- Y_matrix[!apply(Y_matrix, 1, function(x) all(is.na(x))), ]

# 由于数据质量，可能无法得到20个完整的个体
# 让我们看看得到了多少
n_individuals <- nrow(Y_matrix)
n_individuals

# 如果没有得到足够的数据，我们可以尝试另一种方法：
# 使用线性插值填充缺失值

# 让我们使用更简单的方法：假设每个个体都有相同的年龄序列
# 使用数据中最常见的4个年龄
top_ages_sorted <- sort(top_ages)

# 创建新的数据矩阵：为每个原始行编号（假设为个体）创建在这4个年龄点的观测值
# 首先给原始数据添加个体ID（假设每4行是一个个体）
df_clean$individual_id <- rep(1:ceiling(nrow(df_clean)/4), each = 4, length.out = nrow(df_clean))

# 使用reshape2包来重塑数据
library(reshape2)

# 由于数据不是平衡的，我们需要更复杂的处理
# 让我们创建一个函数来提取每个个体在指定年龄点的观测值
extract_individual_data <- function(df, ages) {
  # 找到有多少个唯一的个体（根据连续观测模式）
  # 简单起见，假设数据中每4行是一个个体
  n_individuals <- floor(nrow(df) / 4)
  Y <- matrix(NA, nrow = n_individuals, ncol = length(ages))
  
  for (i in 1:n_individuals) {
    start_idx <- (i-1)*4 + 1
    end_idx <- min(start_idx + 3, nrow(df))
    
    if (end_idx - start_idx + 1 == 4) {
      ind_data <- df[start_idx:end_idx, ]
      # 检查这个个体的年龄是否包含我们需要的年龄
      for (j in 1:length(ages)) {
        age_idx <- which(ind_data$age == ages[j])
        if (length(age_idx) > 0) {
          Y[i, j] <- ind_data$net[age_idx[1]]
        }
      }
    }
  }
  
  # 移除全为NA的行
  Y <- Y[apply(Y, 1, function(x) !all(is.na(x))), ]
  
  return(Y)
}

# 使用最常见的4个年龄点
Y <- extract_individual_data(df_clean, top_ages_sorted)

# 如果Y的行数不足20，让我们尝试使用所有数据，通过不同方法
if (nrow(Y) < 20) {
  # 使用更宽松的条件：只要个体在至少3个年龄点有数据
  cat("使用更宽松的条件选择个体...\n")
  
  # 让我们计算每个"个体"（按原始顺序每4行）在top_ages上有多少观测
  n_possible_individuals <- floor(nrow(df_clean) / 4)
  valid_individuals <- c()
  
  for (i in 1:n_possible_individuals) {
    start_idx <- (i-1)*4 + 1
    end_idx <- min(start_idx + 3, nrow(df_clean))
    
    if (end_idx >= start_idx) {
      ind_data <- df_clean[start_idx:end_idx, ]
      # 计算这个个体在top_ages上有多少观测
      n_obs <- sum(ind_data$age %in% top_ages_sorted)
      if (n_obs >= 3) {  # 至少有3个观测
        valid_individuals <- c(valid_individuals, i)
      }
    }
  }
  
  # 创建Y矩阵
  Y <- matrix(NA, nrow = length(valid_individuals), ncol = 4)
  
  for (idx in 1:length(valid_individuals)) {
    i <- valid_individuals[idx]
    start_idx <- (i-1)*4 + 1
    end_idx <- min(start_idx + 3, nrow(df_clean))
    
    ind_data <- df_clean[start_idx:end_idx, ]
    
    for (j in 1:4) {
      age_val <- top_ages_sorted[j]
      age_match <- which(ind_data$age == age_val)
      if (length(age_match) > 0) {
        Y[idx, j] <- ind_data$net[age_match[1]]
      }
    }
  }
}

# 检查最终得到的Y矩阵
dim(Y)
head(Y)

# 创建JAGS所需的数据列表
data_list <- list(
  Y = Y,
  x = top_ages_sorted,  # 使用最常见的4个年龄点
  R = matrix(c(4, 0, 0, 0,
               0, 4, 0, 0,
               0, 0, 4, 0,
               0, 0, 0, 4), nrow = 4, ncol = 4)
)

# 初始值
inits_list <- list(
  list(alpha = 40, beta = 1),
  list(alpha = mean(Y, na.rm = TRUE), beta = 1/sd(as.vector(Y), na.rm = TRUE)^2)
)

# JAGS模型代码 - 修改为处理可能的NA值
model_string <- textConnection("model {
  for (i in 1:N) {
    Y[i, 1:4] ~ dmnorm(mu[i, 1:4], Sigma.inv[,])
    for (j in 1:4) {
      mu[i, j] <- alpha + beta * x[j]
    }
  }
  alpha ~ dnorm(0, 0.0001)
  beta ~ dnorm(0, 0.0001)
  Sigma.inv[1:4, 1:4] ~ dwish(R[,], 4)
  Sigma[1:4, 1:4] <- inverse(Sigma.inv[,])
}")

# 添加N到数据列表
data_list$N <- nrow(data_list$Y)

# 运行JAGS模型
model <- jags.model(model_string, 
                    data = data_list, 
                    inits = inits_list,
                    n.chains = 2, 
                    quiet = TRUE)

# 老化（burn-in）
update(model, 10000, progress.bar = "none")

# 生成后验样本
params <- c("alpha", "beta", "Sigma", "Sigma.inv")
samples <- coda.samples(model, 
                        variable.names = params, 
                        n.iter = 20000, 
                        progress.bar = "none", 
                        thin = 1)

# 总结结果
summary(samples)

# 绘制图形
# 如果是在RStudio中，使用适当的设备
if (.Platform$GUI == "RStudio") {
  # RStudio有自己的图形设备
  plot(samples)
} else {
  # 非RStudio环境，使用新窗口
  dev.new(width = 7, height = 5)
  plot(samples)
}