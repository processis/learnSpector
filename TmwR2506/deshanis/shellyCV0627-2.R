#R tidymodel parsnip elasticnet machine learning R code example

# Load required packages
library(tidymodels)  # Includes parsnip, recipes, workflows, etc.
library(glmnet)      # Engine for elastic net
library(tidyverse)   # For data manipulation and visualization

# Set seed for reproducibility
set.seed(123)

data <- read_csv("/media/user/娱乐/learnSpector/TmwR2506/desharnais77CH-1.csv") 
#   %>%  mutate(logEffort = log(Effort))  # 对Effort取对数

data <- data %>% mutate(Effort = log10(Effort))
data <- data %>% mutate(PointsAjust = log10(PointsAjust))
data <- data %>% mutate(PointsNonAdjust = log10(PointsNonAdjust))



data$TeamExp<-as.numeric(data$TeamExp)
data$ManagerExp<-as.numeric(data$ManagerExp)


train_data <- subset(data,Project!=73)
train_data <- subset(train_data,Project!=66)
train_data <- subset(train_data,Project!=56)
train_data <- subset(train_data,Project!=41)
train_data <- subset(train_data,Project!=32)
train_data <- subset(train_data,Project!=22)
#tbDesharnais_train <- subset(tbDesharnais_train,Project!=32)
train_data <- subset(train_data,Project!=13)


test_data <- data[c(73,66,56,41,32,22,13), ]

# 2. 划分训练集和测试集（Project 1-7为测试集）
test_data <- data %>% filter(Project %in% 1:7)
train_data <- data %>% filter(!Project %in% 1:7)

# Create a recipe for preprocessing
# (Elastic net benefits from standardized predictors)
recipe <- recipe(Effort ~ ., data = train_data) %>%
  step_normalize(all_numeric_predictors())

# Specify the elastic net model
# mixture = 1 is lasso, mixture = 0 is ridge
# Here we'll tune both mixture and penalty
enet_model <- linear_reg(
  penalty = tune(),       # Regularization parameter (lambda)
  mixture = tune()        # Proportion of L1 vs L2 (alpha)
) %>% 
  set_engine("glmnet") %>% 
  set_mode("regression")

# Set up workflow
workflow <- workflow() %>% 
  add_recipe(recipe) %>% 
  add_model(enet_model)

# Create cross-validation folds for tuning
folds <- vfold_cv(train_data, v = 5)

# Set up tuning grid
tune_grid <- grid_regular(
  penalty(range = c(-5, 0)),  # 10^seq(-5, 0, length.out = 20)
  mixture(range = c(0, 1)),   # Between pure ridge and pure lasso
  levels = 10                 # Number of values for each parameter
)

# Tune the model
tune_results <- tune_grid(
  workflow,
  resamples = folds,
  grid = tune_grid,
  metrics = metric_set(rmse, rsq)
)

# Select the best model based on RMSE
best_model <- select_best(tune_results, metric = "rmse")

# Finalize the workflow with the best parameters
final_workflow <- workflow %>% 
  finalize_workflow(best_model)

# Fit the final model on the full training data
final_fit <- final_workflow %>% 
  fit(data = train_data)

# Evaluate on test data
test_results <- test_data %>% 
  bind_cols(predict(final_fit, new_data = test_data)) %>% 
  metrics(truth = Effort, estimate = .pred)

# Print test metrics
print(test_results)

# Variable importance plot (absolute coefficient values)
final_fit %>% 
  extract_fit_parsnip() %>% 
  vip::vi(lambda = best_model$penalty) %>% 
  mutate(Variable = fct_reorder(Variable, Importance)) %>% 
  ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
  geom_col() +
  ggtitle("Variable Importance from Elastic Net")

# You can also extract the final model coefficients
final_coefs <- final_fit %>% 
  extract_fit_parsnip() %>% 
  tidy()

print(final_coefs)

###################################################PCR

####R tidymodel parsniprincipal component regression PCR machine learning r code example

# Load required packages
library(tidymodels)  # Includes parsnip, recipes, workflows, etc.
library(pls)         # Engine for PCR
library(tidyverse)   # For data manipulation and visualization

data$TeamExp<-as.numeric(data$TeamExp)
data$ManagerExp<-as.numeric(data$ManagerExp)


train_data <- subset(data,Project!=73)
train_data <- subset(train_data,Project!=66)
train_data <- subset(train_data,Project!=56)
train_data <- subset(train_data,Project!=41)
train_data <- subset(train_data,Project!=32)
train_data <- subset(train_data,Project!=22)
#tbDesharnais_train <- subset(tbDesharnais_train,Project!=32)
train_data <- subset(train_data,Project!=13)


test_data <- data[c(73,66,56,41,32,22,13), ]

# 2. 划分训练集和测试集（Project 1-7为测试集）
test_data <- data %>% filter(Project %in% 1:7)
train_data <- data %>% filter(!Project %in% 1:7)

# Create a recipe for preprocessing
# PCR benefits from standardized predictors and PCA transformation
pcr_recipe <- recipe(Effort ~ ., data = train_data) %>%
  step_normalize(all_numeric_predictors()) %>%  # Center and scale
  step_pca(all_numeric_predictors(), num_comp = tune())  # Tune number of components

# Specify the linear regression model (PCR is linear regression on PCs)
pcr_model <- linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression")

# Set up workflow
pcr_workflow <- workflow() %>% 
  add_recipe(pcr_recipe) %>% 
  add_model(pcr_model)

# Create cross-validation folds for tuning
folds <- vfold_cv(train_data, v = 5)

# Set up tuning grid for number of components
# Use up to the maximum possible components (number of predictors)
max_comp <- ncol(train_data) - 1  # minus 1 for the outcome variable

tune_grid <- tibble(num_comp = seq(1, max_comp))

# Tune the model
tune_results <- tune_grid(
  pcr_workflow,
  resamples = folds,
  grid = tune_grid,
  metrics = metric_set(rmse, rsq)
)

# Select the best model based on RMSE
best_model <- select_best(tune_results, metric = "rmse")

# Finalize the workflow with the best number of components
final_workflow <- pcr_workflow %>% 
  finalize_workflow(best_model)

# Fit the final model on the full training data
final_fit <- final_workflow %>% 
  fit(data = train_data)

# Evaluate on test data
test_results <- test_data %>% 
  bind_cols(predict(final_fit, new_data = test_data)) %>% 
  metrics(truth = Effort, estimate = .pred)

# Print test metrics
print(test_results)

# Plot tuning results
autoplot(tune_results) +
  ggtitle("PCR Tuning Results")

# Extract PCA loadings to understand component composition
pca_loadings <- final_fit %>% 
  extract_recipe() %>% 
  tidy(number = 2)  # step_pca is the second step in our recipe

# View first few components
print(pca_loadings %>% filter(component %in% paste0("PC", 1:3)))

# Plot variance explained by each component
final_fit %>% 
  extract_recipe() %>% 
  tidy(number = 2, type = "variance") %>% 
  filter(terms == "percent variance") %>% 
  ggplot(aes(x = component, y = value)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(value, 1)), vjust = -0.5) +
  labs(title = "Percent Variance Explained by Each Principal Component",
       y = "Percent Variance Explained") +
  theme_minimal()

###############################################################cubist

library(tidymodels)  # Includes parsnip, recipes, workflows, etc.
library(Cubist)      # Engine for Cubist models
library(tidyverse)   # For data manipulation and visualization
library(rules)

# Set seed for reproducibility
set.seed(123)

data$TeamExp<-as.numeric(data$TeamExp)
data$ManagerExp<-as.numeric(data$ManagerExp)


train_data <- subset(data,Project!=73)
train_data <- subset(train_data,Project!=66)
train_data <- subset(train_data,Project!=56)
train_data <- subset(train_data,Project!=41)
train_data <- subset(train_data,Project!=32)
train_data <- subset(train_data,Project!=22)
#tbDesharnais_train <- subset(tbDesharnais_train,Project!=32)
train_data <- subset(train_data,Project!=13)


test_data <- data[c(73,66,56,41,32,22,13), ]

# 2. 划分训练集和测试集（Project 1-7为测试集）
test_data <- data %>% filter(Project %in% 1:7)
train_data <- data %>% filter(!Project %in% 1:7)

# Create a recipe for preprocessing
# Cubist generally doesn't require extensive preprocessing
cubist_recipe <- recipe(Effort ~ ., data = train_data)

# Specify the Cubist model with tuning parameters
cubist_model <- cubist_rules(
  committees = tune(),  # Number of sequential models (1-100)
  neighbors = tune()    # Number of nearest neighbors (0-9)
) %>% 
  set_engine("Cubist") %>% 
  set_mode("regression")

# Set up workflow
cubist_workflow <- workflow() %>% 
  add_recipe(cubist_recipe) %>% 
  add_model(cubist_model)

# Create cross-validation folds for tuning
folds <- vfold_cv(train_data, v = 5)

# Set up tuning grid
tune_grid <- grid_regular(
  committees(range = c(1, 20)),  # Try 1 to 20 committees
  neighbors(range = c(0, 9)),    # Try 0 to 9 neighbors
  levels = 5                     # Number of values for each parameter
)

# Tune the model
tune_results <- tune_grid(
  cubist_workflow,
  resamples = folds,
  grid = tune_grid,
  metrics = metric_set(rmse, rsq)
)

# Select the best model based on RMSE
best_model <- select_best(tune_results, metric = "rmse")

# Finalize the workflow with the best parameters
final_workflow <- cubist_workflow %>% 
  finalize_workflow(best_model)

# Fit the final model on the full training data
final_fit <- final_workflow %>% 
  fit(data = train_data)

# Evaluate on test data
test_results <- test_data %>% 
  bind_cols(predict(final_fit, new_data = test_data)) %>% 
  metrics(truth = Effort, estimate = .pred)

# Print test metrics
print(test_results)

# Plot tuning results
autoplot(tune_results) +
  ggtitle("Cubist Tuning Results")

# Extract and view the final model rules
final_model <- extract_fit_engine(final_fit)
summary(final_model)

# Variable importance
final_fit %>% 
  extract_fit_parsnip() %>% 
  vip::vi() %>% 
  mutate(Variable = fct_reorder(Variable, Importance)) %>% 
  ggplot(aes(x = Importance, y = Variable)) +
  geom_col(fill = "steelblue") +
  ggtitle("Cubist Variable Importance")


######################################################random forest
library(tidymodels)  # Includes parsnip, recipes, workflows, etc.
library(ranger)      # Engine for random forest
library(tidyverse)   # For data manipulation and visualization
library(vip)         # For variable importance plots
library(DALEX)       # For model explanations (optional)

set.seed(123)

data$TeamExp<-as.numeric(data$TeamExp)
data$ManagerExp<-as.numeric(data$ManagerExp)


train_data <- subset(data,Project!=73)
train_data <- subset(train_data,Project!=66)
train_data <- subset(train_data,Project!=56)
train_data <- subset(train_data,Project!=41)
train_data <- subset(train_data,Project!=32)
train_data <- subset(train_data,Project!=22)
#tbDesharnais_train <- subset(tbDesharnais_train,Project!=32)
train_data <- subset(train_data,Project!=13)


test_data <- data[c(73,66,56,41,32,22,13), ]

# 2. 划分训练集和测试集（Project 1-7为测试集）
test_data <- data %>% filter(Project %in% 1:7)
train_data <- data %>% filter(!Project %in% 1:7)

# Create a recipe for preprocessing
# Random forests don't typically need extensive preprocessing
rf_recipe <- recipe(logEffort ~ ., data = train_data)

# Specify the Random Forest model with tuning parameters
rf_model <- rand_forest(
  mtry = tune(),       # Number of variables to sample at each split
  trees = tune(),      # Number of trees in the forest
  min_n = tune()       # Minimum node size
) %>% 
  set_engine("ranger", importance = "permutation") %>%  # permutation importance
  set_mode("regression")

# Set up workflow
rf_workflow <- workflow() %>% 
  add_recipe(rf_recipe) %>% 
  add_model(rf_model)

# Create cross-validation folds for tuning
folds <- vfold_cv(train_data, v = 5)

# Set up tuning grid
tune_grid <- grid_regular(
  mtry(range = c(2, ncol(train_data) - 1)),  # Typically 1/3 to all predictors
  trees(range = c(200, 1000)),                # Number of trees
  min_n(range = c(2, 10)),                   # Minimum node size
  levels = 4                                 # Number of values for each parameter
)

# Tune the model
tune_results <- tune_grid(
  rf_workflow,
  resamples = folds,
  grid = tune_grid,
  metrics = metric_set(rmse, rsq),
  control = control_grid(verbose = TRUE)
)

# Select the best model based on RMSE
best_model <- select_best(tune_results, metric = "rmse")

# Finalize the workflow with the best parameters
final_workflow <- rf_workflow %>% 
  finalize_workflow(best_model)

# Fit the final model on the full training data
final_fit <- final_workflow %>% 
  fit(data = train_data)

# Evaluate on test data
test_results <- test_data %>% 
  bind_cols(predict(final_fit, new_data = test_data)) %>% 
  metrics(truth = logEffort, estimate = .pred)

# Print test metrics
print(test_results)

# Plot tuning results
autoplot(tune_results) +
  ggtitle("Random Forest Tuning Results")

# Variable importance plot
vip(final_fit) +
  ggtitle("Random Forest Variable Importance")


##############################################SVM

# Load required packages
library(tidymodels)  # Includes parsnip, recipes, workflows, etc.
library(kernlab)     # Engine for SVM
library(tidyverse)   # For data manipulation and visualization

# Convert numeric columns if needed (assuming this was in your original data prep)
data$TeamExp <- as.numeric(data$TeamExp)
data$ManagerExp <- as.numeric(data$ManagerExp)

# Split data into training and test sets (using your project exclusion approach)
train_data <- subset(data, !Project %in% c(73, 66, 56, 41, 32, 22, 13))
test_data <- data %>% filter(Project %in% c(73, 66, 56, 41, 32, 22, 13))

# Alternatively, using your second approach (commented out here)
 test_data <- data %>% filter(Project %in% 1:7)
 train_data <- data %>% filter(!Project %in% 1:7)

# Create a recipe for preprocessing
# SVM benefits from standardized predictors
svm_recipe <- recipe(Effort ~ ., data = train_data) %>%
  step_normalize(all_numeric_predictors())  # Center and scale numeric predictors
# Note: SVM doesn't need PCA like PCR did

# Specify the SVM model with tunable parameters
svm_model <- svm_rbf(
  cost = tune(),       # Regularization parameter
  rbf_sigma = tune()   # Kernel parameter
) %>% 
  set_engine("kernlab") %>% 
  set_mode("regression")

# Set up workflow
svm_workflow <- workflow() %>% 
  add_recipe(svm_recipe) %>% 
  add_model(svm_model)

# Create cross-validation folds for tuning
folds <- vfold_cv(train_data, v = 5)

# Set up tuning grid for SVM parameters
tune_grid <- grid_regular(
  cost(),          # Tests various cost values
  rbf_sigma(),     # Tests various sigma values
  levels = 5       # Number of values to try for each parameter
)

# Tune the model
tune_results <- tune_grid(
  svm_workflow,
  resamples = folds,
  grid = tune_grid,
  metrics = metric_set(rmse, rsq)
)

# Select the best model based on RMSE
best_model <- select_best(tune_results, metric = "rmse")

# Finalize the workflow with the best parameters
final_workflow <- svm_workflow %>% 
  finalize_workflow(best_model)

# Fit the final model on the full training data
final_fit <- final_workflow %>% 
  fit(data = train_data)

# Evaluate on test data
test_results <- test_data %>% 
  bind_cols(predict(final_fit, new_data = test_data)) %>% 
  metrics(truth = Effort, estimate = .pred)

# Print test metrics
print(test_results)

# Plot tuning results
autoplot(tune_results) +
  ggtitle("SVM Tuning Results")

# If you want to examine the final model details
final_model_details <- extract_fit_engine(final_fit)
print(final_model_details)

# Variable importance plot (not as straightforward as for linear models)
# This requires additional steps for SVM
