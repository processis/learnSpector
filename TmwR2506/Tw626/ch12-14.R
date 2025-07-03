#####ch12

library(tidymodels)
library(patchwork)
library(ggforce)
library(doMC)
registerDoMC(cores = parallel::detectCores())

tidymodels_prefer()

source("ames_snippets.R")

data(two_class_dat)

set.seed(91)
split <- initial_split(two_class_dat)

training_set <- training(split)
testing_set  <-  testing(split)

data_grid <- crossing(A = seq(0.4, 4, length = 200), B = seq(.14, 3.9, length = 200))

load("search_examples.RData")

ggplot(training_set, aes(x = A, y = B, color = Class, pch = Class)) + 
  geom_point(alpha = 0.7) + 
  coord_equal()  + 
  labs(x = "Predictor A", y = "Predictor B", color = NULL, pch = NULL) +
  scale_color_manual(values = c("#CC6677", "#88CCEE"))

library(tidymodels)
tidymodels_prefer()

llhood <- function(...) {
  logistic_reg() %>% 
    set_engine("glm", ...) %>% 
    fit(Class ~ ., data = training_set) %>% 
    glance() %>% 
    select(logLik)
}

bind_rows(
  llhood(),
  llhood(family = binomial(link = "probit")),
  llhood(family = binomial(link = "cloglog"))
) %>% 
  mutate(link = c("logit", "probit", "c-log-log"))  %>% 
  arrange(desc(logLik))

set.seed(1201)
rs <- vfold_cv(training_set, repeats = 10)

lloss <- function(...) {
  perf_meas <- metric_set(roc_auc, mn_log_loss)
  
  logistic_reg() %>% 
    set_engine("glm", ...) %>% 
    fit_resamples(Class ~ A + B, rs, metrics = perf_meas) %>% 
    collect_metrics(summarize = FALSE) %>%
    select(id, id2, .metric, .estimate)
}

resampled_res <- 
  bind_rows(
    lloss()                                    %>% mutate(model = "logistic"),
    lloss(family = binomial(link = "probit"))  %>% mutate(model = "probit"),
    lloss(family = binomial(link = "cloglog")) %>% mutate(model = "c-log-log")     
  ) %>%
  # Convert log-loss to log-likelihood:
  mutate(.estimate = ifelse(.metric == "mn_log_loss", -.estimate, .estimate)) %>% 
  group_by(model, .metric) %>% 
  summarize(
    mean = mean(.estimate, na.rm = TRUE),
    std_err = sd(.estimate, na.rm = TRUE) / sqrt(n()), 
    .groups = "drop"
  )

resampled_res %>% 
  filter(.metric == "mn_log_loss") %>% 
  ggplot(aes(x = mean, y = model)) + 
  geom_point() + 
  geom_errorbar(aes(xmin = mean - 1.64 * std_err, xmax = mean + 1.64 * std_err),
                width = .1) + 
  labs(y = NULL, x = "log-likelihood")

resampled_res %>% 
  filter(.metric == "roc_auc") %>% 
  ggplot(aes(x = mean, y = model)) + 
  geom_point() + 
  geom_errorbar(aes(xmin = mean - 1.64 * std_err, xmax = mean+ 1.64 * std_err),
                width = .1) + 
  labs(y = NULL, x = "area under the ROC curve")

logit_pred <- 
  logistic_reg() %>% 
  set_engine("glm") %>% 
  fit(Class ~ A + B, data = training_set) %>% 
  predict(data_grid, type = "prob") %>% 
  bind_cols(data_grid) %>% 
  mutate(link = "logit")

probit_pred <- 
  logistic_reg() %>% 
  set_engine("glm", family = binomial(link = "probit")) %>% 
  fit(Class ~ A + B, data = training_set) %>% 
  predict(data_grid, type = "prob") %>% 
  bind_cols(data_grid) %>% 
  mutate(link = "probit")

cloglog_pred <- 
  logistic_reg() %>% 
  set_engine("glm", family = binomial(link = "cloglog")) %>% 
  fit(Class ~ A + B, data = training_set) %>% 
  predict(data_grid, type = "prob") %>% 
  bind_cols(data_grid) %>% 
  mutate(link = "c-log-log")

link_grids <- 
  bind_rows(logit_pred, probit_pred, cloglog_pred) %>% 
  mutate(link = factor(link, levels = c("logit", "probit", "c-log-log")))

link_grids %>% 
  ggplot(aes(x = A, y = B)) + 
  geom_point(data = testing_set, aes(color = Class, pch = Class), 
             alpha = 0.7, show.legend = FALSE) + 
  geom_contour(aes( z = .pred_Class1, lty = link), breaks = 0.5, color = "black") + 
  scale_color_manual(values = c("#CC6677", "#88CCEE")) + 
  coord_equal() + 
  labs(x = "Predictor A", y = "Predictor B")



two_class_rec <-
  recipe(Class ~ ., data = two_class_dat) %>% 
  step_normalize(all_numeric_predictors()) 

mlp_mod <- 
  mlp(hidden_units = tune(), epochs = 1000) %>% 
  set_engine("nnet") %>%
  set_mode("classification")

mlp_wflow <- 
  workflow() %>% 
  add_recipe(two_class_rec) %>% 
  add_model(mlp_mod)

mlp_res <-
  tibble(
    hidden_units = 1:20,
    train = NA_real_,
    test = NA_real_,
    model = vector(mode = "list", length = 20)
  )

for(i in 1:nrow(mlp_res)) {
  set.seed(27)
  tmp_mod <-
    mlp_wflow %>% finalize_workflow(mlp_res %>% slice(i) %>% select(hidden_units)) %>%
    fit(training_set)
  mlp_res$train[i] <-
    roc_auc_vec(training_set$Class, predict(tmp_mod, training_set, type = "prob")$.pred_Class1)
  mlp_res$test[i]  <-
    roc_auc_vec(testing_set$Class, predict(tmp_mod, testing_set, type = "prob")$.pred_Class1)
  mlp_res$model[[i]] <- tmp_mod
}




te_plot <- 
  mlp_res %>% 
  slice(c(1, 4, 20)) %>% 
  mutate(
    probs = map(model, ~ bind_cols(data_grid, predict(.x, data_grid, type = "prob")))
  ) %>% 
  dplyr::select(hidden_units, probs) %>% 
  unnest(cols = c(probs)) %>% 
  mutate(
    label = paste(format(hidden_units), "units"),
    label = ifelse(label == " 1 units", " 1 unit", label)
  ) %>% 
  ggplot(aes(x = A, y = B)) + 
  geom_point(data = testing_set, aes(color = Class, pch = Class), 
             alpha = 0.5, show.legend = FALSE) + 
  geom_contour(aes( z = .pred_Class1), breaks = 0.5, color = "black") + 
  scale_color_manual(values = c("#CC6677", "#88CCEE")) + 
  facet_wrap(~ label, nrow = 1) + 
  coord_equal() + 
  ggtitle("Test Set") + 
  labs(x = "Predictor A", y = "Predictor B")

tr_plot <- 
  mlp_res %>% 
  slice(c(1, 4, 20)) %>% 
  mutate(
    probs = map(model, ~ bind_cols(data_grid, predict(.x, data_grid, type = "prob")))
  ) %>% 
  dplyr::select(hidden_units, probs) %>% 
  unnest(cols = c(probs)) %>% 
  mutate(
    label = paste(format(hidden_units), "units"),
    label = ifelse(label == " 1 units", " 1 unit", label)
  ) %>% 
  ggplot(aes(x = A, y = B)) +
  geom_point(data = training_set, aes(color = Class, pch = Class), 
             alpha = 0.5, show.legend = FALSE) + 
  geom_contour(aes( z = .pred_Class1), breaks = 0.5, color = "black") + 
  scale_color_manual(values = c("#CC6677", "#88CCEE")) + 
  facet_wrap(~ label, nrow = 1) + 
  coord_equal() + 
  ggtitle("Training Set") + 
  labs(x = "Predictor A", y = "Predictor B")

tr_plot / te_plot


grid_plot <-
  ggplot(sfd_grid, aes(x = x, y = y)) +
  geom_point() +
  lims(x = 0:1, y = 0:1) +
  labs(x = "Parameter 1", y = "Parameter 2", title = "Space-Filling Grid") +
  geom_contour(data = grid_contours,
               aes(z = obj),
               alpha = .3,
               bins = 12) +
  coord_equal() +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

search_plot <-
  ggplot(nm_res, aes(x = x, y = y)) +
  geom_point(size = .7)  +
  lims(x = 0:1, y = 0:1) +
  labs(x = "Parameter 1", y = "Parameter 2", title = "Global Search") +
  coord_equal()  +
  geom_contour(data = grid_contours,
               aes(x = x, y = y, z = obj),
               alpha = .3,
               bins = 12) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

grid_plot + search_plot



rand_forest(trees = 2000, min_n = 10) %>%                   # <- main arguments
  set_engine("ranger", regularization.factor = 0.5)   

neural_net_spec <- 
  mlp(hidden_units = tune()) %>%
  set_mode("regression") %>%
  set_engine("keras")

tune()

ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train)  %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = tune()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Longitude, deg_free = tune("longitude df")) %>% 
  step_ns(Latitude,  deg_free = tune("latitude df"))

recipes_param <- extract_parameter_set_dials(ames_rec)
recipes_param

wflow_param <- 
  workflow() %>% 
  add_recipe(ames_rec) %>% 
  add_model(neural_net_spec) %>% 
  extract_parameter_set_dials()
wflow_param


hidden_units()
threshold()

extract_parameter_set_dials(ames_rec) %>% 
  update(threshold = threshold(c(0.8, 1.0)))

rf_spec <- 
  rand_forest(mtry = tune()) %>% 
  set_engine("ranger", regularization.factor = tune("regularization")) %>%
  set_mode("regression")

rf_param <- extract_parameter_set_dials(rf_spec)
rf_param

rf_param %>% 
  update(mtry = mtry(c(1, 70)))


pca_rec <- 
  recipe(Sale_Price ~ ., data = ames_train) %>% 
  # Select the square-footage predictors and extract their PCA components:
  step_normalize(contains("SF")) %>% 
  # Select the number of components needed to capture 95% of
  # the variance in the predictors. 
  step_pca(contains("SF"), threshold = .95)

updated_param <- 
  workflow() %>% 
  add_model(rf_spec) %>% 
  add_recipe(pca_rec) %>% 
  extract_parameter_set_dials() %>% 
  finalize(ames_train)
updated_param
updated_param %>% extract_parameter_dials("mtry")

rf_param
regularization_factor()

penalty(c(-1, 0)) %>% value_sample(1000) %>% summary()

penalty(c(0.1, 1.0)) %>% value_sample(1000) %>% summary()

penalty(trans = NULL, range = 10^c(-10, 0))

############################################ch13

library(tidymodels)
library(finetune)
library(ggforce)
library(stringr)
library(av)
library(lme4)
data(cells)
theme_set(theme_bw())

library(doMC)
registerDoMC(cores = parallel::detectCores(logical = TRUE))

library(kableExtra)

tidymodels_prefer()

## -----------------------------------------------------------------------------

load("mlp_times.RData")

library(tidymodels)
tidymodels_prefer()

mlp_spec <- 
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>% 
  set_engine("nnet", trace = 0) %>% 
  set_mode("classification")
  
mlp_param <- extract_parameter_set_dials(mlp_spec)
mlp_param %>% extract_parameter_dials("hidden_units")
mlp_param %>% extract_parameter_dials("penalty")
mlp_param %>% extract_parameter_dials("epochs")

crossing(
  hidden_units = 1:3,
  penalty = c(0.0, 0.1),
  epochs = c(100, 200)
)

grid_regular(mlp_param, levels = 2)

mlp_param %>% 
  grid_regular(levels = c(hidden_units = 3, penalty = 2, epochs = 2))

set.seed(1301)
mlp_param %>% 
  grid_random(size = 1000) %>% # 'size' is the number of combinations
  summary()

library(ggforce)
set.seed(1302)
mlp_param %>% 
  # The 'original = FALSE' option keeps penalty in log10 units
  grid_random(size = 20, original = FALSE) %>% 
  ggplot(aes(x = .panel_x, y = .panel_y)) + 
  geom_point() +
  geom_blank() +
  facet_matrix(vars(hidden_units, penalty, epochs), layer.diag = 2) + 
  labs(title = "Random design with 20 candidates")

set.seed(1303)
mlp_param %>% 
  grid_latin_hypercube(size = 20, original = FALSE) %>% 
  ggplot(aes(x = .panel_x, y = .panel_y)) + 
  geom_point() +
  geom_blank() +
  facet_matrix(vars(hidden_units, penalty, epochs), layer.diag = 2) + 
  labs(title = "Latin Hypercube design with 20 candidates")

library(tidymodels)
data(cells)
cells <- cells %>% select(-case)

set.seed(1304)
cell_folds <- vfold_cv(cells)

mlp_rec <-
  recipe(class ~ ., data = cells) %>%
  step_YeoJohnson(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_pca(all_numeric_predictors(), num_comp = tune()) %>% 
  step_normalize(all_numeric_predictors())

mlp_wflow <- 
  workflow() %>% 
  add_model(mlp_spec) %>% 
  add_recipe(mlp_rec)

mlp_param <- 
  mlp_wflow %>% 
  extract_parameter_set_dials() %>% 
  update(
    epochs = epochs(c(50, 200)),
    num_comp = num_comp(c(0, 40))
  )

roc_res <- metric_set(roc_auc)
set.seed(1305)
mlp_reg_tune <-
  mlp_wflow %>%
  tune_grid(
    cell_folds,
    grid = mlp_param %>% grid_regular(levels = 3),
    metrics = roc_res
  )
mlp_reg_tune

autoplot(mlp_reg_tune) + 
  scale_color_viridis_d(direction = -1) + 
  theme(legend.position = "top")

show_best(mlp_reg_tune) %>% select(-.estimator)

set.seed(1306)
mlp_sfd_tune <-
  mlp_wflow %>%
  tune_grid(
    cell_folds,
    grid = 20,
    # Pass in the parameter object to use the appropriate range: 
    param_info = mlp_param,
    metrics = roc_res
  )
mlp_sfd_tune

autoplot(mlp_sfd_tune)

show_best(mlp_sfd_tune) %>% select(-.estimator)

select_best(mlp_reg_tune, metric = "roc_auc")

logistic_param <- 
  tibble(
    num_comp = 0,
    epochs = 125,
    hidden_units = 1,
    penalty = 1
  )

final_mlp_wflow <- 
  mlp_wflow %>% 
  finalize_workflow(logistic_param)
final_mlp_wflow

final_mlp_fit <- 
  final_mlp_wflow %>% 
  fit(cells)

library(usemodels)

use_xgboost(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
              Latitude + Longitude, 
            data = ames_train,
            # Add comments explaining some of the code:
            verbose = TRUE)

xgboost_recipe <- 
  recipe(formula = Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>% 
  step_novel(all_nominal_predictors()) %>% 
  ## This model requires the predictors to be numeric. The most common 
  ## method to convert qualitative predictors to numeric is to create 
  ## binary indicator variables (aka dummy variables) from these 
  ## predictors. However, for this model, binary indicator variables can be 
  ## made for each of the levels of the factors (known as 'one-hot 
  ## encoding'). 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  step_zv(all_predictors()) 

xgboost_spec <- 
  boost_tree(trees = tune(), min_n = tune(), tree_depth = tune(), learn_rate = tune(), 
             loss_reduction = tune(), sample_size = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("xgboost") 

xgboost_workflow <- 
  workflow() %>% 
  add_recipe(xgboost_recipe) %>% 
  add_model(xgboost_spec) 

set.seed(69305)
xgboost_tune <-
  tune_grid(xgboost_workflow, 
            resamples = stop("add your rsample object"), 
            grid = stop("add number of candidate points"))

c5_spec <- 
  boost_tree(trees = tune()) %>% 
  set_engine("C5.0") %>% 
  set_mode("classification")

set.seed(1307)
c5_spec %>%
  tune_grid(
    class ~ .,
    resamples = cell_folds,
    grid = data.frame(trees = 1:100),
    metrics = roc_res
  )

for (rs in resamples) {
  # Create analysis and assessment sets
  # Preprocess data (e.g. formula or recipe)
  for (mod in configurations) {
    # Fit model {mod} to the {rs} analysis set
    # Predict the {rs} assessment set
  }
}

load("resamples_times.RData")

resamples_times %>%
  dplyr::rename(operation = label) %>% 
  ggplot(aes(y = id_alt, x = duration, fill = operation)) +
  geom_bar(stat = "identity", color = "black") +
  labs(y = NULL, x = "Elapsed Time") + 
  scale_fill_brewer(palette = "Paired") +
  theme(legend.position = "top")
  set_mode("classification")
  
  all_tasks <- crossing(resamples, configurations)
  
  for (iter in all_tasks) {                           
    # Create analysis and assessment sets for {iter}
    # Preprocess data (e.g. formula or recipe)
    # Fit model {iter} to the {iter} analysis set
    # Predict the {iter} assessment set
  }
  
  load("everything_times.RData")
  
  repeats <- 
    everything_times %>% 
    dplyr::filter(id == "Fold1" & event == "preproc") %>% 
    nrow()
  
  everything_times <- 
    everything_times %>%
    dplyr::rename(operation = label) %>% 
    mutate(operation = as.character(operation)) %>% 
    arrange(pid, id, operation) %>% 
    select(pid, id, operation, duration)
  
  start_stop <- 
    everything_times %>%
    pivot_wider(
      id_cols = c(pid, id),
      names_from = "operation",
      values_from = "duration"
    ) %>% 
    group_by(pid) %>% 
    mutate(
      total = model + preprocess,
      .stop_mod = cumsum(total),
      prev = dplyr::lag(total, 1),
      prev = ifelse(is.na(prev), 0, prev),
      .start_pre = cumsum(prev),
      .stop_pre = .start_pre + preprocess,
      .start_mod = .stop_pre
    ) %>% 
    ungroup()  %>% 
    select(pid, id, .start_pre, .stop_pre, .start_mod, .stop_mod) 
  
  starts <- 
    start_stop %>% 
    select(pid, id, contains("start")) %>% 
    pivot_longer(
      cols = c(.start_pre, .start_mod),
      values_to = ".start"
    ) %>% 
    mutate(
      operation = ifelse(grepl("mod$", name), "model", "preprocess")
    ) %>% 
    select(-name)
  
  stops <- 
    start_stop %>% 
    select(pid, id, contains("stop")) %>% 
    pivot_longer(
      cols = c(.stop_pre, .stop_mod),
      values_to = ".stop"
    ) %>% 
    mutate(
      operation = ifelse(grepl("mod$", name), "model", "preprocess")
    ) %>% 
    select(-name)
  
  id_offset <- 0.4
  start_stop_dat <- 
    full_join(starts, stops, by = c("pid", "id", "operation")) %>% 
    mutate(
      id_num = as.numeric(factor(id)),
      id_start = id_num - id_offset,
      id_stop  = id_num + id_offset
    )
  
  start_stop_dat %>% 
    ggplot(aes(y = id_num, x = .start)) +
    geom_rect(
      aes(
        xmin = .start,
        xmax = .stop,
        ymin = id_start,
        ymax = id_stop,
        fill = operation
      ),
      color = "black"
    ) +
    facet_wrap(~ pid, nrow = 2) +
    labs(y = NULL, x = "Elapsed Time") + 
    scale_fill_brewer(palette = "Paired") +
    scale_y_continuous(breaks = 1:5, labels = paste("Fold", 1:5)) +
    theme_bw() +
    theme(legend.position = "top", panel.grid.minor = element_blank())
  
  load("xgb_times.RData")
  ggplot(times, aes(x = num_cores, y = elapsed, color = parallel_over, shape = parallel_over)) + 
    geom_point(size = 2) + 
    geom_line() +
    facet_wrap(~ preprocessing) + 
    labs(x = "Number of Workers", y = "Execution Time (s)") + 
    scale_y_log10() + 
    scale_color_manual(values = c("#7FC97F", "#386CB0")) +
    theme_bw() + 
    theme(legend.position = "top")
  
  
  ggplot(times, aes(x = num_cores, y = speed_up, color = parallel_over, shape = parallel_over)) + 
    geom_abline(lty = 1) + 
    geom_point(size = 2) + 
    geom_line() +
    facet_wrap(~ preprocessing) + 
    coord_obs_pred() + 
    scale_color_manual(values = c("#7FC97F", "#386CB0")) +
    labs(x = "Number of Workers", y = "Speed-up")  +
    theme(legend.position = "top")
  
  coef_penalty <- 0.1
  spec <- linear_reg(penalty = coef_penalty) %>% set_engine("glmnet")
  spec
  
  spec$args$penalty
  
  spec <- linear_reg(penalty = !!coef_penalty) %>% set_engine("glmnet")
  spec$args$penalty
  
  mcmc_args <- list(chains = 3, iter = 1000, cores = 3)
  
  linear_reg() %>% set_engine("stan", !!!mcmc_args)
  
  library(stringr)
  ch_2_vars <- str_subset(names(cells), "ch_2")
  ch_2_vars
  
  recipe(class ~ ., data = cells) %>% 
    step_spatialsign(all_of(ch_2_vars))
  
  
  recipe(class ~ ., data = cells) %>% 
    step_spatialsign(!!!ch_2_vars)
  
  set.seed(1308)
  mlp_sfd_race <-
    mlp_wflow %>%
    tune_race_anova(
      cell_folds,
      grid = 20,
      # Pass in the parameter object to use the appropriate range: 
      param_info = mlp_param,
      metrics = roc_res
    )
  remaining <-
    mlp_sfd_race %>% 
    collect_metrics() %>% 
    dplyr::filter(n == 10)
  
  
  
  full_att <- attributes(mlp_sfd_race)
  
  race_details <- NULL
  for(iter in 1:10) {
    
    tmp <- mlp_sfd_race %>% filter(.order <= iter)
    
    tmp_att <- full_att
    tmp_att$row.names <- attr(tmp, "row.names")
    attributes(tmp) <- tmp_att
    
    if (nrow(show_best(tmp)) == 1) {
      break()
    }
    race_details <-
      bind_rows(
        race_details,
        finetune:::test_parameters_gls(tmp) %>% mutate(iter = iter))
  }
  
  race_details <-
    race_details %>%
    mutate(
      lower = ifelse(iter < 3, NA, lower),
      upper = ifelse(iter < 3, NA, upper),
      pass = ifelse(iter < 3, TRUE, pass),
      decision = ifelse(pass, "retain", "discard"),
      decision = ifelse(pass & estimate == 0, "best", decision)
    )  %>%
    mutate(
      .config = factor(.config),
      .config = reorder(.config, estimate),
      decision = factor(decision, levels = c("best", "retain", "discard"))
    ) 
  race_cols <- c(best = "blue", retain = "black", discard = "grey")
  
  iter_three <- race_details %>% dplyr::filter(iter == 3) 
  
  iter_three %>% 
    ggplot(aes(x = -estimate, y = .config)) + 
    geom_vline(xintercept = 0, lty = 2, color = "green") +
    geom_point(size = 2, aes(color = decision)) +
    geom_errorbarh(aes(xmin = -estimate, xmax = -upper, color = decision), height = .3, show.legend = FALSE) + 
    labs(x = "Loss of ROC AUC", y = NULL) + 
    scale_colour_manual(values = race_cols)
  
  
  race_ci_plots <- function(x, iters = max(x$iter)) {
    
    x_rng <- extendrange(c(-x$estimate, -x$upper))
    
    for (i in 1:iters) {
      if (i < 3) {
        ttl <- paste0("Iteration ", i, ": burn-in")
      } else {
        ttl <- paste0("Iteration ", i, ": testing")
      }
      p <-
        x %>% 
        dplyr::filter(iter == i) %>% 
        ggplot(aes(x = -estimate, y = .config, color = decision)) +
        geom_vline(xintercept = 0, color = "green", lty = 2) +
        geom_point(size = 2) +
        labs(title = ttl, y = "", x = "Loss of ROC AUC") +
        scale_color_manual(values = c(best = "blue", retain = "black", discard = "grey"), 
                           drop = FALSE) +
        scale_y_discrete(drop = FALSE) +
        xlim(x_rng) + 
        theme_bw() +
        theme(legend.position = "top")
      
      if (i >= 3) {
        p <- p  + geom_errorbar(aes(xmin = -estimate, xmax = -upper), width = .3)
      }
      
      print(p)
    }
    invisible(NULL)
  }
  av_capture_graphics(
    race_ci_plots(race_details),
    output = "race_results.mp4",
    width = 720,
    height = 720,
    res = 120,
    framerate = 1/3
  )
  
  library(finetune)
  
  set.seed(1308)
  mlp_sfd_race <-
    mlp_wflow %>%
    tune_race_anova(
      cell_folds,
      grid = 20,
      param_info = mlp_param,
      metrics = roc_res,
      control = control_race(verbose_elim = TRUE)
    )
  
  
  show_best(mlp_sfd_race, n = 10)
  
  library(tidymodels)
  
  data(cells)
  cells <- cells %>% select(-case)
  
  set.seed(1304)
  cell_folds <- vfold_cv(cells)
  
  roc_res <- metric_set(roc_auc)
  
  
  #####################################14
  
  library(tidymodels)
  library(finetune)
  library(patchwork)
  library(kableExtra)
  library(av)
  library(doMC)
  registerDoMC(cores = parallel::detectCores(logical = TRUE))
  tidymodels_prefer()
  
  
  source("extras/verify_results.R")
  source("extras/sa_2d_plot.R")
  source("extras/bo_3panel_plot.R")
  load(file.path("RData", "svm_large.RData"))
  
  data(cells)
  cells <- cells %>% select(-case)
  set.seed(1304)
  cell_folds <- vfold_cv(cells)
  roc_res <- metric_set(roc_auc)
  
  library(tidymodels)
  tidymodels_prefer()
  
  svm_rec <- 
    recipe(class ~ ., data = cells) %>%
    step_YeoJohnson(all_numeric_predictors()) %>%
    step_normalize(all_numeric_predictors())
  
  svm_spec <- 
    svm_rbf(cost = tune(), rbf_sigma = tune()) %>% 
    set_engine("kernlab") %>% 
    set_mode("classification")
  
  svm_wflow <- 
    workflow() %>% 
    add_model(svm_spec) %>% 
    add_recipe(svm_rec)
  
  cost()
  rbf_sigma()
  
  svm_param <- 
    svm_wflow %>% 
    extract_parameter_set_dials() %>% 
    update(rbf_sigma = rbf_sigma(c(-7, -1)))
  
  set.seed(1401)
  start_grid <- 
    svm_param %>% 
    update(
      cost = cost(c(-6, 1)),
      rbf_sigma = rbf_sigma(c(-6, -4))
    ) %>% 
    grid_regular(levels = 2)
  
  set.seed(1402)
  svm_initial <- 
    svm_wflow %>% 
    tune_grid(resamples = cell_folds, grid = start_grid, metrics = roc_res)
  
  collect_metrics(svm_initial)
  
  collect_metrics(svm_initial) %>% 
    select(ROC = mean, cost, rbf_sigma) %>% 
    as.data.frame() %>% 
    format(digits = 4, scientific = FALSE) %>% 
    kable(
      caption = "Resampling statistics used as the initial substrate to the Gaussian process model.",
      label = "initial-gp-data"
    ) %>% 
    kableExtra::kable_styling(full_width = FALSE) %>% 
    kableExtra::add_header_above(c("outcome" = 1, "predictors" = 2))
  
  best_val <- max(collect_metrics(svm_initial)$mean)
  tmp <- tibble(candidate = LETTERS[1:2], .mean = c(.90, .89), .sd = c(0.02, 0.005))
  tmp %>% 
    select(candidate, mean = .mean, variance = .sd) %>% 
    mutate(variance = variance^2) %>% 
    as.data.frame() %>% 
    format(digits = 4, scientific = FALSE) %>% 
    kable(
      caption = "Two example tuning parameters considered for further sampling.",
      label = "tuning-candidates"
    ) %>% 
    kableExtra::kable_styling(full_width = FALSE) %>% 
    kableExtra::add_header_above(c(" " = 1, "GP Prediction of ROC AUC" = 2))
  
  source("extras/nonlinear_function.R")
  grid <- 
    tibble(x = seq(0, 1, length.out = 200)) %>% 
    mutate(y = purrr::map_dbl(x, nonlin_function, error = FALSE))
  
  set.seed(121)
  current_iter <- tibble(x = c(.09, .41,  .55, .7, .8)) %>% 
    mutate(y = purrr::map_dbl(x, nonlin_function))
  
  gp <- GPfit::GP_fit(matrix(current_iter$x, ncol = 1), current_iter$y)
  
  gp_pred <- 
    predict(gp, matrix(grid$x, ncol = 1))$complete_data %>% 
    as_tibble() %>% 
    setNames(c("x", ".mean", ".sd"))  %>% 
    mutate(.sd = sqrt(.sd))
  
  gp_pred <- 
    gp_pred %>% 
    bind_cols(
      exp_improve() %>% 
        predict(gp_pred, maximize = TRUE, iter = 1, best = max(current_iter$y)) %>% 
        setNames("exp_imp")
    ) %>% 
    bind_cols(
      conf_bound(kappa = .1) %>% 
        predict(gp_pred, maximize = TRUE, iter = 1, best = max(current_iter$y)) %>% 
        setNames("conf_int_01")
    ) %>% 
    bind_cols(
      conf_bound(kappa = 1) %>% 
        predict(gp_pred, maximize = TRUE, iter = 1, best = max(current_iter$y)) %>% 
        setNames("conf_int_1")
    )
  
  y_lab <- expression(Estimated ~ italic(R^2))
  
  ggplot(grid, aes(x = x, y = y)) + 
    geom_line(color = "red", alpha = .5, linewidth = 1.25) + 
    labs(y = y_lab, x = "Tuning Parameter") +
    geom_point(data = current_iter)
  
  y_lab <- expression(Estimated ~ italic(R^2))
  
  gp_pred %>% 
    ggplot(aes(x = x)) + 
    geom_point(data = current_iter, aes(y = y)) + 
    geom_line(aes(y = .mean)) +
    geom_vline(xintercept = c(0.1, 0.25), lty = 2, alpha = .5) + 
    geom_ribbon(aes(ymin = .mean -  1 * .sd, ymax = .mean + 1 * .sd), alpha = .1) + 
    labs(y = y_lab, x = "Tuning Parameter")
  
  small_pred <- 
    predict(gp, c(0.1, 0.25))$complete_data %>% 
    as_tibble() %>% 
    setNames(c("x", ".mean", ".sd")) %>% 
    mutate(
      value = c(0.1, 0.25),
      .sd = sqrt(.sd),
      max = .mean + 3 * .sd,
      min = .mean - 3 * .sd
    )
  
  small_pred <- 
    small_pred %>% 
    bind_cols(
      exp_improve() %>% 
        predict(small_pred, maximize = TRUE, iter = 1, best = max(current_iter$y)) %>% 
        setNames("exp_imp")
    )
  
  get_density <- function(dat) {
    res <- tibble(x = seq(dat$min, dat$max, length.out = 200)) %>% 
      mutate(density = dnorm(x, dat$.mean, dat$.sd),
             `Parameter Value` = format(dat$value))
    res
  }
  
  x_lab <- expression(Predicted ~ italic(R^2) ~ Distribution)
  
  small_pred %>% 
    group_by(value) %>% 
    do(get_density(.)) %>% 
    ungroup() %>% 
    ggplot(aes(x = x, y = density, color = `Parameter Value`, lty = `Parameter Value`)) + 
    geom_line() +
    geom_vline(xintercept = max(current_iter$y), lty = 3) +  
    labs(x = x_lab) + 
    scale_color_brewer(palette = "Set1")
  
  small_pred %>% 
    select(`Parameter Value` = x, Mean = .mean, `Std Dev` = .sd, `Expected Improvment` = exp_imp) %>% 
    as.data.frame() %>% 
    format(digits = 4, scientific = FALSE) %>% 
    kable(
      caption = "Expected improvement for the two candidate tuning parameters.",
      label = "two-exp-improve"
    ) %>% 
    kableExtra::kable_styling(full_width = FALSE) %>% 
    kableExtra::add_header_above(c(" " = 1, "Predictions" = 3))
  
  y_lab <- expression(Estimated ~ italic(R^2))
  
  p1 <- 
    gp_pred %>% 
    ggplot(aes(x = x)) + 
    geom_point(data = current_iter, aes(y = y)) + 
    geom_line(aes(y = .mean)) +
    geom_ribbon(aes(ymin = .mean -  1 * .sd, ymax = .mean + 1 * .sd), alpha = .1) + 
    labs(y = y_lab, x = NULL) + 
    theme(
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(), 
      plot.margin = unit(c(0, 0, 0, 0), "null")
    )
  
  p2 <- 
    gp_pred %>% 
    ggplot(aes(x = x, y = exp_imp)) + 
    geom_line() + 
    labs(y = "Expected Improvement", x = "Tuning Parameter") + 
    theme(plot.margin = unit(c(0, 0, 0, 0), "null")) +
    geom_vline(xintercept = gp_pred$x[which.max(gp_pred$exp_imp)], lty = 2)
  
  p1/p2
  
  ctrl <- control_bayes(verbose = TRUE)
  ctrl$save_gp_scoring <- TRUE
  
  tune_bayes_sssshhh <- purrr::quietly(tune_bayes)
  
  set.seed(1403)
  svm_bo_sshh <-
    svm_wflow %>%
    tune_bayes_sssshhh(
      resamples = cell_folds,
      metrics = roc_res,
      initial = svm_initial,
      param_info = svm_param,
      iter = 25,
      control = ctrl
    )
  
  
  verify_consistent_bo(collect_metrics(svm_bo_sshh$result))
  
  svm_bo <- svm_bo_sshh$result
  svm_bo_output <- svm_bo_sshh$messages
  
  gp_candidates <- collect_gp_results(svm_bo)
  
  ctrl <- control_bayes(verbose = TRUE)
  
  set.seed(1403)
  svm_bo <-
    svm_wflow %>%
    tune_bayes(
      resamples = cell_folds,
      metrics = roc_res,
      initial = svm_initial,
      param_info = svm_param,
      iter = 25,
      control = ctrl
    )
  
  bo_res <- collect_metrics(svm_bo) %>% mutate(current_best = FALSE)
  for(i in 1:nrow(bo_res)) {
    bo_res$current_best[i] <- bo_res$mean[i] > max(bo_res$mean[1:(i-1)])
  }
  init_vals <- bo_res %>% dplyr::filter(.iter == 0)
  best_init <- max(bo_res$mean[bo_res$.iter == 0])
  best_bo <- max(bo_res$mean)
  best_bo_iter <- bo_res$.iter[which.max(bo_res$mean)]
  new_best_iter <- bo_res$.iter[which(bo_res$current_best)]
  new_best_iter <- new_best_iter[new_best_iter > 0]
  num_improve <- length(new_best_iter)
  last_iter <- max(collect_metrics(svm_bo)$.iter)
  
  iter_1_roc <- bo_res$mean[bo_res$.iter == 1]
  iter_1_imp <- iter_1_roc > best_init
  iter_1_text <- 
    paste0(
      ifelse(iter_1_imp, "showed an improvement, resulting in an ROC value of ",
             "failed to improve the outcome with an ROC value of "),
      round(iter_1_roc, 5), "."
    )
  
  iter_2_roc <- bo_res$mean[bo_res$.iter == 2]
  iter_2_imp <- iter_2_roc > max(bo_res$mean[bo_res$.iter < 2])
  iter_2_text <- 
    dplyr::case_when(
      !iter_1_imp &  !iter_2_imp ~ 
        paste0("the second iteration also failed to yield an improvement."),
      !iter_1_imp &   iter_2_imp ~ 
        paste0("the second iteration did yield a better result with an area under the ROC curve of ", 
               round(iter_2_roc, 5), "."),
      iter_1_imp &  !iter_2_imp ~ 
        paste0("the second iteration did not continue the trend with a suboptimal ROC value of ",
               round(iter_2_roc, 5), "."),
      iter_1_imp &  !iter_2_imp ~ 
        paste0("the second iteration further increased the outcome value (ROC = ",
               round(iter_2_roc, 5), ").") 
    )
  
  if (num_improve > 1) {
    improve_text <-
      paste0(
        "There were a total of ",
        num_improve,
        " improvements in the outcome along the way at iterations ",
        knitr::combine_words(new_best_iter),
        "."
      )
  } else {
    improve_text <-
      paste0("There was only a single improvement in the outcome at iteration ",
             new_best_iter,
             ".")
  }
  
  if (last_iter < 25) {
    last_bo_text <-
      paste0(
        "There were no more improvements and the default option is to stop if no progress is made after `no_improve = ",
        ctrl$no_improve,
        "` more steps. The last step was:"
      )
  } else {
    last_bo_text <- "The last step was:"
  }
  
  so_stop_index <- grep("Iteration 3", svm_bo_output) 
  if (length(so_stop_index) > 0) {
    cat(svm_bo_output[1:(so_stop_index - 2)], sep = "")
  }
  
  all_imp_index <- grep("♥", svm_bo_output)
  so_stop_index <- all_imp_index[length(all_imp_index)]
  if (length(all_imp_index) > 0) {
    so_start_index <- so_stop_index - 10
    cat(svm_bo_output[so_start_index:(so_stop_index + 1)], sep = "")
  }

  
  
  so_start <- paste("Iteration", last_iter)
  so_start_index <- grep(so_start, svm_bo_output)
  if (length(so_start_index) > 0) {
    cat(svm_bo_output[so_start_index:length(svm_bo_output)], sep = "")
  }
  
  show_best(svm_bo)
  
  av_capture_graphics(
    make_bo_animation(gp_candidates, svm_bo),
    output = "bo_search.mp4",
    width = 760,
    height = 760,
    res = 100,
    vfilter = 'framerate=fps=10', 
    framerate = 1/3
  )
  
  get_accept_probs <- function(coef, pct_diff) {
    # pct loss to abs value
    candidate <- .8 - (pct_diff  * .8 /100) 
    
    x <- finetune:::acceptance_prob(0.8, candidate, 1:50, coef = coef, maximize = TRUE)
    tibble(
      `Acceptance Probability` = x,
      iteration = 1:50,
      pct_diff = pct_diff, 
      coefficient = coef
    )
  }
  
  prob_settings <- crossing(pct_diff = 1:10, coefficient = c(10, 20, 30)/1000)
  prob_res <- purrr::map2_dfr(prob_settings$coefficient, prob_settings$pct_diff, get_accept_probs)
  
  ggplot(prob_res, aes(x = iteration, y = pct_diff, fill = `Acceptance Probability`)) +
    geom_raster() +
    facet_wrap( ~ coefficient, labeller = label_both) +
    scale_fill_gradientn(
      colours = scales::brewer_pal(palette = "Greens")(8),
      limits = 0:1
    ) +
    labs(y = "Percent Loss", x = "Iteration")
  
  glmn_param <- parameters(penalty(), mixture())
  pen_rng <- unlist(range_get(penalty(), original = TRUE))
  mix_rng <- 0:1
  
  iter_1 <- tibble(penalty = 0.025, mixture = .05)
  next_neighbors <- 
    finetune:::random_real_neighbor(iter_1, iter_1, glmn_param, retain = 300) %>% 
    mutate(Iteration = 1)
  
  set.seed(1)
  neighbors_values <- next_neighbors
  best_values <- iter_1 %>% mutate(Iteration = 1)
  
  scoring <- function(x) {
    - log10(x$penalty) * .1 + x$mixture * 2 + rnorm(nrow(x), sd = .5)
  }
  
  path <- best_values
  
  for (i in 2:6) {
    set.seed(i + 5)
    next_scores <- scoring(next_neighbors)
    next_ind <- which.max(next_scores)
    next_value <- next_neighbors %>% slice(next_ind) %>% mutate(Iteration = i)
    
    best_values <- 
      bind_rows(
        best_values,
        next_value
      )
    path <- bind_rows(path, best_values %>% mutate(Iteration = i))
    
    next_neighbors <- 
      finetune:::random_real_neighbor(next_value %>% select(-Iteration), 
                                      path %>% select(-Iteration), 
                                      glmn_param, retain = 300) %>% 
      mutate(Iteration = i)
    neighbors_values <- 
      bind_rows(
        neighbors_values,
        next_neighbors
      )  
  }
  
  ggplot(neighbors_values, aes(x = penalty, y = mixture)) + 
    geom_point(alpha = .3, size = 3/4, aes(color = factor(Iteration)), show.legend = FALSE) + 
    scale_x_continuous(trans = "log10", limits = pen_rng) + 
    scale_y_continuous(limits = mix_rng) + 
    geom_point(data = best_values) + 
    geom_path(data = path) + 
    geom_point(data = path) + 
    facet_wrap(vars(Iteration), labeller = label_both) + 
    labs(
      x = paste(penalty()$label, "(penalty)"),
      y = paste(mixture()$label, "(mixture)")
    )
  
  ctrl_sa <- control_sim_anneal(no_improve = 10L, verbose = TRUE, save_history = TRUE)
  
  tune_sim_anneal_sssshhh <- purrr::quietly(tune_sim_anneal)
  
  set.seed(1404)
  svm_sa_sshh <-
    svm_wflow %>%
    tune_sim_anneal_sssshhh(
      resamples = cell_folds,
      metrics = roc_res,
      initial = svm_initial,
      param_info = svm_param,
      iter = 50,
      control = ctrl_sa
    )
  
  verify_consistent_sa(collect_metrics(svm_sa_sshh$result))
  
  svm_sa <- svm_sa_sshh$result
  svm_sa_output <- svm_sa_sshh$messages
  
  # We set tune_sim_anneal() to save a file to the temp directory.
  file.copy(
    file.path(tempdir(), "sa_history.RData"),
    "RData/sa_history.RData",
    overwrite = TRUE
  )
  
  ctrl_sa <- control_sim_anneal(verbose = TRUE, no_improve = 10L)
  
  set.seed(1404)
  svm_sa <-
    svm_wflow %>%
    tune_sim_anneal(
      resamples = cell_folds,
      metrics = roc_res,
      initial = svm_initial,
      param_info = svm_param,
      iter = 50,
      control = ctrl_sa
    )
  
  load("RData/sa_history.RData")
  
  ## -----------------------------------------------------------------------------
  
  restart_iter <- result_history$.iter[result_history$results == "restart from best"]
  restart_num <- length(restart_iter)
  sa_iter_list <- knitr::combine_words(restart_iter)
  restart_txt <- 
    dplyr::case_when(
      restart_num == 0 ~ paste0("There were no restarts during the search."),
      restart_num == 1 ~ paste0("There was a single restart at iteration ", restart_iter)[1],
      TRUE ~ paste0("There were ", restart_num, " restarts at iterations ", sa_iter_list)[1]
    )
  discard_num<- length(result_history$.iter[result_history$results == "discard suboptimal"])
  if (discard_num > 0) {
    restart_txt <-
      paste0(
        restart_txt, 
        " as well as ", 
        discard_num, 
        " discarded ", 
        ifelse(discard_num > 1, "candidates ", "candidate "),
        "during the process."
      )
  } else {
    restart_txt <- paste0(restart_txt, ".")
  }
  
  ## -----------------------------------------------------------------------------
  
  best_iters <- result_history$.iter[result_history$results == "new best"]
  best_init <- max(result_history$mean[result_history$.iter == 0])
  best_sa_res <- max(result_history$mean[result_history$.iter > 0])
  best_sa_inds <- result_history$.iter[which.max(result_history$mean)]
  best_txt <-
    dplyr::case_when(
      restart_num == 1 ~ paste0("a new global optimum once at iteration ", best_iters, "."),
      TRUE ~ paste0("new global optimums at ", length(best_iters), " different iterations.")[1]
    )
  best_txt <- best_txt[1]
  if (length(best_iters) > 1) {
    best_txt <-
      paste0(
        best_txt,
        " The earliest improvement was at iteration ",
        min(best_iters),
        " and the final optimum occured at iteration ",
        max(best_iters),
        ". The best overall results occured at iteration ", 
        best_sa_inds, " with a mean area under the ROC curve of ",
        round(best_sa_res, 4), " (compared to an initial best of ",
        round(best_init, 4), ")."
      )
  }
  
  so_stop_index <- grep("^ 5", svm_sa_output)
  if (length(so_stop_index) > 0) {
    cat(svm_sa_output[1:so_stop_index], sep = "\n")
  }
  
  last_sa_iter <- max(result_history$.iter)
  
  so_start_index <- grep(paste0("^", last_sa_iter - 10), svm_sa_output)
  so_stop_index  <- grep(paste0("^", last_sa_iter), svm_sa_output)
  if (length(so_stop_index) > 0) {
    cat(svm_sa_output[so_start_index:so_stop_index], sep = "\n")
  }
  
  autoplot(svm_sa, type = "performance")
  
  autoplot(svm_sa, type = "parameters")
  
  av_capture_graphics(
    sa_2d_plot(svm_sa, result_history, svm_large),
    output = "sa_search.mp4",
    width = 720,
    height = 720,
    res = 120,
    vfilter = 'framerate=fps=10', 
    framerate = 1/3
  )
  
  
  