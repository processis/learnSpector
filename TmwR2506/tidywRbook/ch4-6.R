####ch4

library(modeldata) # This is also loaded by the tidymodels package
data(ames)

data(ames, package = "modeldata")

dim(ames)

library(tidymodels)
tidymodels_prefer()

ggplot(ames, aes(x = Sale_Price)) + 
  geom_histogram(bins = 50, col= "white")

ggplot(ames, aes(x = Sale_Price)) + 
  geom_histogram(bins = 50, col= "white") +
  scale_x_log10()

knitr::include_graphics("premade/northridge.png")

knitr::include_graphics("premade/crawford.png")

knitr::include_graphics("premade/dot_rr.png")


library(tidymodels)
data(ames)
ames <- ames %>% mutate(Sale_Price = log10(Sale_Price))


#####ch5

library(tidymodels)
tidymodels_prefer()

source("ames_snippets.R")

set.seed(501)
ames_split <- initial_split(ames, prop = 0.80)
ames_split


ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

dim(ames_train)


sale_dens <- 
  density(ames$Sale_Price, n = 2^10) %>% 
  tidy() 
quartiles <- quantile(ames$Sale_Price, probs = c(1:3)/4)
quartiles <- tibble(prob = (1:3/4), value = unname(quartiles))
quartiles$y <- approx(sale_dens$x, sale_dens$y, xout = quartiles$value)$y

quart_plot <-
  ggplot(ames, aes(x = Sale_Price)) +
  geom_line(stat = "density") +
  geom_segment(data = quartiles,
               aes(x = value, xend = value, y = 0, yend = y),
               lty = 2) +
  labs(x = "Sale Price (log-10 USD)", y = NULL)
quart_plot

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

dim(ames_train)


set.seed(52)
# To put 60% into training, 20% in validation, and 20% in testing:
ames_val_split <- initial_validation_split(ames, prop = c(0.6, 0.2))
ames_val_split

ames_train <- training(ames_val_split)
ames_test <- testing(ames_val_split)
ames_val <- validation(ames_val_split)


library(tidymodels)
data(ames)
ames <- ames %>% mutate(Sale_Price = log10(Sale_Price))

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)



###########ch6

library(tidymodels)
library(kknn)
library(kableExtra)
library(tidyr)

tidymodels_prefer()

source("ames_snippets.R")


library(tidymodels)
tidymodels_prefer()

linear_reg() %>% set_engine("lm")

linear_reg() %>% set_engine("glmnet") 

linear_reg() %>% set_engine("stan")


linear_reg() %>% set_engine("lm") %>% translate()

linear_reg(penalty = 1) %>% set_engine("glmnet") %>% translate()

linear_reg() %>% set_engine("stan") %>% translate()


lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

lm_form_fit <- 
  lm_model %>% 
  # Recall that Sale_Price has been pre-logged
  fit(Sale_Price ~ Longitude + Latitude, data = ames_train)

lm_xy_fit <- 
  lm_model %>% 
  fit_xy(
    x = ames_train %>% select(Longitude, Latitude),
    y = ames_train %>% pull(Sale_Price)
  )

lm_form_fit
lm_xy_fit


arg_info <- 
  tribble(
    ~ `Argument Type`, ~parsnip,
    "# trees", "trees",
    "# sampled predictors", "mtry",
    "# data points to split", "min_n"
  )

arg_info <-
  get_from_env("rand_forest_args") %>% 
  select(engine, parsnip, original) %>% 
  full_join(arg_info, by = "parsnip") %>% 
  mutate(package = ifelse(engine == "spark", "sparklyr", engine))

arg_info %>%
  select(package, `Argument Type`, original) %>%
  # mutate(original = paste0("<tt>", original, "</tt>")) %>% 
  pivot_wider(
    id_cols = c(`Argument Type`),
    values_from = c(original),
    names_from = c(package)
  ) %>% 
  kable(
    caption = "Example argument names for different random forest functions.",
    label = "rand-forest-args",
    escape = FALSE
  ) %>%
  kable_styling() %>%
  column_spec(2:4, monospace = TRUE)


arg_info %>%
  select(`Argument Type`, parsnip) %>%
  distinct() %>% 
  # mutate(parsnip = paste0("<tt>", parsnip, "</tt>")) %>% 
  kable(
    caption = "Random forest argument names used by parsnip.",
    label = "parsnip-args",
    escape = FALSE
  ) %>% 
  kable_styling(full_width = FALSE) %>%
  column_spec(2, monospace = TRUE)


rand_forest(trees = 1000, min_n = 5) %>% 
  set_engine("ranger") %>% 
  set_mode("regression") %>% 
  translate()


rand_forest(trees = 1000, min_n = 5) %>% 
  set_engine("ranger", verbose = TRUE) %>% 
  set_mode("regression") 

lm_form_fit %>% extract_fit_engine()

lm_form_fit %>% extract_fit_engine() %>% vcov()

model_res <- 
  lm_form_fit %>% 
  extract_fit_engine() %>% 
  summary()

# The model coefficient table is accessible via the `coef` method.
param_est <- coef(model_res)
class(param_est)
param_est


tidy(lm_form_fit)

ames_test_small <- ames_test %>% slice(1:5)
predict(lm_form_fit, new_data = ames_test_small)

ames_test_small %>% 
  select(Sale_Price) %>% 
  bind_cols(predict(lm_form_fit, ames_test_small)) %>% 
  # Add 95% prediction intervals to the results:
  bind_cols(predict(lm_form_fit, ames_test_small, type = "pred_int")) 


tribble(
  ~ `Type of Prediction`, ~ `Returns a:`,
  "numeric",                 "numeric matrix",
  "class",                   "character matrix",
  "probability (2 classes)", "numeric matrix (2nd level only)",
  "probability (3+ classes)", "3D numeric array (all levels)", 
) %>% 
  kable(
    caption = "Different return values for glmnet prediction types.",
    label = "predict-types"
  ) %>% 
  kable_styling(full_width = FALSE)



tribble(
  ~ `type value`, ~ `column name(s)`,
  "numeric", ".pred",
  "class", ".pred_class",
  "prob", ".pred_{class levels}",
  "conf_int", ".pred_lower, .pred_upper",
  "pred_int", ".pred_lower, .pred_upper"
) %>% 
  kable(
    caption = "The tidymodels mapping of prediction types and column names.",
    label = "predictable-column-names",
  ) %>% 
  kable_styling(full_width = FALSE)  %>%
  column_spec(1:2, monospace = TRUE)



tree_model <- 
  decision_tree(min_n = 2) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

tree_fit <- 
  tree_model %>% 
  fit(Sale_Price ~ Longitude + Latitude, data = ames_train)

ames_test_small %>% 
  select(Sale_Price) %>% 
  bind_cols(predict(tree_fit, ames_test_small))


parsnip_addin()


library(tidymodels)
data(ames)
ames <- mutate(ames, Sale_Price = log10(Sale_Price))

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

lm_model <- linear_reg() %>% set_engine("lm")


###############################ch7

library(tidymodels)
library(workflowsets)
library(kableExtra)
library(censored)
library(survival)
tidymodels_prefer()
source("ames_snippets.R")


library(tidymodels)  # Includes the workflows package
tidymodels_prefer()

lm_model <- 
  linear_reg() %>% 
  set_engine("lm")


lm_wflow <- 
  workflow() %>% 
  add_model(lm_model)

lm_wflow


lm_wflow <- 
  lm_wflow %>% 
  add_formula(Sale_Price ~ Longitude + Latitude)

lm_wflow

lm_fit <- fit(lm_wflow, ames_train)
lm_fit

predict(lm_fit, ames_test %>% slice(1:3))

lm_fit %>% update_formula(Sale_Price ~ Longitude)


lm_wflow <- 
  lm_wflow %>% 
  remove_formula() %>% 
  add_variables(outcome = Sale_Price, predictors = c(Longitude, Latitude))
lm_wflow

#predictors = c(ends_with("tude"))

fit(lm_wflow, ames_train)
###########errors

library(lme4)
lmer(distance ~ Sex + (age | Subject), data = Orthodont)

data(Orthodont, package = "nlme")

model.matrix(distance ~ Sex + (age | Subject), data = Orthodont)

library(multilevelmod)

multilevel_spec <- linear_reg() %>% set_engine("lmer")

multilevel_workflow <- 
  workflow() %>% 
  # Pass the data along as-is: 
  add_variables(outcome = distance, predictors = c(Sex, age, Subject)) %>% 
  add_model(multilevel_spec, 
            # This formula is given to the model
            formula = distance ~ Sex + (age | Subject))

multilevel_fit <- fit(multilevel_workflow, data = Orthodont)
multilevel_fit



library(censored)

parametric_spec <- survival_reg()

parametric_workflow <- 
  workflow() %>% 
  add_variables(outcome = c(fustat, futime), predictors = c(age, rx)) %>% 
  add_model(parametric_spec, 
            formula = Surv(futime, fustat) ~ age + strata(rx))

parametric_fit <- fit(parametric_workflow, data = ovarian)
parametric_fit


location <- list(
  longitude = Sale_Price ~ Longitude,
  latitude = Sale_Price ~ Latitude,
  coords = Sale_Price ~ Longitude + Latitude,
  neighborhood = Sale_Price ~ Neighborhood
)



library(workflowsets)
location_models <- workflow_set(preproc = location, models = list(lm = lm_model))
location_models
location_models$info[[1]]



location_models <-
  location_models %>%
  mutate(fit = map(info, ~ fit(.x$workflow[[1]], ames_train)))
location_models
location_models$fit[[1]]


extract_workflow(location_models, id = "coords_lm")


fitted_lm_wflow <- extract_workflow(final_lm_res)

collect_metrics(final_lm_res)
collect_predictions(final_lm_res) %>% slice(1:5)


library(tidymodels)
data(ames)

ames <- mutate(ames, Sale_Price = log10(Sale_Price))

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

lm_model <- linear_reg() %>% set_engine("lm")

lm_wflow <- 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_variables(outcome = Sale_Price, predictors = c(Longitude, Latitude))

lm_fit <- fit(lm_wflow, ames_train)


########################ch8

library(tidymodels)
library(kableExtra)

tidymodels_prefer()

val_list <- function(x) {
  x <- format(table(x), big.mark = ",")
  x <- paste0("`", names(x), "` ($n = ", unname(x), "$)")
  knitr::combine_words(x)
}

source("/media/user/娱乐/learnSpector/TmwR2506/ames_snippets.R")

lm_wflow <- 
  lm_wflow %>% 
  remove_recipe() %>% 
  add_variables(outcome = Sale_Price, predictors = c(Longitude, Latitude))

library(tidymodels) # Includes the recipes package
tidymodels_prefer()

simple_ames <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_dummy(all_nominal_predictors())
simple_ames

lm_wflow %>% 
  add_recipe(simple_ames)

lm_wflow <- 
  lm_wflow %>% 
  remove_variables() %>% 
  add_recipe(simple_ames)
lm_wflow

lm_fit <- fit(lm_wflow, ames_train)

predict(lm_fit, ames_test %>% slice(1:3))

lm_fit %>% 
  extract_recipe(estimated = TRUE)

lm_fit %>% 
  # This returns the parsnip object:
  extract_fit_parsnip() %>% 
  # Now tidy the linear model object:
  tidy() %>% 
  slice(1:5)


ggplot(ames_train, aes(y = Neighborhood)) + 
  geom_bar() + 
  labs(y = NULL)

simple_ames <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors())


show_rows <- 
  ames_train %>% 
  mutate(.row = row_number()) %>% 
  group_by(Bldg_Type) %>% dplyr::select(Bldg_Type, .row) %>% 
  slice(1) %>% 
  pull(.row)
recipe(~Bldg_Type, data = ames_train) %>% 
  step_mutate(`Raw Data` = Bldg_Type) %>% 
  step_dummy(Bldg_Type, naming = function(var, lvl, ordinal = FALSE, sep = "_") lvl) %>% 
  prep() %>% 
  bake(ames_train) %>% 
  slice(show_rows) %>% 
  arrange(`Raw Data`) %>% 
  kable(
    caption = 'Illustration of binary encodings (i.e., dummy variables) for a qualitative predictor.',
    label = "dummy-vars"
  ) %>% 
  kable_styling(full_width = FALSE)


ggplot(ames_train, aes(x = Gr_Liv_Area, y = 10^Sale_Price)) + 
  geom_point(alpha = .2) + 
  facet_wrap(~ Bldg_Type) + 
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "lightblue") + 
  scale_x_log10() + 
  scale_y_log10() + 
  labs(x = "Gross Living Area", y = "Sale Price (USD)")


simple_ames <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  # Gr_Liv_Area is on the log scale from a previous step
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") )



library(patchwork)
library(splines)

plot_smoother <- function(deg_free) {
  ggplot(ames_train, aes(x = Latitude, y = 10^Sale_Price)) + 
    geom_point(alpha = .2) + 
    scale_y_log10() +
    geom_smooth(
      method = lm,
      formula = y ~ ns(x, df = deg_free),
      color = "lightblue",
      se = FALSE
    ) +
    labs(title = paste(deg_free, "Spline Terms"),
         y = "Sale Price (USD)")
}

( plot_smoother(2) + plot_smoother(5) ) / ( plot_smoother(20) + plot_smoother(100) )



recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + Latitude,
       data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, deg_free = 20)


library(recipes)
library(themis)

preps <- as.character(methods("prep"))
steps <- gsub("prep\\.", "", preps)
steps <- grep("^step", steps, value = TRUE)

skip <- rep(rlang::na_lgl, length(steps))
for (i in seq_along(skip)) {
  x_code <- try(getFromNamespace(steps[i], "recipes"), silent = TRUE)
  if (inherits(x_code, "try-error")) {
    x_code <- try(getFromNamespace(steps[i], "themis"), silent = TRUE)
  }
  if (!inherits(x_code, "try-error")) {
    skip[i] <- formals(x_code)$skip
  }
}

skip_list <- paste0("`", steps[skip], "()`")

ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, Longitude, deg_free = 20)

tidy(ames_rec)

ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01, id = "my_id") %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, Longitude, deg_free = 20)

lm_wflow <- 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(ames_rec)

lm_fit <- fit(lm_wflow, ames_train)

estimated_recipe <- 
  lm_fit %>% 
  extract_recipe(estimated = TRUE)

tidy(estimated_recipe, id = "my_id")


tidy(estimated_recipe, number = 2)



library(tidymodels)
data(ames)
ames <- mutate(ames, Sale_Price = log10(Sale_Price))

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, Longitude, deg_free = 20)

lm_model <- linear_reg() %>% set_engine("lm")

lm_wflow <- 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(ames_rec)

lm_fit <- fit(lm_wflow, ames_train)

if(is_new_version(lm_fit, "RData/lm_fit.RData")) {
  save(lm_fit, file = "RData/lm_fit.RData", version = 2, compress = "xz")
}