## ----base-r-setup, include = FALSE-----------------------------------------------
knitr::opts_chunk$set(fig.path = "figures/")
data(crickets, package = "modeldata")
library(tidyverse)
library(kableExtra)


## ----base-r-cricket-plot, eval = FALSE-------------------------------------------
library(tidyverse)
## 
## data(crickets, package = "modeldata")
## names(crickets)
## 
## # Plot the temperature on the x-axis, the chirp rate on the y-axis. The plot
## # elements will be colored differently for each species:
## ggplot(crickets,
##        aes(x = temp, y = rate, color = species, pch = species, lty = species)) +
##   # Plot points for each data point and color by species
##   geom_point(size = 2) +
##   # Show a simple linear model fit created separately for each species:
##   geom_smooth(method = lm, se = FALSE, alpha = 0.5) +
##   scale_color_brewer(palette = "Paired") +
##   labs(x = "Temperature (C)", y = "Chirp Rate (per minute)")


## ----cricket-plot, ref.label = "base-r-cricket-plot"-----------------------------
#| out.width = '70%',
#| fig.width = 6,
#| fig.height = 4,
#| warning = FALSE,
#| message = FALSE,
#| echo = FALSE,
#| fig.cap = "Relationship between chirp rate and temperature for two different species of crickets",
#| fig.alt = "A scatter plot of the chirp rate and temperature for two different species of crickets with linear trend lines per species. The trends are linearly increasing with a separation between the two species."


## ----base-r-cricket-fit----------------------------------------------------------
interaction_fit <-  lm(rate ~ (temp + species)^2, data = crickets) 

# To print a short summary of the model:
interaction_fit


## ----base-r-interaction-plots, eval = FALSE--------------------------------------
## # Place two plots next to one another:
## par(mfrow = c(1, 2))
## 
## # Show residuals vs predicted values:
## plot(interaction_fit, which = 1)
## 
## # A normal quantile plot on the residuals:
## plot(interaction_fit, which = 2)


## ----interaction-plots, ref.label= "base-r-interaction-plots"--------------------
#| out.width = '100%',
#| fig.width = 8,
#| fig.height = 4.5,
#| warning = FALSE,
#| echo = FALSE,
#| fig.cap = "Residual diagnostic plots for the linear model with interactions, which appear reasonable enough to conduct inferential analysis",
#| fig.alt = "On the left is a scatter plot of the model residuals versus predicted values. There are no strong trends in the data. The right-hand panel shows a normal quantile-quantile plot where the points indicate that normality is probably a good assumption."


## ----base-r-cricket-anova--------------------------------------------------------
# Fit a reduced model:
main_effect_fit <-  lm(rate ~ temp + species, data = crickets) 

# Compare the two:
anova(main_effect_fit, interaction_fit)


## ----base-r-main-coef------------------------------------------------------------
summary(main_effect_fit)


## ----base-r-cricket-pred---------------------------------------------------------
new_values <- data.frame(species = "O. exclamationis", temp = 15:20)
predict(main_effect_fit, new_values)


## ----base-r-three-plots, eval = FALSE--------------------------------------------
## plot(plot_data$x, plot_data$y)
## 
## library(lattice)
## xyplot(y ~ x, data = plot_data)
## 
## library(ggplot2)
## ggplot(plot_data, aes(x = x, y = y)) + geom_point()


## ----prob-args, echo = FALSE, results = "asis"-----------------------------------
prob_tbl <- 
  tribble(
    ~ Function, ~Package, ~Code,
    "lda()"        , "MASS"       ,  "predict(object)"                      ,
    "glm()"        , "stats"      ,  'predict(object, type = "response")'          ,
    "gbm()"        , "gbm"        ,  'predict(object, type = "response", n.trees)' ,
    "mda()"        , "mda"        ,  'predict(object, type = "posterior")'         ,
    "rpart()"      , "rpart"      ,  'predict(object, type = "prob")'              ,
    "various"      , "RWeka"      ,  'predict(object, type = "probability")'       ,
    "logitboost()" , "LogitBoost" ,  'predict(object, type = "raw", nIter)'        ,
    "pamr.train()" , "pamr"       ,  'pamr.predict(object, type = "posterior")'    
  ) 

prob_tbl %>% 
  kable(
    caption = "Heterogeneous argument names for different modeling functions.",
    label = "probability-args",
    escape = FALSE
  ) %>%
  kable_styling(full_width = FALSE) %>%
  column_spec(1, monospace = ifelse(prob_tbl$Function == "various", FALSE, TRUE)) %>%
  column_spec(3, monospace = TRUE)


## ----base-r-lm-missing, error = TRUE---------------------------------------------
# Add a missing value to the prediction set
new_values$temp[1] <- NA

# The predict method for `lm` defaults to `na.pass`:
predict(main_effect_fit, new_values)

# Alternatively 
predict(main_effect_fit, new_values, na.action = na.fail)

predict(main_effect_fit, new_values, na.action = na.omit)


## ----base-r-corr-list------------------------------------------------------------
corr_res <- map(mtcars %>% select(-mpg), cor.test, y = mtcars$mpg)

# The first of ten results in the vector: 
corr_res[[1]]


## ----base-r-corr-tidy------------------------------------------------------------
library(broom)

tidy(corr_res[[1]])


## ----base-r-corr-plot, eval=FALSE------------------------------------------------
## corr_res %>%
##   # Convert each to a tidy format; `map_dfr()` stacks the data frames
##   map_dfr(tidy, .id = "predictor") %>%
##   ggplot(aes(x = fct_reorder(predictor, estimate))) +
##   geom_point(aes(y = estimate)) +
##   geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .1) +
##   labs(x = NULL, y = "Correlation with mpg")


## ----corr-plot, ref.label = "base-r-corr-plot"-----------------------------------
#| echo = FALSE,
#| fig.cap = "Correlations (and 95% confidence intervals) between predictors and the outcome in the `mtcars` data set",
#| fig.alt = "A plot of the correlations (and 95% confidence intervals) between predictors and the outcome in the `mtcars` data set. None of the intervals overlap with zero. The car weight had the largest negative correlation and the rear axle ratio has the highest positive correlation."


## ----base-r-by-species-split-----------------------------------------------------
split_by_species <- 
  crickets %>% 
  group_nest(species) 
split_by_species


## ----base-r-species-models-------------------------------------------------------
model_by_species <- 
  split_by_species %>% 
  mutate(model = map(data, ~ lm(rate ~ temp, data = .x)))
model_by_species


## ----base-r-species-coefs--------------------------------------------------------
model_by_species %>% 
  mutate(coef = map(model, tidy)) %>% 
  select(species, coef) %>% 
  unnest(cols = c(coef))


## ----base-r-detach, warning = FALSE, message = FALSE, echo = FALSE---------------
pkgs <- paste0("package:", 
               c("kableExtra", 
                 "tidyverse", "tidymodels", 
                 tidyverse:::core, tidymodels:::core))
for (i in pkgs) {
  try(detach(i, unload = TRUE, character.only = TRUE, force = TRUE), silent = TRUE)
}


## ----base-r-tidymodels-package---------------------------------------------------
library(tidymodels)


## ----base-r-conflicted, eval = FALSE---------------------------------------------
## library(conflicted)
## conflict_prefer("filter", winner = "dplyr")


## ----base-r-clonflicts-----------------------------------------------------------
tidymodels_prefer(quiet = FALSE)

