#ch9 judge model effectiveness
desha_res <- predict(lm_fit, new_data = tbDesharnais_test %>% select(-Effort))
desha_res
#match predicted with observed outcome
desha_res <- bind_cols(desha_res, tbDesharnais_test %>%
                         select(Effort))
desha_res
#plot data first
ggplot(desha_res, aes(x= Effort, y = .pred)) +
  #create a diagonal line
  geom_abline(lty =2) +
  geom_point(alpha = 0.5) +
  labs(y = "Predicted Effort", x = "F Points") +
  # scale and size x y axis uniformly
  coord_obs_pred()
# calculate rmse
  rmse(desha_res, truth = Effort, estimate = .pred)
#
#compute rmse , rsq, mae   using metric set
desha_metrics <- metric_set(rmse,rsq,mae)
desha_metrics(desha_res, truth = Effort, estimate = .pred)
