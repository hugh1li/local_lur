LUR_full_set <- function(source_data, n_rows, response, dep_col, exclude){
  subsample <- source_data %>% sample_n(size = n_rows, replace =FALSE) 
  model <- subsample %>% make_lur(dat1= ., response = response, exclude = exclude, dep_col = dep_col)
  print(model$formula)
  print(model$summary)
  sx = subsample
  lm_models <- lm(formula(model$formula), sx)
  plot(lm_models, which = 4)
  print(car::vif(lm_models))
  return(list(model = model, data = sx))
}