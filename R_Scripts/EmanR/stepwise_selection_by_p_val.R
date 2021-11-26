library('broom')

#' Performs backwards selection by running lm repeatedly and removing the term
#' with the largest p-value each time, until all variables left in the model
#' have an associated p-value smaller than some threshold specified
#'
#' @param data dataframe containing relevant data
#' @param response_var str of column name of response variable
#' @param p_val num from 0 to 1 of maximum threshold for p-values
#' 
#' @details prints output of each iteration number and model summary
#' 
backwards_step <- function(data, response_var, p_val){
  formula <- paste0(response_var, " ~ .")
  model <- lm(as.formula(formula), data)
  tm <- tidy(model)
  tm <- subset(tm, term != "(Intercept)")
  non_sig_vars <- tm$term[tm$p.value > p_val]
  iter <- 0
  print(paste0("----------Iteration: ", iter,"----------"))
  print(summary(model))
  while (length(non_sig_vars) > 0){
    remove_var <- tm$term[which.max(tm$p.value)]
    formula <- paste0(formula, " - ", remove_var)
    model <- lm(as.formula(formula), data)
    tm <- tidy(model)
    tm <- subset(tm, term != "(Intercept)")
    non_sig_vars <- tm$term[tm$p.value > p_val]
    iter <- iter + 1
    print(paste0("----------Iteration: ", iter,"----------"))
    print(summary(model))
  }
}

# Example with dummy data
eg_df <- airquality
backwards_step(eg_df, "Solar.R", 0.15)
