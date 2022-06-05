#' My lm
#'
#' My_lm is used to fit linear models.
#'
#' @param formula An object of class "formula". A symbolic description of the
#'   model to be fitted.
#' @param data.frame A data frame containing the variables in the model.
#'
#' @keywords lm linear model
#'
#' @return A table with rows for each coefficient (including the intercept) and
#'   columns for the estimate, standard error, t-value, and p-value.
#'
#' @examples
#' my_lm(my_penguins$bill_length_mm, my_penguins$bill_depth_mm)
#' my_lm(my_penguins$body_mass_g, my_penguins$flipper_length_mm)
#'
#' @export
my_lm <- function(formula, data.frame) {
  #suppress NaNs warning
  withr::local_options(.new = list(warn = -1))
  #create a table: rows (coefficient and independent variable) columns (estimate, error, t value, p value)
  results <- data.frame(matrix(NA, nrow = 2, ncol = 4))
  rownames(results) <- c("Intercept", "data.frame")
  colnames(results) <- c("Estimate", "Std. Error", "t value", "p value")
  #find estimates
  formula_data <- formula ~ data.frame
  X <- model.matrix(formula_data)
  Xframe <- model.frame(formula_data)
  Y <- model.response(Xframe)
  Xt <- t(X)
  XtXinverse <- solve(Xt %*% X, diag(2))
  beta_hat <- XtXinverse %*% Xt %*% Y
  #find errors
  df_lm <- length(formula) - 2
  for (i in Y) {
    for (j in X) {
      each_variance <- ((Y - (X %*% beta_hat))^2)/df_lm
    }
  }
  variance <- sum(each_variance)
  variance <- sum(each_variance)
  se <- diag(sqrt(variance * XtXinverse))
  #find t values
  t_values <- beta_hat / se
  #find p values
  p_values <- 2 * pt(abs(t_values), df = df_lm, lower.tail = FALSE)
  #enter values into table
  results[,1] <- beta_hat
  results[,2] <- se
  results[,3] <- t_values
  results[,4] <- p_values
  #return table
  return(results)
}
