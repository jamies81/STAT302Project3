#' My t-test
#'
#' Performs a one sample t-test on vectors of data.
#'
#' @param x A numeric vector of data.
#' @param alternative A character string specifying the alternative hypothesis.
#'   This only accepts "two.sided", "less", or "greater".
#' @param mu A number indicating the null hypothesis value of the mean.
#'
#' @keywords t-test test statistic
#'
#' @return A list of the following elements: test statistic, degrees of freedom,
#'   alternative, and p-value.
#'
#' @examples
#' my_t.test(my_penguins$body_mass_g, alternative = "two.sided", mu = 0)
#' my_t.test(my_penguins$bill_length_mm, alternative = "greater", mu = 1)
#'
#' @export
my_t.test <- function(x, alternative = c("two.sided", "less", "greater"), mu = 0) {
  #remove NAs
  t.test.data <- na.omit(x)
  #create test statistic
  test_stat <- (mean(t.test.data) - mu) / sd(t.test.data) * sqrt(length(t.test.data))
  #create degrees of freedom
  df <- length(t.test.data) - 1
  #check if alternative is accepted responses
  #check if two-sided
  if (alternative == "two.sided") {
    #create p-value
    p_val <- 2 * pt(-abs(test_stat), df, lower.tail = TRUE)
    #return list
    return(list("test statistic" = test_stat,
                "degrees of freedom" = df,
                "alternative" = alternative,
                "p value" = p_val))
  } else if (alternative == "less") {
    #create p-value
    p_val <- pt(test_stat, df)
    #return list
    return(list("test statistic" = test_stat,
                "degrees of freedom" = df,
                "alternative" = alternative,
                "p value" = p_val))
  } else if (alternative == "greater") {
    #create p-value
    p_val <- pt(test_stat, df, lower.tail = FALSE)
    #return list
    return(list("test statistic" = test_stat,
                "degrees of freedom" = df,
                "alternative" = alternative,
                "p value" = p_val))
  } else
    #informative error
    stop(print("alternative must be 'two.sided', 'less', or 'greater'"))
}
