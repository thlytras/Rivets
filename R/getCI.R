#' Get point estimates and 95% Confidence Intervals from a regression model
#'
#' This function generates point estimates and 95% Confidence Intervals for 
#' the parameters of a regression model.
#'
#' @param m A regression model
#' @param intercept Include the intercept in the result?
#' @param prob Default 0.95 for a 95% Confidence Interval
#'
#' @return A data.frame with three columns: \code{est}, \code{lo}, \code{hi} providing 
#'    the point estimate and lower/upper limits of the 95% Confidence Interval for each 
#'    model parameter.
#' 
#'    The function supports objects of class 'lm' and 'glm', 'lmerMod' and 'glmerMod' 
#'    (mixed-effects models from package \code{lme4}, for which only the 
#'    fixed-effects part is considered) and 'coxph' (from package \code{survival})
#'
#' @examples
#' mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
#' mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
#' getCI(mylogit)
#'
#' @export
getCI <- function(m, intercept=FALSE, prob=0.95) {
  cf <- coef(summary(m))[,1:2]
  if (inherits(m, "coxph")) cf <- coef(summary(m))[,c(1,3)]
  if (!intercept & !inherits(m, "coxph")) cf <- cf[-1,,drop=FALSE]
  res <- as.data.frame.matrix(as.matrix(cf) %*% rbind(1, qnorm((1-prob)/2)*c(0, 1, -1)))
  colnames(res) <- c("est", "lo", "hi")
  res
}
