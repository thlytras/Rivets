#' Get estimates for a parameter combination from a regression model
#'
#' This function generates point estimates and 95% Confidence Intervals from a 
#' regression model, for any combination of model parameters.
#'
#' @param m A vector of class \code{\link{Date}} (of length >=1)
#' @param par A matrix (or data.frame) specifying the parameter combinations. See Details.
#'
#' @details This function calculates the week number according to ISO 8601. Note that
#'    dates near the start or end of a given year may belong to the previous or next
#'    year respectively, thus the year needs to be calculated too.
#'
#' @return A data.frame with three columns: \code{est}, \code{lo}, \code{hi} providing 
#'    the point estimate and lower/upper limits of the 95% Confidence Interval, for each 
#'    parameter combination provided in \code{par}. Thus the return value has the same 
#'    number of rows as \code{par}.
#' 
#'    The function supports objects of class 'lm' and 'glm', 'lmerMod' and 'glmerMod' 
#'    (mixed-effects models from package \code{lme4}, for which only the 
#'    fixed-effects part is considered) and 'coxph' (from package \code{survival})
#'
#' @examples
#' isoweek(Sys.Date())
#' mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
#' mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
#' pars <- data.frame(gpa=c(2,2,3), rank=c(3,4,4))
#' getCoef(mylogit, pars)
#'
#' @export
getCoef <- function(m, par) {
  if (!inherits(m, c("lm","glm","lmerMod","glmerMod","coxph"))) 
    stop("Model object does not belong in one of the supported classes")
  par <- as.matrix(par)
  n <- names(coef(m))
  pnf <- colnames(par)[!(colnames(par) %in% n)]  # Parameters not found
  if (length(pnf)>0) stop(paste("Some parameters in 'par' not found in model parameters:", paste(pnf, collapse=", ")))
  if (inherits(m, c("lmerMod","glmerMod"))) {
    cf <- fixef(m)[colnames(par)]
  } else {
    cf <- coef(m)[colnames(par)]
  }
  vcv <- vcov(m)[colnames(par), colnames(par)]
  pe <- c(par %*% cf)
  vr <- diag(par %*% tcrossprod(vcv, par))
  res <- data.frame(est = pe, lo = pe + qnorm(0.025)*sqrt(vr), hi = pe - qnorm(0.025)*sqrt(vr))
  res
}
