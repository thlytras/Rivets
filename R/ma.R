#' Create lagged or lead variables
#'
#' This function replicates the functionality of the Stata subscript syntax (e.g. [_n-1])
#' to positionally shift a vector and create lagged or lead variables.
#'
#' @param x A vector (of any type)
#' @param d How many places to shift. A positive integer means a shift to the left,
#'   a negative integer means a shift to the right.
#'
#' @details This function calculates the week number according to ISO 8601. Note that
#'    dates near the start or end of a given year may belong to the previous or next
#'    year respectively, thus the year needs to be calculated too.
#'
#' @return A vector of the same length as x, shifted \code{d} places to the left (if 
#'    \code{d} positive) or right (if \code{d} negative). The rightmost or leftmost
#'    \code{abs(d)} place respectively are filled with \code{NA}.
#'
#' @examples
#' vecshift(1:10, 3)
#'
#' @export
vecshift <- function(x, d=0) {
  if (d==0) return(x)
  if (d>0) return(c(x[-(1:d)], rep(NA,d)))
  if (d<0) return(c(rep(NA, -d), x[1:(length(x)+d)]))
}


#' Calculate a simple moving average
#'
#' This function calculates a simple moving average
#'
#' @param x A numeric vector
#' @param d Window size
#' @param na.rm Use a shorter window size for the first values? See "Return Value".
#'
#' @return A vector of the same length as x. If \code{na.rm=FALSE} (the default),
#'    the first \code{d-1} places of the return vector are \code{NA}. If \code{na.rm=TRUE},
#'    a moving average is calculated for these with a shorter window size.
#'
#' @examples
#' ma(1:6, 3)
#' # Returns NA NA 2 3 4 5
#' 
#' ma(1:6, 3, na.rm=TRUE)
#' # Returns 1.0 1.5 2.0 3.0 4.0 5.0
#'
#' @export
ma <- function(x, d, na.rm=FALSE) {
  apply(sapply(-(0:(d-1)), function(i) vecshift(x,i)), 1, mean, na.rm=na.rm)
}
