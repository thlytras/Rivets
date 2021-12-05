#' Plot a band (e.g. confidence band) in a chart
#'
#' This function facilitates plotting coloured 95% confidence bands by using a more 
#' intuitive syntax than \code{polygon}. It is often used together with \code{addAlpha}
#' for drawing nice figures.
#'
#' @param x A vector of x values
#' @param y.lo A vector of y values, the lower limit of the confidence band
#' @param y.hi A vector of y values, the upper limit of the confidence band
#' @param col What colour to draw the band?
#'
#' @details This function calculates the week number according to ISO 8601. Note that
#'    dates near the start or end of a given year may belong to the previous or next
#'    year respectively, thus the year needs to be calculated too.
#'
#' @return None
#'
#' @examples
#' with(cars, plot(speed, dist))
#' m <- lm(dist~speed, data=cars)
#' p <- data.frame(speed=seq(min(cars$speed), max(cars$speed), by=0.1))
#' p <- cbind(p, predict(m, p, interval="confidence"))
#' with(cars, plot(speed, dist, type="n"))
#' plotBand(p$speed, p$lwr, p$upr, col="skyblue")
#' abline(m, lwd=2)
#' with(cars, points(speed, dist, type="p", pch=19))
#'
#' @export
plotBand <- function(x, y.lo, y.hi, col) {
  if (length(x)!=length(y.lo) || length(x)!=length(y.hi))
    stop("Arguments 'x', 'y.lo' and 'y.hi' must have the same length.")
  polygon(c(x, rev(x)), c(y.lo, rev(y.hi)), border=NA, col=col)
}
