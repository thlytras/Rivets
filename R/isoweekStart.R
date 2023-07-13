#' Calculate the start date of a given ISO week
#'
#' This function takes a vector of ISO week numbers (of the form YYYYWW) and returns a Date 
#' vector with the first Monday of each week. It is essentially the inverse function of 
#' \code{\link{isoweek}}.
#'
#' @param x A numeric vector of ISO week numbers (of format YYYYWW) 
#'
#' @return A vector of class \code{\link{Date}} and length equal to \code{x}, containing the 
#'   start date (first Monday) of each ISO week. \code{NA} (with a warning) if ISO week is invalid.
#'
#' @examples
#' isoweek(201740) # Start of 2017-18 influenza surveillance
#' isoweekStart(isoweek(Sys.Date()))
#'
#' @export
isoweekStart <- function(x) {
  year <- x %/% 100
  week <- x %% 100
  x.date <- as.Date(paste(year,"-6-1", sep=""))
  x.weekday <- as.integer(format(x.date,"%w"))
  x.weekday[x.weekday==0]=7
  x.nearest.thu <- x.date-x.weekday+4
  x.isoweek <- isoweek(x.nearest.thu, type="week")
  res <- x.nearest.thu + 7*(week-x.isoweek) - 3
  if (sum(is.na(res)) > 0) 
    warning("Invalid ISO week numbers provided")
  return(res)
}



#' Are values valid ISO weeks?
#'
#' Take a numeric vector and return \code{TRUE} if values are valid ISO week numbers (of the
#'   form YYYYWW), or \code{FALSE} otherwise.
#'
#' @param x A numeric vector of ISO week numbers (of format YYYYWW) 
#'
#' @return A logical vector of the same length as \code{x}
#'
#' @examples
#' is.isoweek(c(202053, 202153))  # returns c(TRUE, FALSE)
#'
#' @export
is.isoweek <- function(x) {
  !is.na(suppressWarnings(isoweekStart(x)))
}



#' Generate regular sequences of ISO weeks
#'
#' Take two ISO week numbers (of the form YYYYWW) and returns a regular sequence of 
#' ISO weeks covering the entire range.
#'
#' @param from Numeric vector of length 1 (first week of sequence), or of length 2
#'   (week range) if \code{to} is \code{NULL}
#' @param to Numeric vector of length 1 (last week of sequence), or \code{NULL} (in which
#'   case \code{from} must be a range of weeks).
#'
#' @return A numeric vector of ISO weeks covering the provided range.
#'
#' @examples
#' seq_isoweek(c(202001, 202301))
#' seq_isoweek(201840, 201920)  # Typical influenza surveillance period
#'
#' @export
seq_isoweek <- function(from, to=NULL) {
  if (is.null(to)) {
    to <- from[2]
    from <- from[1]
  }
  if (length(from)>1) {
    from <- from[1]
    warning("Argument 'from' has length >1 and only the first element will be used")
  }
  if (length(to)>1) {
    to <- to[1]
    warning("Argument 'to' has length >1 and only the first element will be used")
  }
  from <- suppressWarnings(isoweekStart(from))
  to <- suppressWarnings(isoweekStart(to))
  if (is.na(from) || is.na(to))
    stop("Invalid ISO weeks supplied")
  res <- seq.Date(from, to, by="week")
  isoweek(res)
}
