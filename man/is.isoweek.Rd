% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/isoweekStart.R
\name{is.isoweek}
\alias{is.isoweek}
\title{Are values valid ISO weeks?}
\usage{
is.isoweek(x)
}
\arguments{
\item{x}{A numeric vector of ISO week numbers (of format YYYYWW)}
}
\value{
A logical vector of the same length as \code{x}
}
\description{
Take a numeric vector and return \code{TRUE} if values are valid ISO week numbers (of the
  form YYYYWW), or \code{FALSE} otherwise.
}
\examples{
is.isoweek(c(202053, 202153))  # returns c(TRUE, FALSE)

}
