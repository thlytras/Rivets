#' Multiple argument version of gsub()
#'
#' This functions does pattern matching and replacement on a character vector 
#' just like gsub(), but accepts multiple pattern arguments that are replaced 
#' sequentially on the target character vector.
#' 
#' @param pattern A character vector (possibly of length >1) of patterns to be matched, 
#' see \link{\code{gsub}}
#' @param replacement A character vector (of same length as \code{pattern}) with replacement
#' for each matched pattern. See \link{\code{gsub}}
#' @param x a character vector where matches are sought, see \link{\code{gsub}}
#' @param ... further arguments passed to \link{\code{gsub}} (such as \code{fixed}, for example)
#'
#' @return The target character vector \code{x} with each matching pattern replaced in sequence.
#'
#' @export 
mgsub <- function(pattern, replacement, x, ...) {
  if (length(pattern)!=length(replacement)) {
    stop("pattern and replacement do not have the same length.")
  }
  result <- x
  for (i in 1:length(pattern)) {
    result <- gsub(pattern[i], replacement[i], result, ...)
  }
  result
}
