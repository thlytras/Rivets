#' Name a vector on-the-fly
#'
#' This function gets a vector and a name(s), and returns the vector with the names assigned.
#'
#' @param x A vector (of any type)
#' @param names A vector of names, with the same length as \code{x}
#'
#' @details This function is useful when one uses c() to concatenate complex arguments, but 
#' does not want to assign these to temporary objects in order to name them.
#'
#' @return Just \code{x} with the \code{names} assigned.
#'
#' @examples
#' c(one=10, nameMe(c(3.14, 5), c("two", "three")))
#'
#' @export
nameMe <- function(x, names) {
  if (length(x) != length(names))
    stop("`names` argument should have the same length as `x`")
  names(x) <- names
  x
}
