#' Add transparency (alpha) to a vector of colours
#'
#' This function accepts a vector of colours and adds a given alpha to make them 
#' transparent.
#'
#' @param x A vector of colours (names, hex, etc)
#' @param alpha The required alpha. 0 is fully transparent, 1 is fully opaque.
#'
#' @return A vector of colours of the same length as x, with transparency added.
#'
#' @examples
#' addalpha(c("red", "green", "blue"), 0.3)
#' addalpha(rainbow(8), 0.6)
#'
#' @export
addalpha <- function(x, alpha=1.0) {
  r <- col2rgb(x, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}
