#' Calculate or validate number using the Luhn algorithm 
#'
#' This function calculates a simple moving average
#'
#' @param x A number
#' @param calc If \code{TRUE} calculate the check digit, or else validate \code{x}
#'
#' @return If \code{calc=TRUE}, returns the check digit. If \code{calc=FALSE}, then r
#'    returns \code{TRUE} if the number is valid and \code{FALSE} otherwise.
#'
#' @examples
#' luhn(11555175, calc=TRUE)
#' # Returns 6
#' 
#' luhn(115551756)
#' # Returns TRUE
#'
#' @export
luhn <- function(x, calc=FALSE) {
  x <- as.numeric(x)[[1]]
  if (is.na(x)) stop("'x' must be a number")
  calc <- calc[[1]]
  if (!is.logical(calc)) stop("'calc' must be a logical")
  dig <- as.integer(rev(strsplit(as.character(x),"")[[1]]))
  if (!calc) {
    e_chk <- dig[1]
    dig <- dig[-1]
  }
  chksum <- suppressWarnings(sum(as.integer(strsplit(paste0(dig*(2:1),collapse=""),"")[[1]])))
  chk <- (10 - (chksum %% 10)) %% 10
  if (calc) {
    return(chk)
  } else {
    return(e_chk==chk)
  }
}
