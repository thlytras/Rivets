#' Pretty-printer for p-values
#'
#' This function formats p-values in the most usual way, i.e. with two decimals if 
#' p>=0.05, with three decimals if p>=0.001 and p<0.05, and as "p<0.001" if p<0.001.
#' 
#' @param x One or more color names (as a character vector)
#' @param html Escape the < character with the &lt; character entity
#' @param tb Omit the "p=" or "p<" part, e.g. if outputting to a table
#'
#' @return A character vector of equal length to x, with formatted p-values
#'
#' @examples
#' piformat(c(0.0001,0.001,0.01,0.1,0.5))
#'
#' @export
piformat <- function(x, html=FALSE, tb=TRUE) {
  lt <- c("<", "&lt;")
  res <- x
  res[which(res<0 | res>1)] <- NA
  res[which(res>=0.05)] <- round(res[which(res>=0.05)], 2)
  res[which(res<0.05 & res>=0.001)] <- round(res[which(res<0.05 & res>=0.001)], 3)
  res[which(res<0.001)] <- paste0(lt[html+1], "0.001")
  if (!tb) {
    res[!is.na(res)] <- paste0("p=", res[!is.na(res)])
    res <- gsub(paste0("=", lt[html+1]), lt[html+1], res, fixed=TRUE)
  }
  return(res)
}
