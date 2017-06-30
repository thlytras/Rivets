#' Pretty-printer for p-values
#'
#' This function formats p-values in the most usual way, i.e. with two decimals if 
#' p>=0.05, with three decimals if p>=0.001 and p<0.05, and as "p<0.001" if p<0.001.
#' 
#' @param x One or more color names (as a character vector)
#'
#' @return A character vector of equal length to x, with formatted p-values
#'
#' @examples
#' piformat(c(0.0001,0.001,0.01,0.1,0.5))
#'
#' @export
piformat <- function(x, html=FALSE) {
  res <- x
  res[which(res<0 | res>1)] <- NA
  res[which(res>=0.05)] <- round(res[which(res>=0.05)], 2)
  res[which(res<0.05 & res>=0.001)] <- round(res[which(res<0.05 & res>=0.001)], 3)
  res[which(res<0.001)] <- ifelse(html, "p&lt;0.001", "p<0.001")
  res[if (html) (res!="p&lt;0.001") else (res!="p<0.001")] <- paste("p=", res[res!="p<0.001"], sep="")
  return(res)
}
