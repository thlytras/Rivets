#' Plot colors from R's color palette
#'
#' This function takes a color name -or more- as argument (e.g. "green") and 
#' displays all the variants of this color that R knows about. This way one 
#' can choose the most aesthetically pleasing colors to use for his/her plot.
#'
#' @param x One or more color names (as a character vector)
#'
#' @details The function matches x to the color names that \code{\link{colors}} 
#'    returns, and then plots nice rectangles each filled with a matching color 
#'    and labelled with the respective color name.
#'
#' @return Nothing
#'
#' @examples
#' plotColors("sea")
#' plotColors(c("green","yellow"))
#'
#' @export
plotColors <- function(x) {
    if (length(x)==1) {
	a <- colors()[grep(x, colors())]
    } else {
	a <- unique(unlist(sapply(x, function(y) colors()[grep(y, colors())])))
    }
    if (length(a)==0) stop("No colors specified!")
    divisor <- ceiling(length(a)/25)
    par(mfrow=c(divisor,1))
    rowlength <- ceiling(length(a)/divisor)
    sapply(seq(1,length(a),by=rowlength), function(i){
	sel=i:(i+rowlength-1)
	par(mar=c(1,1,8,1))
	plot(rep(1, length(sel)), type="h", lend=2, col=a[sel], axes=F, lwd=20*25/rowlength, ylim=c(0,1), ylab=NA, xlab=NA)
	axis(3, at=1:length(sel), label=a[sel], las=2, lwd=0)
    })
    invisible()
}


#' @rdname plotColors
plotColours <- function(x) {
  plotColors(x)
}
