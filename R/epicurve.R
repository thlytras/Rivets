#' Draw a classic style epidemic curve (with rectangles)
#'
#' epikml() takes a set of coordinates and other associated info as input, and creates
#' a KML (Keyhole Markup Language) file that can be opened with Google Earth or other 
#' similar programs. It's original intention was to plot disease cases, but can find wider 
#' use as well.
#'
#' @param x Data vector of integers or 'Date' objects
#' @param series Factor of equal length as x, used to group cases and color them separately
#' @param col Color, or vector of colors of length equal to the levels of \code{series}.
#' @param xlim Limits for the x axis. Defaults to the range of \code{x}, 
#'    plus/minus \code{xmargin}
#' @param xlab Title for x axis
#' @param ylab Title for y axis
#' @param xaxt x axis type. Specifying \code{xaxt="n"} suppresses plotting of the x axis.
#'    You can then draw it explicitly with \code{link{axis}}.
#' @param yaxt y axis type. Specifying \code{yaxt="n"} suppresses plotting of the y axis.
#'    You can then draw it explicitly with \code{link{axis}}.
#' @param box If FALSE, rectangles are drawn with the appropriate aspect ratio to fit 
#'    the range of both axes. If TRUE, the function adjusts the length of the y axis
#'    so that squares are drawn
#' @param xmargin When drawing the x axis and the limits are not explicitly set, extend
#'    the range of x by this amount and use that as the limits of the x axis.
#'
#' @details Scales are NOT drawn for either axis. Use \code{\link{axis}} to draw them
#'    explicitly to your liking. 
#'
#' @return Nothing. The function plots the epidemic curve on the current graphics device.
#'
#' @examples
#' # Create some dummy data
#' sampdates <- seq(as.Date("2016-07-15"), as.Date("2016-09-15"), 1)
#' x <- sample(sampdates, 120, rep=TRUE)
#' gender <- sample(c("Male","Female"), 120, rep=TRUE)
#' 
#' # Draw the epidemic curve
#' epicurve(x, gender, c("skyblue","pink"))
#'
#' @export
epicurve <- function(x, series=NA, col="green", 
    xlim=range(x, na.rm=TRUE)+xmargin*c(-1,1), xlab=NA, ylab=NA, 
    xaxt="s", yaxt="s", box=FALSE, xmargin = 2) {
  if (length(series)>1) {
    series <- factor(series)
    data <- table(factor(as.integer(x), levels=xlim[1]:xlim[2]), series)
  } else {
    data <- as.matrix(table(factor(as.integer(x), levels=xlim[1]:xlim[2])))
  }
  
  ratio <- ((par("plt")[2]-par("plt")[1])*par("fin")[1])/((par("plt")[4]-par("plt")[3])*par("fin")[2])
  ylimit <- ifelse(box,
    (as.integer(diff(xlim)) + 1.5)/ratio + 0.5,
    ceiling((max(rowSums(data))+2)/5)*5 + 0.5
    )

  plot(0, xaxs="i", yaxs="i", bty="l", xaxt="n", yaxt="n", type="n", 
    xlab=xlab, ylab=ylab, ylim=c(0.5, ylimit), las=2, 
    xlim=as.integer(xlim)+c(-0.5,0.5))

  # My original box drawing function
#   boxi <- function(x,y.from,y.to, col="green") {
#     polygon(c(x-0.5,x+0.5,x+0.5,x-0.5),c(y.from-0.5,y.from-0.5,y.to+0.5,y.to+0.5),col=col)
#     if (y.to-y.from>0) sapply(y.from:(y.to-1),function(j)lines(c(x-0.5,x+0.5),c(j+0.5,j+0.5)))
#   }

  # A quicker version
  boxi <- function(x,y.from,y.to, col="green") {
    polygon(
      rep(c(x-0.5,x+0.5,x+0.5,x-0.5),y.to-y.from+1),
      as.double(sapply(y.from:y.to,function(j)c(j-0.5,j-0.5,j+0.5,j+0.5))),
      col=col
    )
  }
  
  for (i in 1:nrow(data)) {
    offset=0
    for (j in 1:ncol(data)) {
      if (data[i,j]>0) {
	boxi(i+as.integer(xlim[1])-1,1+offset,data[i,j]+offset, col=col[j])
	offset=offset+data[i,j]
      }
    }
  }
  
  if (xaxt!="n") axis(1, at=pretty(x), labels=pretty(x))
  if (yaxt!="n") axis(2)

}
