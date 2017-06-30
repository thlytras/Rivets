#' Plot geocoded cases in a KML file
#'
#' epikml() takes a set of coordinates and other associated info as input, and creates
#' a KML (Keyhole Markup Language) file that can be opened with Google Earth or other 
#' similar programs. It's original intention was to plot disease cases, but can find wider 
#' use as well.
#'
#' @param x A numeric vector of longitudes
#' @param y A numeric vector of latitudes
#' @param by A factor, or a list of factors (each of the same length as \code{x} and \code{y})
#'    that are used to group each case in the KML file. 
#' @param name A vector (of same length as \code{x} and \code{y}) with names for each case.
#'    These are printed next to each pin.
#' @param info A vector, or a list of vectors (each of the same length as \code{x} and \code{y})
#'    with extra information about each case. These will be displayed in the pop-up frame that
#'    appears when one clicks on a pin, in a simple tabular format.
#' @param labelinfo A vector of length one or more (equal to the number of vectors in argument
#'    \code{info}), with corresponding labels for each type of information.
#' @param pin A character vector with the color and style for each pin, or a factor whose levels 
#'    correspond to the pin.type argument, which in that case will be a vector of pin colors and 
#'    styles. See "Details".
#' @param pin.type A character vector (of same length as \code{x} and \code{y}) of pin colors and 
#'    styles, which correspond to the factor levels of the argument \code{pin}. If \code{NA}, then 
#'    the argument \code{pin} holds the color and style of each pin.
#' @param file A file name to output the KML into. Otherwise the KML is printed on the screen.
#'
#' @details The following pin styles are recognized: pushpin, paddle, blank, circle, diamond, 
#'    square, stars. Also the following pin colors: blue, green, yellow, white, light blue, pink, 
#'    purple, red. Pin color precedes pin style, e.g. "blue pushpin", "green diamond", etc. 
#'    Alternatively, capital english letters and numbers from 1 to 10 can be specified; these 
#'    create a red paddle pin with the respective letter or number in its centre.
#'
#' @return Nothing. The function prints its output on the screen or on a KML file.
#'
#' @examples
#' # Create some dummy data
#' dat <- data.frame(
#'    lon = c(23.7, 23.8, 23.75, 23.78),
#'    lat = c(38.1, 38, 38.02, 38.07),
#'    nom = c("Case A", "Case B", "Case C", "Case D"),
#'    gender = c("Male", "Male", "Female", "Female"),
#'    died = c(TRUE, FALSE, FALSE, FALSE)
#' )
#' with(dat, epikml(lon, lat, by = gender, name = nom, 
#'    info = list(gender, died), labelinfo=c("Gender", "Died?"),
#'    pin = died, pin.type=c("green paddle", "purple paddle"), 
#'    file="diseaseCases.kml"))
#' # File 'diseaseCases.kml' will be in your working directory.
#' # Open it with Google Earth or a similar program.
#'
#' @export
epikml <- function(x, y, by=NA, name=NA, info=NA, labelinfo=NA, pin="default", pin.type=NULL, file=NA) {
    # Checking arguments for validity
  if (class(x)!="numeric") stop("\"x\" must be numeric")
  if (class(y)!="numeric") stop("\"y\" must be numeric")
  if (length(x)!=length(y)) stop("\"x\" and \"y\" must have same length")
  if (!(length(name)==1 && is.na(name))) {
    name <- as.vector(name)[1:length(x)]
  }
  if (!(length(by)==1 && is.na(by))) {
    if (class(by)!="list") { by <- list(by) }
    by <- lapply(by, factor)
    by.l <- unique(sapply(by, length))
    if (length(by.l)!=1 | by.l!=length(x)) stop("All \"by\" arguments must have same length, equal to the number of rows in the data")
  }
  if (!(length(info)==1 && is.na(info))) {
    if (class(info)!="list") { info <- list(info) }
    info.l <- unique(sapply(info, length))
    if (length(info.l)!=1 | info.l!=length(x)) stop("All \"info\" arguments must have same length, equal to the number of rows in the data")
    labelinfo <- labelinfo[1:length(info)]
    labelinfo[is.na(labelinfo)] <- ""
  }
  if (length(pin)==1) {
    pin <- rep(pin, length(x))
  } else if (length(pin)!=length(x)) {
    stop("\"pin\" must have same length as \"x\" and \"y\"")
  }
  
  # Processing pin styles
  pin <- factor(pin)
  if (!is.null(pin.type)) {
    if (length(pin.type)<length(levels(pin))) stop("Not enough pin.type arguments for pin factor levels")
    levels(pin) <- pin.type[1:length(levels(pin))]
  }
  levels(pin)<-gsub("blue","blu",levels(pin))
  levels(pin)<-gsub("green","grn",levels(pin))
  levels(pin)<-gsub("light ","lt",levels(pin))
  levels(pin)<-gsub("yellow","ylw",levels(pin))
  levels(pin)<-gsub("white","wht",levels(pin))
  levels(pin)<-gsub("paddle","blank",levels(pin))
  levels(pin)<-gsub(" ","-",levels(pin))
  levels(pin)<-gsub("^blu-pushpin","blue-pushpin",levels(pin))
  available_pins <- c(
    as.character(sapply(
      c("pushpin", "blank", "circle", "diamond", "square", "stars"),
      function(x) paste(
	c("blu", "grn", "ltblu", "pink", "purple", "red", "wht", "ylw"),
	x, sep="-")
    )),
    LETTERS, 1:10, "default"
  )
  available_pins[1]<-"blue-pushpin"
  available_pin_urls <- paste("http://maps.google.com/mapfiles/kml/", c(rep("pushpin",8), rep("paddle",77)), "/", c(available_pins[-85], "wht-blank"), ".png", sep="")
  sel_pins<-sapply(levels(pin), function(x) {grep(x,available_pins)[1]})
  sel_pins[is.na(sel_pins)] <- 85   # unrecognized pins are replaced with "default" pin
  levels(pin) <- available_pins[sel_pins]

    # Constructing a data.frame
  data <- data.frame(x=x, y=y, pins=pin)
  if (class(by)=="list") {
    byC = (length(data)+1):(length(data)+length(by))  # Remember the set of columns
    data <- cbind(data,by)
  }
  if (length(name)==nrow(data)) {
    nameC <- length(data)+1  # Remember the column
    data <- cbind(data,name)
  }
  if (class(info)=="list") {
    infoC = (length(data)+1):(length(data)+length(info))  # Remember the set of columns
    data <- cbind(data,info)
  }
  


    # Functions that do all the work
  nodestyle <- function(pin_id, tab=2) {
    cat(rep(tb,tab + 0), '<Style id="', available_pins[pin_id], '">\n', sep="")
    cat(rep(tb,tab + 1), '<IconStyle>\n', sep="")
    cat(rep(tb,tab + 2), '<Icon>\n', sep="")
    cat(rep(tb,tab + 3), '<href>', available_pin_urls[pin_id], '</href>\n', sep="")
    cat(rep(tb,tab + 2), '</Icon>\n', sep="")
    cat(rep(tb,tab + 1), '</IconStyle>\n', sep="")
    cat(rep(tb,tab + 0), '</Style>\n', sep="")  
  }

  nodepoint <- function(element, tab=0) {
    cat(rep(tb,0 + tab), '<Placemark>\n', sep="")
    if (exists("nameC")) {
      cat(rep(tb,1 + tab), '<name>', element[nameC], '</name>\n', sep="")
    }
    if (exists("infoC")) {
      cat(rep(tb,1 + tab), '<ExtendedData>\n', sep="")
      sapply(infoC, function(i){
	cat(rep(tb,2 + tab), '<Data name="', labelinfo[i-infoC[1]+1],'">\n', sep="")
	cat(rep(tb,3 + tab), '<value>', element[i], '</value>\n', sep="")
	cat(rep(tb,2 + tab), '</Data>\n', sep="")
      })
      cat(rep(tb,1 + tab), '</ExtendedData>\n', sep="")
    }
    cat(rep(tb,1 + tab), '<styleUrl>#', element["pins"], '</styleUrl>\n', sep="")
    cat(rep(tb,1 + tab), '<Point>\n', sep="")
    cat(rep(tb,2 + tab), '<coordinates>\n', sep="")
    cat(rep(tb,3 + tab), '',element["x"],',',element["y"],'\n', sep="")
    cat(rep(tb,2 + tab), '</coordinates>\n', sep="")  
    cat(rep(tb,1 + tab), '</Point>\n', sep="")
    cat(rep(tb,0 + tab), '</Placemark>\n', sep="")  
  }
  
  recursiveNode <- function(datasubset, factorCs, namefactorC) {
    if(length(factorCs)==0) {
      # do the actual plotting
      if (nrow(datasubset)>0) {
	cat(rep(tb,2), '<Folder>\n', sep="")
	cat(rep(tb,3), '<name>', as.character(datasubset[1,namefactorC]), '</name>\n', sep="")
	apply(datasubset,1,nodepoint,tab=3)
	cat(rep(tb,2), '</Folder>\n', sep="")
      }
    } else {
      cat(rep(tb,2), '<Folder>\n', sep="")
      cat(rep(tb,3), '<name>', as.character(datasubset[1,namefactorC]), '</name>\n', sep="")
      by(datasubset, datasubset[,factorCs[1]], recursiveNode, factorCs=factorCs[-1], namefactorC=factorCs[1])
      cat(rep(tb,2), '</Folder>\n', sep="")
    }
  }
  
    # Starting output...
  tb <- "  "
  tryCatch({
    sink(file)
    cat('<?xml version="1.0" encoding="utf-8"?>\n')
    cat('<kml xmlns="http://earth.google.com/kml/2.2">\n')
    cat(tb, '<Document>\n', sep="")
      # Putting it all together...
    sapply(unique(sel_pins),nodestyle)
    if (length(by)==1 && is.na(by)) {
      apply(data,1,nodepoint,tab=2)
    } else {
      by(data, data[,byC[1]], recursiveNode, factorCs=byC[-1], namefactorC=byC[1])
    }
    
    cat(tb, '</Document>\n', sep="")
    cat('</kml>\n')

    sink()
  }, error=function(err){
    sink()
    unlink(file)   # If an error occurs while we have sunk our output to the kml file, we must close the sink and remove the file.
    stop(err)
  })
}
