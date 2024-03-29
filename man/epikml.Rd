% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/epikml.R
\name{epikml}
\alias{epikml}
\title{Plot geocoded cases in a KML file}
\usage{
epikml(
  x,
  y,
  by = NA,
  name = NA,
  info = NA,
  labelinfo = NA,
  pin = "default",
  pin.type = NULL,
  file = NA
)
}
\arguments{
\item{x}{A numeric vector of longitudes}

\item{y}{A numeric vector of latitudes}

\item{by}{A factor, or a list of factors (each of the same length as \code{x} and \code{y})
that are used to group each case in the KML file.}

\item{name}{A vector (of same length as \code{x} and \code{y}) with names for each case.
These are printed next to each pin.}

\item{info}{A vector, or a list of vectors (each of the same length as \code{x} and \code{y})
with extra information about each case. These will be displayed in the pop-up frame that
appears when one clicks on a pin, in a simple tabular format.}

\item{labelinfo}{A vector of length one or more (equal to the number of vectors in argument
\code{info}), with corresponding labels for each type of information.}

\item{pin}{A character vector with the color and style for each pin, or a factor whose levels 
correspond to the pin.type argument, which in that case will be a vector of pin colors and 
styles. See "Details".}

\item{pin.type}{A character vector (of same length as \code{x} and \code{y}) of pin colors and 
styles, which correspond to the factor levels of the argument \code{pin}. If \code{NA}, then 
the argument \code{pin} holds the color and style of each pin.}

\item{file}{A file name to output the KML into. Otherwise the KML is printed on the screen.}
}
\value{
Nothing. The function prints its output on the screen or on a KML file.
}
\description{
epikml() takes a set of coordinates and other associated info as input, and creates
a KML (Keyhole Markup Language) file that can be opened with Google Earth or other 
similar programs. It's original intention was to plot disease cases, but can find wider 
use as well.
}
\details{
The following pin styles are recognized: pushpin, paddle, blank, circle, diamond, 
   square, stars. Also the following pin colors: blue, green, yellow, white, light blue, pink, 
   purple, red. Pin color precedes pin style, e.g. "blue pushpin", "green diamond", etc. 
   Alternatively, capital english letters and numbers from 1 to 10 can be specified; these 
   create a red paddle pin with the respective letter or number in its centre.
}
\examples{
# Create some dummy data
dat <- data.frame(
   lon = c(23.7, 23.8, 23.75, 23.78),
   lat = c(38.1, 38, 38.02, 38.07),
   nom = c("Case A", "Case B", "Case C", "Case D"),
   gender = c("Male", "Male", "Female", "Female"),
   died = c(TRUE, FALSE, FALSE, FALSE)
)
with(dat, epikml(lon, lat, by = gender, name = nom, 
   info = list(gender, died), labelinfo=c("Gender", "Died?"),
   pin = died, pin.type=c("green paddle", "purple paddle"), 
   file="diseaseCases.kml"))
# File 'diseaseCases.kml' will be in your working directory.
# Open it with Google Earth or a similar program.

}
