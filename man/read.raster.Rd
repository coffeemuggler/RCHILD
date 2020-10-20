\name{read.raster}
\alias{read.raster}
\title{Function to import raster files.}
\description{This function creates a raster-object from different types of input
data set, such as GeoTiff, Erdas Imagine and ESRI.}
\usage{read.raster(dataset, ...)}
\arguments{
  \item{dataset}{(character scalar) Filename of the data set to import.}
  \item{\dots}{Further arguments passed to the function, see \code{\link{raster}}}
}

\value{A raster-object.}

\author{Michael Dietze}



\seealso{\code{\link{write.raster}}}
\examples{
# infer filename of an example data set
filename <- system.file("external/test.grd", package="raster")

# import and plot the example data set as raster object
raster <- read.raster(filename)
plot(raster)
}

\keyword{CHILD}

