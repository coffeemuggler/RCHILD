\name{DEM500}
\alias{DEM500}
\docType{data}
\title{Example digital elevation model data set}
\description{This data set contains a digital elevation model (DEM) data set of 
             the eastern Erzgebirge region with 500 m spatial resolution. 
             The data set is a raster object and was imported from the ESRI 
             ascii format, with 139 columns and 143 rows. The DEM is
             projected with UTM zone 33N. It was created from an ASTER GDEM
              data set, resampled to a pixel size of 250 m.}
\usage{data(DEM500)}
\format{
  The format is:
  
ncols         139\cr
nrows         143\cr
xllcorner     361217.04418762\cr
yllcorner     5605428.9851945\cr
cellsize      500\cr
NODATA_value  -9999\cr
-9999 -9999 -9999 -9999 -9999 -9999 -9999 -9999 -9999...\cr
-9999 -9999 -9999 -9999 -9999 -9999 -9999 -9999 -9999...\cr
-9999 -9999 -9999 -9999 -9999 -9999 -9999 -9999 -9999...\cr
-9999 -9999 -9999 -9999 -9999 -9999 -9999 -9999 -9999...\cr
}
\source{ASTER GDEM. http://asterweb.jpl.nasa.gov/gdem.asp}
\examples{
data(DEM500)
}
\keyword{datasets}
