\name{TIN.raster}
\alias{TIN.raster}
\title{Function to interpolate a TIN to a raster.}
\description{This function transforms irregular-spaced data into a grid-spaced form, 
i.e. a \code{raster} object. Currently there are two interpolation methods 
available: linear interpolation and thin plate spline interpolation
(see below).}
\usage{TIN.raster(TIN, resolution, method = "lin", theta)}
\arguments{
  \item{TIN}{(numeric matrix) TIN, created by \code{\link{read.TIN}}, i.e. a matrix 
with m nodes, each represented by its x-y-z-coordinate. }
  \item{resolution}{(numeric scalar) Target resolution of the raster data set to be created. 
If not specified, the mean node spacing is used.}
  \item{method}{(character scalar) Interpolation method, currently one out of "lin" 
(linear interpolation), "cub" (cubic spline interpolation) and "tps" 
(thin plate spline interpolation), default is "lin".}
  \item{theta}{Optional value for TPS-mode, specifies tapering range. Default
value is 10 times the target resolution.}
}

\value{A \code{raster} object with interpolated elevation data.}
\references{CSDMS website. http://csdms.colorado.edu/wiki/Model:CHILD.\cr
Tucker, GE. 2010. CHILD Users Guide for version R9.4.1. 
http://csdms.colorado.edu/mediawiki/images/Child_users_guide.pdf \cr
Tucker, GE., Lancaster, ST., Gasparini, NM., Bras, RL. 2001. The 
Channel-Hillslope Integrated Landscape Development (CHILD) Model. 
In Harmon, RS.,  Doe, W.W. III (eds). Landscape Erosion and Evolution
Modeling. Kluwer Academic/Plenum Publishers, pp. 349-388.}
\author{Michael Dietze}



\seealso{\code{\link{read.TIN}}, \code{\link{write.raster}}}
\examples{
# load example data set
data(hillslope1)

# extract TIN of timestep 10 and show first 5 elements
TIN10 <- read.TIN(hillslope1, timestep = 10)
TIN10[1:5,]

# interpolate TIN to raster by different methods
DEM10lin <- TIN.raster(TIN10, resolution = 5)
DEM10tps <- TIN.raster(TIN10, resolution = 5, method = "tps")

# plot differences between data sets
DEM10diff <- DEM10lin - DEM10tps
plot(DEM10diff)
}

\keyword{CHILD}
