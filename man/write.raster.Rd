\name{write.raster}
\alias{write.raster}
\title{Function to export CHILD layers to raster.}
\description{This function interpolates layers created by a CHILD run (e.g. elevation) 
and writes the result into one of the following raster formats: *.tiff, 
*.img, *.asc.}
\usage{write.raster(dataset, filename, timestep, layer = "elevation", 
    resolution, projection, method = "lin", upliftrate)}
\arguments{
  \item{dataset}{(S4-object) CHILD model run to export.}
  \item{filename}{Filename of the output data set. The extension determines the data type
and must be one of the following: "*.tiff" - geotiff, "*.img" - erdas 
image, "*.asc" - ESRI raster file.}
  \item{timestep}{(numeric scalar or vector) The time step for which a surface shall be 
created. If timestep is a scalar, the raster will either the surface of 
this time step (if layer = "elevation") or the erosion rate between this 
time step and its precursor (if layer = "erosion"). If timestep is a 
vector (t1, t0), the raster will either be the difference of the 
surfaces of the two time steps (t1 - t0, if layer = "elevation") or 
the erosion rate between the two time steps (t0 to t1, if layer = 
"erosion").}
  \item{layer}{The layer type to export. Must be one of the following: "elevation" - 
(surface elevation) and "erosion" (erosion rate).}
  \item{resolution}{The spatial resolution of the output file. If not specified the mean 
node distance of the CHILD TIN is used.}
  \item{projection}{The geographic projection of the output file. If not specified, the 
exported raster data set will not have any reference.}
  \item{method}{Interpolation method for raster generation, default is "lin" ( 
cf. \code{\link{TIN.raster}}).}
  \item{upliftrate}{(numeric vector or scalar) Optional uplift rate(s) for erosion rate
calculation, default is zero.}
}

\value{A raster data set of the specified file type, stored in the
working directory path.}
\references{CSDMS website. http://csdms.colorado.edu/wiki/Model:CHILD.\cr
Tucker, GE. 2010. CHILD Users Guide for version R9.4.1. 
http://csdms.colorado.edu/mediawiki/images/Child_users_guide.pdf \cr
Tucker, GE., Lancaster, ST., Gasparini, NM., Bras, RL. 2001. The 
Channel-Hillslope Integrated Landscape Development (CHILD) Model. 
In Harmon, RS.,  Doe, W.W. III (eds). Landscape Erosion and Evolution
Modeling. Kluwer Academic/Plenum Publishers, pp. 349-388.}
\author{Michael Dietze}



\seealso{\code{\link{display.surface}}}
\examples{
# load example data set
data(hillslope1)

# export elevation of model hillslope1 at time step 5 as geotiff
write.raster(dataset = hillslope1,
             filename = "hillslope1_5.tiff",
             timestep = c(5, 1),
             layer = "elevation",
             resolution = 2)
}

\keyword{CHILD}

