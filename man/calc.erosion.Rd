\name{calc.erosion}
\alias{calc.erosion}
\title{Function to calculate erosion rates.}
\description{This function calculates average erosion rates between specified time 
steps. Optional uplift rates are considered.}
\usage{calc.erosion(dataset, timesteps, upliftrate, coordinates = TRUE)}
\arguments{
  \item{dataset}{(S4-object) CHILD model run output data set.}
  \item{timesteps}{(numeric vector or scalar) ID of the time steps for which the erosion 
rate is calculated, should be specified as c(timestep1, timestep0). If 
only one timestep is provided, the preceeding one (if present) will be 
used automatically.}
  \item{upliftrate}{(numeric vector or scalar) Optional uplift rate(s), if not specified 
the upliftrate is taken from the CHILD model run output data set.}
  \item{coordinates}{(logical scalar) Option to return node coordinates and respective 
erosion rates or only erosion rates, default is TRUE.}
}

\value{If coordinates parameter is set TRUE a numeric matrix with node 
coordinates and average erosion rates, otherwise a vector with 
average erosion rates, only.}
\references{CSDMS website. http://csdms.colorado.edu/wiki/Model:CHILD.\cr
Tucker, GE. 2010. CHILD Users Guide for version R9.4.1. 
http://csdms.colorado.edu/mediawiki/images/Child_users_guide.pdf \cr
Tucker, GE., Lancaster, ST., Gasparini, NM., Bras, RL. 2001. The 
Channel-Hillslope Integrated Landscape Development (CHILD) Model. 
In Harmon, RS.,  Doe, W.W. III (eds). Landscape Erosion and Evolution
Modeling. Kluwer Academic/Plenum Publishers, pp. 349-388.}
\author{Michael Dietze}




\examples{
data(hillslope1) # read data set

# calculate erosion rate between time steps 1 and 6 without uplift
ER <- calc.erosion(hillslope1, timesteps = c(5,1))

# display result in native code
ERplot <- TIN.raster(TIN = ER, resolution = 5)
plot(ERplot, col = rev(heat.colors(250)))

# calculate and display result as map overlay using the package function
display.surface(hillslope1, 
                timestep = c(5, 1), 
                erosion = TRUE, 
                type = "map")
}

\keyword{CHILD}
