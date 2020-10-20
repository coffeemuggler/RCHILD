\name{trace.network}
\alias{trace.network}
\title{Function to trace a stream network.}
\description{This function traces stream network segments with a drainage area
above a certain threshold.}
\usage{trace.network(dataset, timestep, area_min, width_scale, width_max, 
    plot = FALSE)}
\arguments{
  \item{dataset}{(S4-object) CHILD model run output data set.}
  \item{timestep}{(numeric scalar) time step for which the network will be tracved.}
  \item{area_min}{(numeric scalar) Minimum drainage area from which on a stream is defined.
If not specified, all nodes will be treated as stream.}
  \item{width_scale}{(character scalar) Type of line width scaling for drainage area. One 
out of "none" (equal line width), "linear" (linear scaling), "root" 
(square root scaling), "power" (power two scaling), "log" (logarithmic 
scaling), default is "none".}
  \item{width_max}{(numeric scalar) Maximum line width, default is 1.}
  \item{plot}{(logical scalar) Optional plotting of a surface map and the 
stream network overlay, default is FALSE.}
}

\value{A numeric matrix with stream segment information}
\references{CSDMS website. http://csdms.colorado.edu/wiki/Model:CHILD.\cr
Tucker, GE. 2010. CHILD Users Guide for version R9.4.1. 
http://csdms.colorado.edu/mediawiki/images/Child_users_guide.pdf \cr
Tucker, GE., Lancaster, ST., Gasparini, NM., Bras, RL. 2001. The 
Channel-Hillslope Integrated Landscape Development (CHILD) Model. 
In Harmon, RS.,  Doe, W.W. III (eds). Landscape Erosion and Evolution
Modeling. Kluwer Academic/Plenum Publishers, pp. 349-388.}
\author{Michael Dietze}



\seealso{\code{\link{trace.stream}}}
\examples{
# load example data set
data(hillslope1)

# create stream network for...
network <- trace.network(hillslope1,           # model run hillslope1
                         timestep = 5,         # timestep 5
                         area_min = 100,       # streams with > 100 sqm
                         width_scale = "root", # root-scaled line width
                         width_max = 5,        # maximum line width of 5
                         plot = TRUE)          # and plot it

# show the first five rows of the resulting matrix
network[1:5,]
}

\keyword{CHILD}
