\name{read.TIN}
\alias{read.TIN}
\title{Function to read a TIN from a CHILD file set.}
\description{This function reads the nodes- and z-files from a CHILD model run data 
returns a matrix with m nodes and their x-y-z-coordinates for a specified
set and time step of the model run.}
\usage{read.TIN(dataset, timestep)}
\arguments{
  \item{dataset}{CHILD object name.}
  \item{timestep}{Time step from which the node data is read.}
}
\details{The resulting data set is no triangulated irregular network (TIN)
object. It is similar in structure to the PTS-file but without 
boundary flags. However, the data set can serve as input data for
a raster interpolation (\code{\link{TIN.raster}}) or external TIN
creation (e.g. \code{tri.mesh} from package tripack).}
\value{A numeric matrix with x-y-z-coordinates of the nodes.}
\references{CSDMS website. http://csdms.colorado.edu/wiki/Model:CHILD.\cr
Tucker, GE. 2010. CHILD Users Guide for version R9.4.1. 
http://csdms.colorado.edu/mediawiki/images/Child_users_guide.pdf \cr
Tucker, GE., Lancaster, ST., Gasparini, NM., Bras, RL. 2001. The 
Channel-Hillslope Integrated Landscape Development (CHILD) Model. 
In Harmon, RS.,  Doe, W.W. III (eds). Landscape Erosion and Evolution
Modeling. Kluwer Academic/Plenum Publishers, pp. 349-388.}
\author{Michael Dietze}



\seealso{\code{\link{TIN.raster}}}
\examples{
# load example data set
data(hillslope1)

# read TIN from time step 10 of data set hillslope1
TIN <- read.TIN(hillslope1, timestep = 10)

# show first 5 rows of the dat set
TIN[1:5,]

# display the modelled surface and add TIN nodes
display.surface(hillslope1, timestep = 10, type = "map")
points(TIN[,1], TIN[,2], pch = 4, cex = 0.5)
}

\keyword{CHILD}
