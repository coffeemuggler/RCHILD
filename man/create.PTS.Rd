\name{create.PTS}
\alias{create.PTS}
\title{Function to create a PTS matrix.}
\description{CHILD can be run with a user-defined surface layer (mesh) in the
pts-format, i.e. a matrix with m nodes and 4 variables 
(x-y-z-coordinates and the boundary flag). The function creates 
a mesh of specified geometry, performs a spatial overlay of nodes 
and DEM and sets the boundary flags (see details).}
\usage{create.PTS(DEM, extent, spacing, type, coordinates, boundary)}
\arguments{
  \item{DEM}{(\code{SpatialGridDataFrame} or \code{raster}) Elevation data set. 
See examples for important constraints.}
  \item{extent}{(numeric vector) Spatial extent of the resulting PTS file, 
boundaries must be given in the order west, east, south, north. 
If not specified, the extent of the DEM will be used.}
  \item{spacing}{(numeric scalar) Spacing between nodes, in the case of random 
nodes mean spacing.}
  \item{type}{(character scalar) PTS arrangement type, one of the following
is allowed: "randcomp", (completely random), "randpart" (partly 
random), "raster" (raster spacing), "hexagonal" (hexagonal 
spacing), "coordinates", (user-defined coordinates), default is
"hexagonal".}
  \item{coordinates}{(numeric matrix) Optional user-defined x-y-coordinates of 
locations where nodes will be created.}
  \item{boundary}{(character scalar) Way of how to set the boundary flags of the
mesh margin. Must be one of the following: "open" (all nodes open),
"closed" (all nodes closed), "lowest" (all nodes closed but the 
lowest one, i.e. assumed outlet), default is "open".}
}
\details{The type option "randpart" (partly random) is defined as in the CHILD 
users guide, i.e. as uniform random deviation of 0-25 \% from the 
nominal node spacing. \cr
Boundary flags are a mandatory item of the pts-file. The function sets 
flags so that all nodes are "interiour" (i.e. 0) except for those 
forming the spatial boundary of the node cloud, i.e. the convex hull. 
Depending on the value of the argument boundary, such marginal nodes 
can take different boundary flags. If boundary is set to "closed", 
all flags will be set to closed (1). This means there is no outlet 
present in the surface model. However, CHILD will report an error if 
it cannot find at least one outlet. If this is not intended, the user
must change this setting manually (cf. example). If boundary is set to 
"open", all boundary flags will be set to open (2). This is the default 
since in this case the model will establish the "real" drainage networks
by itself. If boundary is set to "lowest", all boundary flags will be 
set to closed except for the node with the lowest elevation, which is 
then assumend to represent the natural outlet. Again, in any case of 
erroneous flag settings, the user may change the flags manually, either
by directly identifying the respective node (cf. example) or e.g. by 
spatial querrying (choose coordinates that fall into a defined boundary
box).\cr
The elevation data set should be free of NA values, because if a node 
falls into pixel with NA it will not be kept in the resulting data set. 
This is especially relevant for nodes on the convex hull. If the 
elevation data set contains a geographic projection, this will be 
removed (only inside the function) to ensure correct node spacing.}
\value{A numeric matrix with n rows of nodes and 4 columns, comprising the 
x-y-z-coordinates of the created mesh nodes and respective boundary
flags.}
\references{CSDMS website. http://csdms.colorado.edu/wiki/Model:CHILD.\cr
Tucker, GE. 2010. CHILD Users Guide for version R9.4.1. 
http://csdms.colorado.edu/mediawiki/images/Child_users_guide.pdf \cr
Tucker, GE., Lancaster, ST., Gasparini, NM., Bras, RL. 2001. The 
Channel-Hillslope Integrated Landscape Development (CHILD) Model. 
In Harmon, RS.,  Doe, W.W. III (eds). Landscape Erosion and Evolution
Modeling. Kluwer Academic/Plenum Publishers, pp. 349-388.}
\author{Michael Dietze}



\seealso{\code{\link{write.PTS}}, \code{\link{run.CHILD}}, \code{\link{read.TIN}}}
\examples{
# load example data set
data(DEM500)
DEM <- DEM500
  
## assign PTS extent, just 1000 m smaller than DEM
extent <- c(DEM@extent@xmin + 1000, 
            DEM@extent@xmax - 1000, 
            DEM@extent@ymin + 1000, 
            DEM@extent@ymax - 1000)

# create and show a completely random PTS data set
PTS1 <- create.PTS(DEM = DEM, 
                   extent = extent, 
                   spacing = 5000, 
                   type = "randcomp", 
                   boundary = "open")
plot(DEM)
points(PTS1[,1], PTS1[,2], col = PTS1[,4] + 1)  

# create and show a partly random PTS data set
PTS2 <- create.PTS(DEM = DEM, 
                   extent = extent, 
                   spacing = 5000, 
                   type = "randpart", 
                   boundary = "closed")
plot(DEM)
points(PTS2[,1], PTS2[,2], col = PTS2[,4] + 1)

# create and show a raster PTS data set
PTS3 <- create.PTS(DEM = DEM, 
                   extent = extent, 
                   spacing = 5000, 
                   type = "raster", 
                   boundary = "lowest")
plot(DEM)
points(PTS3[,1], PTS3[,2], col = PTS3[,4] + 1)
# note the outlet node in the southeastern corner

# to correct this situation, add another outlet in the north
# first, plot node number spatially to identify lowest northern node
plot(DEM)
text(x = PTS3[,1], 
     y = PTS3[,2],
     labels = seq(1, nrow(PTS3)),
     cex = 0.5)

# next, change boundary flag of lowest northern node
PTS3[176, 4] <- 2

# finally, plot new node cloud
plot(DEM)
points(PTS3[,1], PTS3[,2], col = PTS3[,4] + 1)

# create and show a hexagonal PTS data set
PTS4 <- create.PTS(DEM = DEM, 
                   extent = extent, 
                   spacing = 5000, 
                   type = "hexagonal", 
                   boundary = "open")
plot(DEM)
points(PTS4[,1], PTS4[,2], col = PTS4[,4] + 1)

# create and show a PTS data set from user-defined coordinates
x <- runif(225, extent[1], extent[2]) # some arbitrary x-coordinates
y <- runif(225, extent[3], extent[4]) # some arbitrary y-coordinates
coordinates <- cbind(x, y) # a matrix with the x-y-coordinates

PTS5 <- create.PTS(DEM = DEM, 
                   extent = extent, 
                   type = "coordinates", 
                   coordinates = coordinates,
                   boundary = "open")
plot(DEM)
points(PTS5[,1], PTS5[,2], col = PTS5[,4] + 1)
}

\keyword{CHILD}
