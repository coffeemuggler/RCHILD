\name{trace.stream}
\alias{trace.stream}
\title{Function to trace stream length profiles.}
\description{This function traces stream profiles, initiating at user-defined starting
points, downslope until an open boundary is reached. If several start point 
coordinates are specified, multiple stream profile objects are generated.  
If several time slices are specified, also several stream profiles are 
generated.}
\usage{trace.stream(dataset, timestep, startpoints)}
\arguments{
  \item{dataset}{(S4-object) CHILD model run output data set.}
  \item{timestep}{(numeric scalar or vector) Time step(s) for which stream profiles are 
generated.}
  \item{startpoints}{(numeric vector or matrix) Start point coordinates or node IDs for stream 
profile tracing.}
}

\value{A list object (code{streams[[timestep]][[stream]]$parameter}) 
with subsequent list objects, arranged by time steps and streams.  
Each subsequent list object contains the following stream data: \cr
length (numeric vector) - stream distance from start point \cr
elevation (list) - elevation of the stream nodes \cr
slope (list) - slope of the stream nodes \cr
area (list) - drainage area of the stream nodes \cr
x (numeric vector) - x-coordinates of the stream nodes \cr
y (numeric vector) - y-coordinates of the stream nodes \cr
nodes (numeric vector) - ID of the stream nodes}
\references{CSDMS website. http://csdms.colorado.edu/wiki/Model:CHILD.\cr
Tucker, GE. 2010. CHILD Users Guide for version R9.4.1. 
http://csdms.colorado.edu/mediawiki/images/Child_users_guide.pdf \cr
Tucker, GE., Lancaster, ST., Gasparini, NM., Bras, RL. 2001. The 
Channel-Hillslope Integrated Landscape Development (CHILD) Model. 
In Harmon, RS.,  Doe, W.W. III (eds). Landscape Erosion and Evolution
Modeling. Kluwer Academic/Plenum Publishers, pp. 349-388.}
\author{Michael Dietze}



\seealso{\code{\link{display.surface}}, \code{\link{click.coordinates}}}
\examples{
# load example data set
data(hillslope1)

# set function parameters
timesteps <- c(5:15) # timesteps through which streams are traced
startpoints <- cbind(c(50, 100), c(120, 100)) # start coordinates

# trace two streams through timesteps 10 to 20
X <- trace.stream(dataset = hillslope1, 
                  timestep = timesteps, 
                  startpoints = startpoints)

# show structure of X
str(X)

# plot the streams on a surface map
display.surface(hillslope1, timestep = 10, type = "map")
lines(X[[1]][[1]]$x_coord, X[[1]][[1]]$y_coord, col = 4)
lines(X[[1]][[2]]$x_coord, X[[1]][[2]]$y_coord, col = 4)

# plot stream 2 course over time on a surface map (meaningless)
display.surface(hillslope1, timestep = 10, type = "map")
for(i in 1:10) {lines(X[[i]][[2]]$x_coord, X[[i]][[2]]$y_coord, col = 4)}

# plot length profiles of stream 1 through time
plot(X[[1]][[1]]$length, X[[1]][[1]]$elevation, 
     type = "l", 
     ylim = c(0, 50),
     xlab = "stream length",
     ylab = "elevation")
for(i in 2:10) {lines(X[[i]][[1]]$length, X[[i]][[1]]$elevation)}
}

\keyword{CHILD}
