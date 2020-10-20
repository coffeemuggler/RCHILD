\name{write.stream}
\alias{write.stream}
\title{Function to write streams to a shapefile.}
\description{This function creates and writes a shapefile object from traced streams. 
If present, further stream metadata will be exported as shapefile 
attributes as well.}
\usage{write.stream(stream, filename, segments = FALSE, projection)}
\arguments{
  \item{stream}{(list object) Stream object to be exported. This must be a list
object of only one timestep from \code{\link{trace.stream}}. See
examples for how to extract such a list structure.}
  \item{filename}{(character scalar) Name of the output file, with extension.}
  \item{segments}{(logical) Option to create stream individual segments (all lines between 
nodes as separate objects) or merged objects (all lines between nodes as
one stream object), default is FALSE.}
  \item{projection}{The geographic projection of the output file. If not specified, the 
exported data set will not have any reference.}
}

\value{A shapefile with stream courses and thematic stream data.}
\references{CSDMS website. http://csdms.colorado.edu/wiki/Model:CHILD.\cr
Tucker, GE. 2010. CHILD Users Guide for version R9.4.1. 
http://csdms.colorado.edu/mediawiki/images/Child_users_guide.pdf \cr
Tucker, GE., Lancaster, ST., Gasparini, NM., Bras, RL. 2001. The 
Channel-Hillslope Integrated Landscape Development (CHILD) Model. 
In Harmon, RS.,  Doe, W.W. III (eds). Landscape Erosion and Evolution
Modeling. Kluwer Academic/Plenum Publishers, pp. 349-388.}
\author{Michael Dietze}



\seealso{\code{\link{write.raster}}, \code{\link{trace.stream}}}
\examples{
# load example data set
data(hillslope1)

# trace two streams from different starting points through two time steps
startpoints <- cbind(c(50, 100), c(120, 100))
streams <- trace.stream(dataset = hillslope1, 
                        timestep = 2:3, 
                        startpoints = startpoints)

# extract the first time step as source data for shapefile export
streams_export <- streams[[1]]

# write the streams into a shapefile and save it
write.stream(stream = streams_export, 
             filename = "streams.shp", 
             segments = TRUE,
             projection = "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84")
}

\keyword{CHILD}
