\name{interpolate.edges}
\alias{interpolate.edges}
\title{Function to interpolate edge nodes.}
\description{This function identifies the outer boundaries of the modelled surface 
(boundary flags 1 or 2). These nodes are typically set to zero elevation. 
The function searches for the closest non-zero node and uses its elevation.}
\usage{interpolate.edges(dataset)}
\arguments{
  \item{dataset}{(S4-object) CHILD model run output data set.}
}

\value{An S4-object with output data of a CHILD model run, but with
interpolated z-values of nodes of the surface boundary.}
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
data(hillslope1b)

# display original surface
display.surface(hillslope1b, timestep = 20, type = "wireframe", theta = 90)

# interpolate edges
hillslope1bi <- interpolate.edges(hillslope1b)

# display interpolated surface to check the differences
display.surface(hillslope1bi, timestep = 20, type = "wireframe", theta = 90)
}

\keyword{CHILD}
