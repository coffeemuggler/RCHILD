\name{CHILD-class}
\Rdversion{1.1}
\docType{class}
\alias{CHILD-class}
\alias{CHILD}
\alias{show,CHILD-method}

\title{Class \code{"CHILD"}}
\description{
A CHILD object comprises all data sets from a model run output. CHILD 
saves a series of ASCII-files in a common directory. A CHILD object can 
be created from a model run output file collection with the function 
\code{\link{read.CHILD}}.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{CHILD(...)}.
}
\section{Slots}{
  \describe{
    \item{\code{summary}:}{Object of class \code{"character"}, A brief
      character summay of the model setup and results.}
    \item{\code{inputs}:}{Object of class \code{"character"}, A record of
      input parameters.}
    \item{\code{timesteps}:}{Object of class \code{"numeric"}, A vector
      of the created time steps, i.e. the layer IDs.}
    \item{\code{pts}:}{Object of class \code{"matrix"}, An optinal matrix
      of xyz-coordinates and boundary flags as input data for surface 
      triangulation in CHILD.}
    \item{\code{upliftmap}:}{Object of class \code{"list"}, An optional 
      list with xy-coordinates of spatially distributed uplift values.}
    \item{\code{layers}:}{Object of class \code{"list"}, An optional 
      list of Layer information for time steps.}
    \item{\code{nodes}:}{Object of class \code{"list"}, A list with 
      xy-coordinates of nodes, the ID of one spoke, and boundary code.}
    \item{\code{edges}:}{Object of class \code{"list"}, A list with 
      IDs of origin and destination nodes, ID of ccw edge.}
    \item{\code{tri}:}{Object of class \code{"list"}, A list with 
      IDs of vertices, neighbor triangles, and edges.}
    \item{\code{random}:}{Object of class \code{"numeric"}, A vector with 
      data for re-starting the random number sequence of mesh generation.}
    \item{\code{varea}:}{Object of class \code{"list"}, A list with 
      Voronoi area of each node in square metres.}
    \item{\code{slp}:}{Object of class \code{"list"}, A list with 
      the gradient of each node in downstream direction.}
    \item{\code{area}:}{Object of class \code{"list"}, A list with 
      the drainage area of each node in square metres.}
    \item{\code{vols}:}{Object of class \code{"numeric"}, A vector with 
      the landscape volume at each storm.}
    \item{\code{dvols}:}{Object of class \code{"numeric"}, A vector with 
      the change in landscape volume over one storm.}
    \item{\code{q}:}{Object of class \code{"list"}, A list with 
      the water discharge at each node in cubic metres per year.}
    \item{\code{net}:}{Object of class \code{"list"}, A list with 
      the ID of each nodes downstream neighbor.}
    \item{\code{storm}:}{Object of class \code{"matrix"}, A matrix with 
      storm data: inter-storm duration in years, storm intensity in metres
      per year and storm duration in years.}
    \item{\code{tau}:}{Object of class \code{"list"}, A list with the 
      shear stress in Pascal for each node.}
    \item{\code{tx}:}{Object of class \code{"list"}, A list with 
      the volumetric proportion of the finest grain size in the top layer.}
    \item{\code{z}:}{Object of class \code{"list"}, A list with node 
      elevations in metres.}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "CHILD")}: ... }
	 }
}
\references{
  CSDMS website. http://csdms.colorado.edu/wiki/Model:CHILD.\cr
  Tucker, GE. 2010. CHILD Users Guide for version R9.4.1. 
  http://csdms.colorado.edu/mediawiki/images/Child_users_guide.pdf \cr
  Tucker, GE., Lancaster, ST., Gasparini, NM., Bras, RL. 2001. The 
  Channel-Hillslope Integrated Landscape Development (CHILD) Model. 
  In Harmon, RS.,  Doe, W.W. III (eds). Landscape Erosion and Evolution 
  Modeling. Kluwer Academic/Plenum Publishers, pp. 349-388.
}
\author{
Michael Dietze
}
\examples{
showClass("CHILD")
}
\keyword{classes}
\usage{
CHILD(...)
}
\arguments{
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
