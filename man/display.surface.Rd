\name{display.surface}
\alias{display.surface}
\title{Function to display modelled surfaces.}
\description{This function offers three different viaualisation types of surfaces: 
"map" - classic colour-coded grid map, "wireframe" - a perspective plot and 
"scene" - a real time interactive 3D scene (using open GL). The surface 
can carry additional layers such as a hillshade, drainage network, single 
streams, erosion rates etc.}
\usage{display.surface(dataset, timestep, resolution, projection, type, 
    exaggeration, stream = FALSE, startpoints, network = FALSE, 
    area_min, width_scale, width_max, erosion = FALSE, upliftrate, 
    hillshade = FALSE, contours = FALSE, theta, phi, zlim)}
\arguments{
  \item{dataset}{(S4-object) CHILD model run output data set.}
  \item{timestep}{(numeric scalar or vector) The time step for which a surface shall be 
created. Usually, time step is a scalar, specifying only one time step. 
In the case of displaying an erosion rate, not for the actual time step 
and its precursor, but for two other time steps, timestep must be a vector
with two elements: the actual time step t1 (also used for displaying all 
other layers) and the time step t0 (used for the erosion calculation 
only). To display the difference of two elevation surfaces, timestep 
can also have two elements, t1 and t0.}
  \item{resolution}{(numeric scalar) Resolution of the animated scene in metres. If missing, 
the mean node distance of the TIN is used.}
  \item{projection}{(character scalar) Geographic projection of the data set, necessary to  
allow hillshade computation. If no projection is specified a default one 
will be set ("+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"). 
If you are unsure about this parameter see the documentation of package 
"raster" or skip displaying a hillshade. Also, see example for how to 
infer and set projections.}
  \item{type}{(character scalar) Type of visualisation, one out of "map", "wireframe" 
and "scene", default is "map".}
  \item{exaggeration}{(numeric scalar) Vertical exaggeration of the z-values, default is 2.}
  \item{stream}{(logical scalar) Option to add streams, starting at user-defined 
coordinates, to the surface, default is FALSE. If enabled, startpoints 
must be specified.}
  \item{startpoints}{(numeric vector or matrix) Start coordinates of streams that are 
optinally added to the surface. x- and y-coordinates must be provided 
column-wise.}
  \item{network}{(logical scalar) Option to add a drainage network, default is FALSE.}
  \item{area_min}{(numeric scalar) Minimum drainage area from which on a stream network 
is defined. If not specified, all nodes will be treated as stream.}
  \item{width_scale}{(character scalar) Type of line width scaling for drainage area. One 
out of "none" (equal line width), "linear" (linear scaling), "root" 
(square root scaling), "power" (power two scaling), "log" (logarithmic 
scaling), default is "none".}
  \item{width_max}{(numeric scalar) Maximum line width, default is 1.}
  \item{erosion}{(logical scalar) Option to display erosion rates rather than surface 
elevation, default is FALSE.}
  \item{upliftrate}{(numeric scalar or vector) Optional constant uplift rate or uplift 
rates of each node. Necessary for calculating erosion rates, if not 
specified the uplift rate is taken from the CHILD model run output 
data set.}
  \item{hillshade}{(logical scalar) Optional hillshade overlay for type "map", default 
is FALSE.}
  \item{contours}{(logical scalar) Optional contours overlay for type "map", default 
is FALSE.}
  \item{theta}{(numeric scalar) Azimut angle of the view in degree, for type 
"wireframe", default is 30.}
  \item{phi}{(numeric scalar) Colatitude of the view in degree, for type "wireframe",  
default is 30.}
  \item{zlim}{(numeric vector) Optional z-axis limitc vector, defining constant 
minimum and maximum z value for graphical output scale and legend. Z 
generally refers to the elevation data except when type is "map", where 
it may refer the erosion rate, if this option is enabled.}
}
\details{In order to create a hillshade from a grid (option if type is "map") 
the input surface must have a defined projection. See example of 
function \code{\link{animate.surface}} for how to create a projection 
definition.\cr\cr Currently, streams can only be displayed as map 
and wireframe. Let us just wait a few months to see if it is also 
possible to visualise them as scenes, ok?}
\value{A plot object with a graphical representation of a modelled 
surface along with additional optional layers.}
\references{CSDMS website. http://csdms.colorado.edu/wiki/Model:CHILD.\cr
Tucker, GE. 2010. CHILD Users Guide for version R9.4.1. 
http://csdms.colorado.edu/mediawiki/images/Child_users_guide.pdf \cr
Tucker, GE., Lancaster, ST., Gasparini, NM., Bras, RL. 2001. The 
Channel-Hillslope Integrated Landscape Development (CHILD) Model. 
In Harmon, RS.,  Doe, W.W. III (eds). Landscape Erosion and Evolution
Modeling. Kluwer Academic/Plenum Publishers, pp. 349-388.}
\author{Michael Dietze}



\seealso{\code{\link{animate.surface}}, \code{\link{read.CHILD}}}
\examples{
 # load example data set
 data(hillslope1)
 
 # display the hillslope1 model...
 display.surface(dataset = hillslope1, 
                 timestep = 10,                   # at time step 10
                 type = "map",                    # as a map output
                 contours = TRUE,                 # with contours
                 stream = TRUE,                   # and two streams,
                 startpoints = cbind(c(50, 100),  # originating at
                                     c(90, 120)), # these coordinates
                 zlim = c(0, 100))                # and this legend range
 
 # display the hillslope1 model...
  display.surface(dataset = hillslope1, 
                  timestep = 20,                   # at time step 20
                  type = "map",                    # as a map output
                  contours = TRUE,                 # with contours
                  hillshade = TRUE,                # with hillshade overlay
                  erosion = TRUE)                  # and defined value scale

# display the hillslope1 model...
 display.surface(dataset = hillslope1, 
                 timestep = 20,                   # at time step 20
                 type = "wireframe",              # as a wireframe output
                 stream = TRUE,                   # and two streams,
                 startpoints = cbind(c(50, 100),  # originating at
                                     c(90, 120))) # these coordinates
 
 # display the hillslope1 model...
 display.surface(dataset = hillslope1, 
                 timestep = 20,                   # at time step 20
                 type = "wireframe",              # as a wireframe output
                 stream = TRUE,                   # and two streams,
                 startpoints = cbind(c(50, 100),  # originating at
                                     c(90, 120)), # these coordinates
                 theta = 90,                      # with frontal view
                 phi = 20)                        # and lower tilting angle
}

\keyword{CHILD}
