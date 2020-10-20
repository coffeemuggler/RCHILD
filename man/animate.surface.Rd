\name{animate.surface}
\alias{animate.surface}
\title{Function to export animated scenes.}
\description{This function creates surfaces for each of the specified time steps and 
uses these as input data for an animation file. This file can be a 
gif-file, become embedded into a html-document or be a movie file.}
\usage{animate.surface(dataset, filename, type = "wireframe", timesteps, 
    interval, resolution, projection, exaggeration, hillshade = FALSE, 
    contours = FALSE, theta, phi)}
\arguments{
  \item{dataset}{(S4-object) CHILD model run output data set.}
  \item{filename}{(character scalar) Name of the output file. The extension determines 
the data type of the visualisation. "*.gif" - gif-animation, "*.html" 
- animation embedded in a HTML-document, "*.avi" - movie animation in 
avi-format. The filename must not contain any points, except for the 
introduction of the file extension. Default filename is "outfile.html".}
  \item{type}{(character scalar) Type of generated scene, one out of "map" and 
"wireframe", default is "wireframe".}
  \item{timesteps}{(numeric vector) Time steps to include to animation. If missing, all 
modelled time steps are used.}
  \item{interval}{(numeric scalar) Interval of animation frames in seconds, default is 1.}
  \item{resolution}{(numeric scalar) Resolution of the animated scene in metres. If missing, 
the mean node distance of the TIN is used.}
  \item{projection}{(character scalar) Geographic projection of the data set, necessary to 
allow hillshade computation. If no projection is specified a default 
one will be set ("+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 
+ellps=WGS84"). If you are unsure about this parameter see the 
documentation of package "raster" or skip displaying a hillshade. Also, 
see example for how to infer and set projections.}
  \item{exaggeration}{(numeric scalar) Vertical exaggeration of the z-values, default is 2.}
  \item{hillshade}{(logical scalar) Hillshade overlay option for type "map", default 
is FALSE.}
  \item{contours}{(logical scalar) Contours overlay option for type "map", default 
is FALSE.}
  \item{theta}{(numeric scalar) Azimut angle of the view in degree, for type 
"wireframe", default is 30.}
  \item{phi}{(numeric scalar) Colatitude of the view in degree, for type "wireframe",
default is 30.}
}
\details{To use animation via *.gif, ImageMagick must be installed on the 
computer (http://www.imagemagick.org/script/index.php). To use 
animation via *.avi, ffmpeg must be installed on the computer 
(http://ffmpeg.org/download.html). All files are created and stored
in a temporary directory (see console output after function execution) 
but should be able to be "saved as" in a more convenvient directory.}
\value{A series of images of modelled surfaces that are converted into 
an animated file.}
\references{CSDMS website. http://csdms.colorado.edu/wiki/Model:CHILD.\cr
Tucker, GE. 2010. CHILD Users Guide for version R9.4.1. 
http://csdms.colorado.edu/mediawiki/images/Child_users_guide.pdf \cr
Tucker, GE., Lancaster, ST., Gasparini, NM., Bras, RL. 2001. The 
Channel-Hillslope Integrated Landscape Development (CHILD) Model. 
In Harmon, RS.,  Doe, W.W. III (eds). Landscape Erosion and Evolution
Modeling. Kluwer Academic/Plenum Publishers, pp. 349-388.}
\author{Michael Dietze}



\seealso{\code{\link{read.CHILD}}, 
\code{\link{display.surface}}, 
\code{\link{write.raster}}}
\examples{
# uncomment code to run examples

# load example data set
data(hillslope1)

# generate a gif-animation, uncomment to run
animate.surface(dataset = hillslope1, 
               type = "map",
               filename = "animation.html", 
               interval = 0.1, 
               resolution = 1)

# example of how to assess and set the projection of a DEM
data(DEM500) # read example DEM
DEM <- DEM500

# show (that there is no) projection
projection(DEM) 

# set projection
projection(DEM)  <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"

# show updated projection
projection(DEM) 
}

\keyword{CHILD}

