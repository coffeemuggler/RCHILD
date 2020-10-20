#' Function to export animated scenes.
#' 
#' This function creates surfaces for each of the specified time steps and uses
#' these as input data for an animation file. This file can be a gif-file,
#' become embedded into a html-document or be a movie file.
#' 
#' To use animation via *.gif, ImageMagick must be installed on the computer
#' (http://www.imagemagick.org/script/index.php). To use animation via *.avi,
#' ffmpeg must be installed on the computer (http://ffmpeg.org/download.html).
#' All files are created and stored in a temporary directory (see console
#' output after function execution) but should be able to be "saved as" in a
#' more convenvient directory.
#' 
#' @param dataset (S4-object) CHILD model run output data set.
#' 
#' @param filename (character scalar) Name of the output file. The extension
#' determines the data type of the visualisation. "*.gif" - gif-animation,
#' "*.html" - animation embedded in a HTML-document, "*.avi" - movie animation
#' in avi-format. The filename must not contain any points, except for the
#' introduction of the file extension. Default filename is "outfile.html".
#' 
#' @param type (character scalar) Type of generated scene, one out of "map" and
#' "wireframe", default is "wireframe".
#' 
#' @param timesteps (numeric vector) Time steps to include to animation. If
#' missing, all modelled time steps are used.
#' 
#' @param interval (numeric scalar) Interval of animation frames in seconds,
#' default is 1.
#' 
#' @param resolution (numeric scalar) Resolution of the animated scene in
#' metres. If missing, the mean node distance of the TIN is used.
#' 
#' @param projection (character scalar) Geographic projection of the data set,
#' necessary to allow hillshade computation. If no projection is specified a
#' default one will be set ("+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100
#' +ellps=WGS84"). If you are unsure about this parameter see the documentation
#' of package "raster" or skip displaying a hillshade. Also, see example for
#' how to infer and set projections.
#' 
#' @param exaggeration (numeric scalar) Vertical exaggeration of the z-values,
#' default is 2.
#' 
#' @param hillshade (logical scalar) Hillshade overlay option for type "map",
#' default is FALSE.
#' 
#' @param contours (logical scalar) Contours overlay option for type "map",
#' default is FALSE.
#' 
#' @param theta (numeric scalar) Azimut angle of the view in degree, for type
#' "wireframe", default is 30.
#' 
#' @param phi (numeric scalar) Colatitude of the view in degree, for type
#' "wireframe", default is 30.
#' 
#' @return A series of images of modelled surfaces that are converted into an
#' animated file.
#' 
#' @author Michael Dietze
#' 
#' @seealso \code{\link{read.CHILD}}, \code{\link{display.surface}},
#' \code{\link{write.raster}}
#' @references CSDMS website. http://csdms.colorado.edu/wiki/Model:CHILD.\cr
#' Tucker, GE. 2010. CHILD Users Guide for version R9.4.1.
#' http://csdms.colorado.edu/mediawiki/images/Child_users_guide.pdf \cr Tucker,
#' GE., Lancaster, ST., Gasparini, NM., Bras, RL. 2001. The Channel-Hillslope
#' Integrated Landscape Development (CHILD) Model.  In Harmon, RS., Doe, W.W.
#' III (eds). Landscape Erosion and Evolution Modeling. Kluwer Academic/Plenum
#' Publishers, pp. 349-388.
#' @keywords CHILD
#' @examples
#' 
#' # uncomment code to run examples
#' 
#' # load example data set
#' data(hillslope1)
#' 
#' # generate a gif-animation, uncomment to run
#' animate.surface(dataset = hillslope1, 
#'                type = "map",
#'                filename = "animation.html", 
#'                interval = 0.1, 
#'                resolution = 1)
#' 
#' # example of how to assess and set the projection of a DEM
#' data(DEM500) # read example DEM
#' DEM <- DEM500
#' 
#' # show (that there is no) projection
#' projection(DEM) 
#' 
#' # set projection
#' projection(DEM)  <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
#' 
#' # show updated projection
#' projection(DEM) 
#' 
#' @export animate.surface
animate.surface <- function(
  dataset,
  filename,
  type = "wireframe",
  timesteps,
  interval,
  resolution,
  projection,
  exaggeration,
  hillshade = FALSE,
  contours = FALSE,
  theta,
  phi
){
  ## load necessary library
  require(animation) 
 
  ## get/set timesteps
  if(missing(timesteps) == TRUE) timesteps <- 1:(
    length(dataset@timesteps) - 1)
 
  ## get/set output file name
  if(missing(filename) == TRUE) filename = "outfile.html"
  
  ## set z-limits
  zlim <- c(min(unlist(dataset@z), 
                na.rm = TRUE), 
            max(unlist(dataset@z), 
                na.rm = TRUE))

  ## get/set raster resolution
  if (missing(resolution) == TRUE) { 
    ## extract node coordinates
    xy <- dataset@nodes[[timesteps[1]]][,1:2]
    ## calculate mean node distance as mean resolution
    resolution <- mean(max(xy[,1], na.rm = TRUE) - 
                         min(xy[,1], na.rm = TRUE), 
                       max(xy[,2], na.rm = TRUE) - 
                         min(xy[,2], na.rm = TRUE)) / 
      sqrt(nrow(xy))
  }
  
  ## get/set projection
  if(missing(projection) == TRUE) projection <- 
    "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
  
  ## get/set animation interval
  if(missing(interval) == TRUE) interval <- 1

  ## get/set exaggeration
  if(missing(exaggeration) == TRUE) exaggeration <- 0.5

  ## get/set wireframe display angles
  if(missing(theta) == TRUE) theta <- 30
  if(missing(phi) == TRUE) phi <- 30

  ## get/set filename extension
  extension <- strsplit(filename, split = ".", fixed = TRUE)[[1]][2]
  
  if(extension == "gif") {
    saveGIF({
      ## Export gif-animation
      for(i in 1:length(timesteps)){
      ## create succesive surface displays
        display.surface(dataset = dataset,
                        timestep = timesteps[i],
                        projection = projection,
                        type = type, 
                        exaggeration = exaggeration, 
                        hillshade = hillshade,
                        contours = contours,
                        theta = theta,
                        phi = phi,
                        zlim = zlim)
      }},
        movie.name = filename,
        interval = interval)
  }
  if(extension == "html") {
    saveHTML({
      ## Export html-animation
      for(i in 1:length(timesteps)){
      ## create succesive surface displays
        display.surface(dataset = dataset,
                        timestep = timesteps[i],
                        projection = projection,
                        type = type, 
                        exaggeration = exaggeration, 
                        hillshade = hillshade,
                        contours = contours,
                        theta = theta,
                        phi = phi,
                        zlim = zlim)
      }
    })
  }
  
  if(extension == "avi") {
    saveVideo({
    ## Export avi-animation
      for(i in 1:length(timesteps)){
      ## create succesive surface displays
        display.surface(dataset = dataset,
                        timestep = timesteps[i],
                        projection = projection,
                        type = type, 
                        exaggeration = exaggeration, 
                        hillshade = hillshade,
                        contours = contours,
                        theta = theta,
                        phi = phi,
                        zlim = zlim)
      }
    },
    video.name = "test.avi",
    interval = interval)
  }  
}