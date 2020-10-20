#' Function to import raster files.
#' 
#' This function creates a raster-object from different types of input data
#' set, such as GeoTiff, Erdas Imagine and ESRI.
#' 
#' 
#' @param dataset (character scalar) Filename of the data set to import.
#' @param \dots Further arguments passed to the function, see
#' \code{\link{raster}}
#' @return A raster-object.
#' @author Michael Dietze
#' @seealso \code{\link{write.raster}}
#' @keywords CHILD
#' @examples
#' 
#' # infer filename of an example data set
#' filename <- system.file("external/test.grd", package="raster")
#' 
#' # import and plot the example data set as raster object
#' raster <- read.raster(filename)
#' plot(raster)
#' 
#' @export read.raster
read.raster <- function(
  dataset,
  ...
){
  
  require(raster)
  
  raster(dataset, ...)
}