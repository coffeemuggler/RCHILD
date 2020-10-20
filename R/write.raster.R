#' Function to export CHILD layers to raster.
#' 
#' This function interpolates layers created by a CHILD run (e.g. elevation)
#' and writes the result into one of the following raster formats: *.tiff,
#' *.img, *.asc.
#' 
#' 
#' @param dataset (S4-object) CHILD model run to export.
#' 
#' @param filename Filename of the output data set. The extension determines
#' the data type and must be one of the following: "*.tiff" - geotiff, "*.img"
#' - erdas image, "*.asc" - ESRI raster file.
#' 
#' @param timestep (numeric scalar or vector) The time step for which a surface
#' shall be created. If timestep is a scalar, the raster will either the
#' surface of this time step (if layer = "elevation") or the erosion rate
#' between this time step and its precursor (if layer = "erosion"). If timestep
#' is a vector (t1, t0), the raster will either be the difference of the
#' surfaces of the two time steps (t1 - t0, if layer = "elevation") or the
#' erosion rate between the two time steps (t0 to t1, if layer = "erosion").
#' 
#' @param layer The layer type to export. Must be one of the following:
#' "elevation" - (surface elevation) and "erosion" (erosion rate).
#' 
#' @param resolution The spatial resolution of the output file. If not
#' specified the mean node distance of the CHILD TIN is used.
#' 
#' @param projection The geographic projection of the output file. If not
#' specified, the exported raster data set will not have any reference.
#' 
#' @param method Interpolation method for raster generation, default is "lin" (
#' cf. \code{\link{TIN.raster}}).
#' 
#' @param upliftrate (numeric vector or scalar) Optional uplift rate(s) for
#' erosion rate calculation, default is zero.
#' 
#' @return A raster data set of the specified file type, stored in the working
#' directory path.
#' 
#' @author Michael Dietze
#' @seealso \code{\link{display.surface}}
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
#' # load example data set
#' data(hillslope1)
#' 
#' # export elevation of model hillslope1 at time step 5 as geotiff
#' write.raster(dataset = hillslope1,
#'              filename = "hillslope1_5.tiff",
#'              timestep = c(5, 1),
#'              layer = "elevation",
#'              resolution = 2)
#' 
#' @export write.raster
write.raster <- function(
  dataset,
  filename,
  timestep,
  layer = "elevation",
  resolution,
  projection,
  method = "lin",
  upliftrate
){
  
  ## load necessary libraries
  require(raster)
  
  ## check timestep structure and assign t1 and t0 (optional)
  if(length(timestep) == 1) {
    timestep1 <- timestep
    timestep0 <- ifelse(timestep < 1, 1, timestep1 - 1)}
  if(length(timestep) == 2) {
    timestep1 <- timestep[1]
    timestep0 <- timestep[2]
    if(timestep1 == 1) {timestep0 <- 1}
  } 
  
  ## check and optinally set output raster resoltuion
  if (missing(resolution) == TRUE) { 
    ## extract node coordinates
    xy <- dataset@nodes[[timestep]][,1:2]
    ## set mean resolution from mean node distance
    resolution <- mean(max(xy[,1], na.rm = TRUE) - 
                         min(xy[,1], na.rm = TRUE),
                       max(xy[,2], na.rm = TRUE) - 
                         min(xy[,2], na.rm = TRUE)) / 
      sqrt(nrow(xy))
  }
  
  ## extract filename extension to set export file type
  extension <- strsplit(filename, split = ".", fixed = TRUE)[[1]][2]
  extin <- c("tiff", "img", "ascii")
  extout <- c("GTiff", "HFA", "ascii")
  if(sum(extension == c("tiff", "img", "ascii")) > 0) {
    format <- extout[extension == extin]
  } else stop ("Output data type not defined.")
  
  ## export elevation raster
  if(layer == "elevation" & length(timestep == 1)) {
    ## read TIN from CHILD data set
    TIN <- read.TIN(dataset, timestep1)
    ## interpolate TIN to raster
    surface <- TIN.raster(TIN, 
                          resolution = resolution, 
                          method = method)
    ## set projection
    if(missing(projection) == TRUE) projection <- NA
    projection(surface) <- projection
    ## write raster data set
    writeRaster(x = surface, 
                filename = filename, 
                format = format)
  } else if(layer == "elevation" & length(timestep) == 2) {
  ## export elevation difference raster
    ## read TIN from CHILD data set
    TIN1 <- read.TIN(dataset, timestep1)
    TIN0 <- read.TIN(dataset, timestep0)
    ## interpolate TINs to raster and calculate difference
    surface1 <- TIN.raster(TIN1, 
                           resolution = resolution, 
                           method = method)
    surface0 <- TIN.raster(TIN0, 
                           resolution = resolution, 
                           method = method)
    surface <- surface0 - surface1
    ## set projection
    if(missing(projection) == TRUE) projection <- NA
    projection(surface) <- projection
    ## write raster data set
    writeRaster(x = surface, 
                filename = filename, 
                format = format)
  } else if(layer == "erosion") {
  ## export erosion rate raster
    ## calculate erosion data set
    ER <- calc.erosion(dataset, 
                       timesteps = c(timestep0, timestep1), 
                       upliftrate = upliftrate)
    ## interpolate erosion data set to raster
    surface <- TIN.raster(ER, 
                          resolution = resolution, 
                          method = method)
    ## set projection
    if(missing(projection) == TRUE) projection <- NA
    projection(surface) <- projection
    ## write raster data set
    writeRaster(x = surface, 
                filename = filename, 
                format = format)
  } else stop("Layer type not supported")
}