#' Function to interpolate a TIN to a raster.
#' 
#' This function transforms irregular-spaced data into a grid-spaced form, i.e.
#' a \code{raster} object. Currently there are two interpolation methods
#' available: linear interpolation and thin plate spline interpolation (see
#' below).
#' 
#' 
#' @param TIN (numeric matrix) TIN, created by \code{\link{read.TIN}}, i.e. a
#' matrix with m nodes, each represented by its x-y-z-coordinate.
#' 
#' @param resolution (numeric scalar) Target resolution of the raster data set
#' to be created.  If not specified, the mean node spacing is used.
#' 
#' @param method (character scalar) Interpolation method, currently one out of
#' "lin" (linear interpolation), "cub" (cubic spline interpolation) and "tps"
#' (thin plate spline interpolation), default is "lin".
#' 
#' @param theta Optional value for TPS-mode, specifies tapering range. Default
#' value is 10 times the target resolution.
#' 
#' @return A \code{raster} object with interpolated elevation data.
#' 
#' @author Michael Dietze
#' @seealso \code{\link{read.TIN}}, \code{\link{write.raster}}
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
#' # extract TIN of timestep 10 and show first 5 elements
#' TIN10 <- read.TIN(hillslope1, timestep = 10)
#' TIN10[1:5,]
#' 
#' # interpolate TIN to raster by different methods
#' DEM10lin <- TIN.raster(TIN10, resolution = 5)
#' DEM10tps <- TIN.raster(TIN10, resolution = 5, method = "tps")
#' 
#' # plot differences between data sets
#' DEM10diff <- DEM10lin - DEM10tps
#' plot(DEM10diff)
#' 
#' @export TIN.raster
TIN.raster <- function(
  TIN, 
  resolution, 
  method = "lin", 
  theta
) {
  
  require(fields) # load required library
  require(akima) # load required library
  require(raster) # load necessary library
  
  ## check and set raster resolution as mean node spacing
  if (missing(resolution) == TRUE) {
    xy <- TIN[,1:2]
    resolution <- mean(max(xy[,1], na.rm = TRUE) - 
                         min(xy[,1], na.rm = TRUE), 
                       max(xy[,2], na.rm = TRUE) - 
                         min(xy[,2], na.rm = TRUE)) / 
      sqrt(nrow(xy))
  }  
  
  ## linear interpolation to raster
  if (method == "lin") {
    ## interpolation of TIN data to equally spaced data
    DEM <- interp(TIN[,1], TIN[,2], TIN[,3],
             xo=seq(min(TIN[,1]) + resolution / 2, 
                    max(TIN[,1] - resolution / 2), resolution),
             yo=seq(min(TIN[,2]) + resolution / 2, 
                    max(TIN[,2] - resolution / 2), resolution),
             linear = TRUE)
    ## convert data to raster format
    DEM <- raster(DEM) 
    return(DEM) # return raster DEM
  }
  else if (method == "cub") { # cubic interpolation method
    ## interpolation of TIN data to equally spaced data
    DEM <- interp.new(TIN[,1], TIN[,2], TIN[,3],
             xo=seq(min(TIN[,1]) + resolution / 2, 
                    max(TIN[,1] - resolution / 2), resolution),
             yo=seq(min(TIN[,2]) + resolution / 2, 
                    max(TIN[,2] - resolution / 2), resolution),
             linear = FALSE)
    ## convert data to raster format and return it
    DEM <- raster(DEM) 
    return(DEM)
  }
  else if (method == "tps") {
  ## spline interpolation to raster
    ## convert coordinates to data frame
    xy <- data.frame(cbind(TIN[,1], TIN[,2])) 
    ## assign z-value
    z <- TIN[,3]
    ## check/set value for theta
    if(missing(theta) == TRUE) theta <- 10 * resolution 
    ## define interpolation formula
    tps <- fastTps(xy, z, theta = theta) 
    ## determine number of pixels
    n_x <- round((max(TIN[,1]) - min(TIN[,1])) / resolution, 0) 
    n_y <- round((max(TIN[,2]) - min(TIN[,2])) / resolution, 0)
    ## create empty target raster
    DEM <- raster(nrows = n_x, ncols = n_y,
                  xmn = min(TIN[,1]), xmx = max(TIN[,1]), 
                  ymn = min(TIN[,2]), ymx = max(TIN[,2]))
    ## interpolate and return raster
    DEM <- interpolate(DEM, tps)
    return(DEM)
  }
}