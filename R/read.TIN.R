#' Function to read a TIN from a CHILD file set.
#' 
#' This function reads the nodes- and z-files from a CHILD model run data
#' returns a matrix with m nodes and their x-y-z-coordinates for a specified
#' set and time step of the model run.
#' 
#' The resulting data set is no triangulated irregular network (TIN) object. It
#' is similar in structure to the PTS-file but without boundary flags. However,
#' the data set can serve as input data for a raster interpolation
#' (\code{\link{TIN.raster}}) or external TIN creation (e.g. \code{tri.mesh}
#' from package tripack).
#' 
#' @param dataset CHILD object name.
#' @param timestep Time step from which the node data is read.
#' @return A numeric matrix with x-y-z-coordinates of the nodes.
#' @author Michael Dietze
#' @seealso \code{\link{TIN.raster}}
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
#' # read TIN from time step 10 of data set hillslope1
#' TIN <- read.TIN(hillslope1, timestep = 10)
#' 
#' # show first 5 rows of the dat set
#' TIN[1:5,]
#' 
#' # display the modelled surface and add TIN nodes
#' display.surface(hillslope1, timestep = 10, type = "map")
#' points(TIN[,1], TIN[,2], pch = 4, cex = 0.5)
#' 
#' @export read.TIN
read.TIN <- function(
  dataset,
  timestep
) {
  
  ## read coordinates and return them as matrix
  return(as.matrix(cbind(dataset@nodes[[timestep]][,1:2], 
                         dataset@z[[timestep]]))) 
}