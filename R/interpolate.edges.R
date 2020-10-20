#' Function to interpolate edge nodes.
#' 
#' This function identifies the outer boundaries of the modelled surface
#' (boundary flags 1 or 2). These nodes are typically set to zero elevation.
#' The function searches for the closest non-zero node and uses its elevation.
#' 
#' 
#' @param dataset (S4-object) CHILD model run output data set.
#' @return An S4-object with output data of a CHILD model run, but with
#' interpolated z-values of nodes of the surface boundary.
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
#' data(hillslope1b)
#' 
#' # display original surface
#' display.surface(hillslope1b, timestep = 20, type = "wireframe", theta = 90)
#' 
#' # interpolate edges
#' hillslope1bi <- interpolate.edges(hillslope1b)
#' 
#' # display interpolated surface to check the differences
#' display.surface(hillslope1bi, timestep = 20, type = "wireframe", theta = 90)
#' 
#' @export interpolate.edges
interpolate.edges <- function(
  dataset
) {
  
  ## create timestep vector
  timesteps <- length(dataset@timesteps) - 1
  
  ## interpolate boundary node elevation through timesteps
  for(j in 1:timesteps) {
    ## get nodes and elevation from timestep j
    nodes <- dataset@nodes[[j]]
    elevation <- dataset@z[[j]]
    ## create matrix with x-y-z-coordinates and boundary flag
    xyzb <- cbind(nodes[,1:2], elevation, nodes[,4])
    ## identify interiour data
    xyzb_interiour <- xyzb[(xyzb[,4] == 0),]

    ## identify closest elevation values to boundary nodes
    for(i in 1:nrow(xyzb)) {
      if(xyzb[i,4] != 0) {
        distance <- sqrt((xyzb_interiour[,1] - xyzb[i,1])^2 + 
          (xyzb_interiour[,2] - xyzb[i,2])^2)
        xyzb[i,3] <- xyzb_interiour[(distance == min(distance)),3]
      }
    }
    ## write updated boundary node elevation to data set
    dataset@z[[j]] <- xyzb[,3]
  }
  return(dataset)
}