#' Function to get coordinates by mouse.
#' 
#' This function returns the x-y-coordinates of mouseclicks on a plot area
#' (e.g. a map generated by \code{\link{display.surface}}).
#' 
#' 
#' @param n (numeric vector) Number of coordinates to be recorded.
#' @return A numeric matrix with n rows of mouse-click-derived coordinates (x
#' and y in columns).
#' @author Michael Dietze
#' @seealso \code{\link{trace.stream}}, \code{\link{display.surface}}
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
#' # create an empty plot as base for coordinates digitising
#' plot(NA, xlim = c(0, 200), ylim = c(0, 200))
#'   
#' # digitise and show 2 x-y-coordinates by left clicks
#' xy <- click.coordinates(2)
#' xy
#' 
#' @export click.coordinates
click.coordinates <- function(
  n
){
  
  coordinates <- locator(n, type = "p")
  coordinates <- cbind(coordinates$x, coordinates$y)
  
  return(coordinates)
}