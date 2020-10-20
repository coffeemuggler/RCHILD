#' Function to write a pts-file.
#' 
#' The function writes a pts-file from a PTS object (e.g. created by
#' create.PTS) as input file for a CHILD model run.
#' 
#' A PTS-file has a simple header which indicates the number of nodes.  These
#' are stored in subsequent rows with their x-y-z-coordinates and boundary
#' flags.
#' 
#' @param filename (character scalar) Name of the pts-file to be created, with
#' extention.
#' 
#' @param PTS (numeric matrix) PTS-object to data is read from (created by
#' \code{\link{create.PTS}}).
#' 
#' @return A pts-file with x-y-z-coordinates and boundary flags as input for a
#' CHILD model run. The file comprises the data CHILD uses to create a TIN.
#' 
#' @author Michael Dietze
#' 
#' @seealso \code{\link{create.PTS}}, \code{\link{read.TIN}}
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
#'   # uncomment code to rune examples
#'   
#'   # load example data set
#'   data(DEM500)
#'   DEM <- DEM500
#'   
#'   ## assign PTS extent, just 1000 m smaller than DEM
#'   extent <- c(DEM@extent@xmin + 1000, 
#'               DEM@extent@xmax - 1000, 
#'               DEM@extent@ymin + 1000, 
#'               DEM@extent@ymax - 1000)
#'   
#'   # create and show a hexagonal PTS data set
#'   PTS <- create.PTS(DEM = DEM, 
#'                      extent = extent, 
#'                      spacing = 5000, 
#'                      type = "hexagonal", 
#'                      boundary = "open")
#'   plot(DEM)
#'   points(PTS[,1], PTS[,2], col = PTS[,4] + 1)
#'   
#' # write the PTS-file, uncomment to execute
#' # write.PTS("erzgebirge_5000m.pts", PTS = PTS)
#' 
#' @export write.PTS
write.PTS <- function(
  filename, 
  PTS
) {
  
  pts_file <- file(filename, "w") # open connection to pts file
  cat(c(nrow(PTS), "\n"), file = pts_file) # write header section
  for (i in 1:nrow(PTS)) {
    cat(c(PTS[i,1], PTS[i,2], PTS[i,3], 
      PTS[i,4]), "\n", file = pts_file)}
}