#' Function to trace a stream network.
#' 
#' This function traces stream network segments with a drainage area above a
#' certain threshold.
#' 
#' 
#' @param dataset (S4-object) CHILD model run output data set.
#' @param timestep (numeric scalar) time step for which the network will be
#' tracved.
#' @param area_min (numeric scalar) Minimum drainage area from which on a
#' stream is defined. If not specified, all nodes will be treated as stream.
#' @param width_scale (character scalar) Type of line width scaling for
#' drainage area. One out of "none" (equal line width), "linear" (linear
#' scaling), "root" (square root scaling), "power" (power two scaling), "log"
#' (logarithmic scaling), default is "none".
#' @param width_max (numeric scalar) Maximum line width, default is 1.
#' @param plot (logical scalar) Optional plotting of a surface map and the
#' stream network overlay, default is FALSE.
#' @return A numeric matrix with stream segment information
#' @author Michael Dietze
#' @seealso \code{\link{trace.stream}}
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
#' # create stream network for...
#' network <- trace.network(hillslope1,           # model run hillslope1
#'                          timestep = 5,         # timestep 5
#'                          area_min = 100,       # streams with > 100 sqm
#'                          width_scale = "root", # root-scaled line width
#'                          width_max = 5,        # maximum line width of 5
#'                          plot = TRUE)          # and plot it
#' 
#' # show the first five rows of the resulting matrix
#' network[1:5,]
#' 
#' @export trace.network
trace.network <- function(
  dataset,
  timestep,
  area_min,
  width_scale,
  width_max,
  plot = FALSE
) {
  
  ## check and assign predefined values
  if(missing(area_min) == TRUE) area_min <- 0
  if(missing(width_scale) == TRUE) width_scale <- "none"
  if(missing(width_max) == TRUE) width_max <- 1
  
  ## assign node IDs and find nodes beyond minimum drainage area
  node_ID <- dataset@nodes[[timestep]][,3]
  stream_nodes1 <- dataset@nodes[[timestep]][(
    dataset@area[[timestep]] > area_min),3]
  
  ## trace stream nodes and identify downslope neighbour
  streams <- matrix(nrow = length(stream_nodes1), ncol = 8)
  for(i in 1:length(stream_nodes1)) {
    x1 <- dataset@nodes[[timestep]][node_ID == stream_nodes1[i],1]
    y1 <- dataset@nodes[[timestep]][node_ID == stream_nodes1[i],2]
    z1 <- dataset@z[[timestep]][node_ID == stream_nodes1[i]]
    x2 <- dataset@nodes[[timestep]][dataset@net[[timestep]][
      node_ID == stream_nodes1[i]] + 1,1]
    y2 <- dataset@nodes[[timestep]][dataset@net[[timestep]][
      node_ID == stream_nodes1[i]] + 1,2]
    z2 <- dataset@z[[timestep]][dataset@net[[timestep]][
      node_ID == stream_nodes1[i]] + 1]
    a  <- dataset@area[[timestep]][node_ID == stream_nodes1[i]]
    streams[i,1:7] <- c(x1, y1, z1, x2, y2, z2, a)
  }
  
  ## normalise line width, scale it and re-scale it
  width_line <- (streams[,7] - min(streams[,7], na.rm = TRUE)) / (
    max((streams[,7] - min(streams[,7], na.rm = TRUE)), na.rm = TRUE))
  if(width_scale == "none") {
    width_scaled <- rep(1, length(width_line))
  } else if(width_scale == "linear") {
    width_scaled <- width_line
  } else if(width_scale == "root") {
    width_scaled <- sqrt(width_line)
  } else if(width_scale == "power") {
    width_scaled <- width_line^2
  }
  
  ## rescale width by maximum width parameter
  streams[,8] <- 1 + width_scaled * (width_max - 1)
  
  ## assign  column-names to output matrix
  colnames(streams) <- c("x1", "y1", "z1", "x2", "y2", "z2", "area", "width")
  
  ## optionally, plot drainage network
  if(plot == TRUE) {
    ## create a surface display plot
    display.surface(dataset, timestep)
    
    ## add all drainage network segments
    for(i in 1:nrow(streams)) {lines(streams[i,c(1, 4)], 
                                     streams[i,c(2, 5)], 
                                     col = 4, 
                                     lwd = streams[i,8])}
  }
  
  return(streams)
}