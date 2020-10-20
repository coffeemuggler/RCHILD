#' Function to trace stream length profiles.
#' 
#' This function traces stream profiles, initiating at user-defined starting
#' points, downslope until an open boundary is reached. If several start point
#' coordinates are specified, multiple stream profile objects are generated.
#' If several time slices are specified, also several stream profiles are
#' generated.
#' 
#' 
#' @param dataset (S4-object) CHILD model run output data set.
#' 
#' @param timestep (numeric scalar or vector) Time step(s) for which stream
#' profiles are generated.
#' 
#' @param startpoints (numeric vector or matrix) Start point coordinates or
#' node IDs for stream profile tracing.
#' 
#' @return A list object (codestreams[[timestep]][[stream]]$parameter) with
#' subsequent list objects, arranged by time steps and streams.  Each
#' subsequent list object contains the following stream data: \cr length
#' (numeric vector) - stream distance from start point \cr elevation (list) -
#' elevation of the stream nodes \cr slope (list) - slope of the stream nodes
#' \cr area (list) - drainage area of the stream nodes \cr x (numeric vector) -
#' x-coordinates of the stream nodes \cr y (numeric vector) - y-coordinates of
#' the stream nodes \cr nodes (numeric vector) - ID of the stream nodes
#' 
#' @author Michael Dietze
#' @seealso \code{\link{display.surface}}, \code{\link{click.coordinates}}
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
#' # set function parameters
#' timesteps <- c(5:15) # timesteps through which streams are traced
#' startpoints <- cbind(c(50, 100), c(120, 100)) # start coordinates
#' 
#' # trace two streams through timesteps 10 to 20
#' X <- trace.stream(dataset = hillslope1, 
#'                   timestep = timesteps, 
#'                   startpoints = startpoints)
#' 
#' # show structure of X
#' str(X)
#' 
#' # plot the streams on a surface map
#' display.surface(hillslope1, timestep = 10, type = "map")
#' lines(X[[1]][[1]]$x_coord, X[[1]][[1]]$y_coord, col = 4)
#' lines(X[[1]][[2]]$x_coord, X[[1]][[2]]$y_coord, col = 4)
#' 
#' # plot stream 2 course over time on a surface map (meaningless)
#' display.surface(hillslope1, timestep = 10, type = "map")
#' for(i in 1:10) {lines(X[[i]][[2]]$x_coord, X[[i]][[2]]$y_coord, col = 4)}
#' 
#' # plot length profiles of stream 1 through time
#' plot(X[[1]][[1]]$length, X[[1]][[1]]$elevation, 
#'      type = "l", 
#'      ylim = c(0, 50),
#'      xlab = "stream length",
#'      ylab = "elevation")
#' for(i in 2:10) {lines(X[[i]][[1]]$length, X[[i]][[1]]$elevation)}
#' 
#' @export trace.stream
trace.stream <- function(
  dataset, 
  timestep, 
  startpoints
) {
  
  ## rearrange startpoints data structure
  if(is.matrix(startpoints) == FALSE) startpoints <- t(matrix(startpoints))
  
  ## create dummy output data set
  streams <- list(NA) # create dummy list structure
  
  # loop over all specified time steps
  for(i in 1:length(timestep)){
    ## assign node coordinates, IDs, boundary flags and downstream IDs
    x  <- dataset@nodes[[timestep[i]]][,1]
    y  <- dataset@nodes[[timestep[i]]][,2]
    id <- seq(1, length(x))
    bf <- dataset@nodes[[timestep[i]]][,4]
    ds <- dataset@net[[timestep[i]]] + 1
    
    ## if start points are coordinates find and assign nearest node
    if (ncol(startpoints) > 1) {
      start_id <- rep(NA, nrow(startpoints))
      for(j in 1:length(start_id)) {
        dist <- sqrt((
          x - startpoints[j,1])^2 + (y - startpoints[j,2])^2)
        start_id[j] <- id[dist == min(dist)]
      }
    } else start_id <- startpoints # alternatively use initial nodes IDs
    
    ## trace streams, originating at each start point
    stream_individual <- list(NA) # create dummy list
    for (k in 1:length(start_id)){
      n0 <- start_id[k] # assign start node
      stream_nodes <- rep(NA, length(id)) # create result vector
      stream_nodes[1] <- n0 # assign first result
      
      ## stream tracing algorithm, returning successive stream nodes
      for (l in 2:length(stream_nodes)) { # trace stream nodes
        if(is.na(bf[id == n0]) == TRUE) break # stop at open boundary
        n1 <- ds[id == n0] # assign downstream neighbour node
        stream_nodes[l] <- n1 # assign node to result vector
        n0 <- n1 # move nodes further
        if(is.na(n0) == TRUE) break
      }
      
      ## stream node post-processing and calculation of stream parameters
      stream_nodes <- stream_nodes[!is.na(stream_nodes)] # remove NA values
      xy_coords <- cbind(x[stream_nodes], y[stream_nodes])
      dxy_coords <- xy_coords[2:nrow(xy_coords),] - xy_coords[1:(
        nrow(xy_coords) - 1),] # calculate coordinate differences
    
      ## assign individual stream parameters
      stream_individual[[length(stream_individual) + 1]] <- list(
        length = c(0, cumsum(sqrt(dxy_coords[,1]^2 + dxy_coords[,1]^2))),
        elevation = dataset@z[[timestep[i]]][stream_nodes],
        slope = dataset@slp[[timestep[i]]][stream_nodes],
        area = dataset@area[[timestep[i]]][stream_nodes],
        x_coord = dataset@nodes[[timestep[i]]][stream_nodes,1],
        y_coord = dataset@nodes[[timestep[i]]][stream_nodes,2],
        nodes = dataset@nodes[[timestep[i]]][stream_nodes,3])
    }
    ## remove dummy matrix from list
    stream_individual[1] <- NULL 
    ## append individual stream data to general list
    streams[[length(streams) + 1]]  <- stream_individual 
   }
  
  ## remove dummy matrix from list
  streams[1] <- NULL 

  return(streams)
}