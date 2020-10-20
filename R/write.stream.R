#' Function to write streams to a shapefile.
#' 
#' This function creates and writes a shapefile object from traced streams.  If
#' present, further stream metadata will be exported as shapefile attributes as
#' well.
#' 
#' 
#' @param stream (list object) Stream object to be exported. This must be a
#' list object of only one timestep from \code{\link{trace.stream}}. See
#' examples for how to extract such a list structure.
#' 
#' @param filename (character scalar) Name of the output file, with extension.
#' 
#' @param segments (logical) Option to create stream individual segments (all
#' lines between nodes as separate objects) or merged objects (all lines
#' between nodes as one stream object), default is FALSE.
#' 
#' @param projection The geographic projection of the output file. If not
#' specified, the exported data set will not have any reference.
#' 
#' @return A shapefile with stream courses and thematic stream data.
#' 
#' @author Michael Dietze
#' @seealso \code{\link{write.raster}}, \code{\link{trace.stream}}
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
#' # trace two streams from different starting points through two time steps
#' startpoints <- cbind(c(50, 100), c(120, 100))
#' streams <- trace.stream(dataset = hillslope1, 
#'                         timestep = 2:3, 
#'                         startpoints = startpoints)
#' 
#' # extract the first time step as source data for shapefile export
#' streams_export <- streams[[1]]
#' 
#' # write the streams into a shapefile and save it
#' write.stream(stream = streams_export, 
#'              filename = "streams.shp", 
#'              segments = TRUE,
#'              projection = "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84")
#' 
#' @export write.stream
write.stream <- function(
  stream,
  filename,
  segments = FALSE,
 projection
) {
  
  ## load required library
  require(rgdal)
  
  ## retrieve output file name
  outfilename <- strsplit(filename, split = ".shp")[[1]]
  
  ## get/set projection
  if(missing(projection) == TRUE) projection <- NA
  
  ## create control and output variables
  n_stream   <- length(stream) # total number of individual stream objects
  stream_lines <- list("dummy") # dummy list of geometric properties
  stream_data  <- matrix(as.numeric(1), # dummy list of thematic properties
                         nrow = 1, 
                         ncol = 9)
  
  ## convert each stream into a vector object
  if(segments == TRUE) {
    ## option for individual segments enabled
    k <- 1 # set counting variable to 1
    for(i in 1:n_stream) {
      for(j in 2:length(stream[[i]]$x)) {
        ## extract vector node coordinates
        stream_lines[[length(stream_lines) + 1]] <- Lines(
          list(Line(cbind(c(stream[[i]]$x[(j - 1):j]), 
                          c(stream[[i]]$y[(j - 1):j])))), 
          ID = as.numeric(k))
        
        ## extract thematic data
        stream_data <- rbind(stream_data, c(
          stream[[i]]$length[j],    # length
          stream[[i]]$elevation[j], # elevation
          stream[[i]]$slope[j],     # slope
          stream[[i]]$area[j],      # contributing area
          stream[[i]]$x[j],         # x-coordinate
          stream[[i]]$y[j],         # y-coordinate
          stream[[i]]$nodes[j],     # node ID
          i,                        # stream number
          j))                       # segment number
        
        k <- k + 1 # update counter variable
      }
      j <- 2 # reset counter variable
    }
    
    ## remove dummy content from geometric properties
    stream_lines[[1]] <- NULL
    
    ## convert thematic stream properties to data frame
    stream_data <- as.data.frame(as.matrix(stream_data[2:nrow(
      stream_data),]), row.names = as.character(seq(1, k)))
  } else {
  ## option for individual segments disabled
    for(i in 1:n_stream) {
      ## extract vector node coordinates
      stream_lines[[length(stream_lines) + 1]] <- Lines(
        list(Line(cbind(c(stream[[i]]$x), c(stream[[i]]$y)))), 
        ID = as.numeric(i))
      
      ## extract thematic data
      stream_data <- rbind(stream_data, c(
        max(stream[[i]]$length),                      # length
        mean(stream[[i]]$elevation),                  # elevation
        mean(stream[[i]]$slope),                      # slope
        max(stream[[i]]$area, na.rm = TRUE),          # contributing area
        mean(stream[[i]]$x),                          # x-coordinate
        mean(stream[[i]]$y),                          # y-coordinate
        stream[[i]]$nodes[length(stream[[i]]$nodes)], # node ID
        i,                                            # stream number
        1))                                           # segment number
    }
    ## remove dummy content from geometric properties
    stream_lines[[1]] <- NULL
    
    ## convert thematic properties to data frame
    stream_data <- as.data.frame(as.matrix(stream_data[2:nrow(
      stream_data),]), row.names = as.character(seq(1, n_stream)))
  }
  
  ## combine geometric and thematic data
  stream_shp <- SpatialLinesDataFrame(SpatialLines(stream_lines,
    proj4string = CRS(as.character(projection))), data = stream_data)
  
  ## write column names of shapefile data
  colnames(stream_shp@data) <- c("length", 
                                 "elevation",
                                 "slope",
                                 "area",
                                 "xcoord",
                                 "ycoord",
                                 "nodeID",
                                 "stream",
                                 "segment")
  
  ## write data set to shapefile 
  writeOGR(obj = stream_shp,
           dsn = getwd(), 
           layer = outfilename, 
           driver="ESRI Shapefile")
}