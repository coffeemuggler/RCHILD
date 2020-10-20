#' Function to display modelled surfaces.
#' 
#' This function offers three different viaualisation types of surfaces: "map"
#' - classic colour-coded grid map, "wireframe" - a perspective plot and
#' "scene" - a real time interactive 3D scene (using open GL). The surface can
#' carry additional layers such as a hillshade, drainage network, single
#' streams, erosion rates etc.
#' 
#' In order to create a hillshade from a grid (option if type is "map") the
#' input surface must have a defined projection. See example of function
#' \code{\link{animate.surface}} for how to create a projection
#' definition.\cr\cr Currently, streams can only be displayed as map and
#' wireframe. Let us just wait a few months to see if it is also possible to
#' visualise them as scenes, ok?
#' 
#' @param dataset (S4-object) CHILD model run output data set.
#' @param timestep (numeric scalar or vector) The time step for which a surface
#' shall be created. Usually, time step is a scalar, specifying only one time
#' step.  In the case of displaying an erosion rate, not for the actual time
#' step and its precursor, but for two other time steps, timestep must be a
#' vector with two elements: the actual time step t1 (also used for displaying
#' all other layers) and the time step t0 (used for the erosion calculation
#' only). To display the difference of two elevation surfaces, timestep can
#' also have two elements, t1 and t0.
#' @param resolution (numeric scalar) Resolution of the animated scene in
#' metres. If missing, the mean node distance of the TIN is used.
#' @param projection (character scalar) Geographic projection of the data set,
#' necessary to allow hillshade computation. If no projection is specified a
#' default one will be set ("+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100
#' +ellps=WGS84").  If you are unsure about this parameter see the
#' documentation of package "raster" or skip displaying a hillshade. Also, see
#' example for how to infer and set projections.
#' @param type (character scalar) Type of visualisation, one out of "map",
#' "wireframe" and "scene", default is "map".
#' @param exaggeration (numeric scalar) Vertical exaggeration of the z-values,
#' default is 2.
#' @param stream (logical scalar) Option to add streams, starting at
#' user-defined coordinates, to the surface, default is FALSE. If enabled,
#' startpoints must be specified.
#' @param startpoints (numeric vector or matrix) Start coordinates of streams
#' that are optinally added to the surface. x- and y-coordinates must be
#' provided column-wise.
#' @param network (logical scalar) Option to add a drainage network, default is
#' FALSE.
#' @param area_min (numeric scalar) Minimum drainage area from which on a
#' stream network is defined. If not specified, all nodes will be treated as
#' stream.
#' @param width_scale (character scalar) Type of line width scaling for
#' drainage area. One out of "none" (equal line width), "linear" (linear
#' scaling), "root" (square root scaling), "power" (power two scaling), "log"
#' (logarithmic scaling), default is "none".
#' @param width_max (numeric scalar) Maximum line width, default is 1.
#' @param erosion (logical scalar) Option to display erosion rates rather than
#' surface elevation, default is FALSE.
#' @param upliftrate (numeric scalar or vector) Optional constant uplift rate
#' or uplift rates of each node. Necessary for calculating erosion rates, if
#' not specified the uplift rate is taken from the CHILD model run output data
#' set.
#' @param hillshade (logical scalar) Optional hillshade overlay for type "map",
#' default is FALSE.
#' @param contours (logical scalar) Optional contours overlay for type "map",
#' default is FALSE.
#' @param theta (numeric scalar) Azimut angle of the view in degree, for type
#' "wireframe", default is 30.
#' @param phi (numeric scalar) Colatitude of the view in degree, for type
#' "wireframe", default is 30.
#' @param zlim (numeric vector) Optional z-axis limitc vector, defining
#' constant minimum and maximum z value for graphical output scale and legend.
#' Z generally refers to the elevation data except when type is "map", where it
#' may refer the erosion rate, if this option is enabled.
#' @return A plot object with a graphical representation of a modelled surface
#' along with additional optional layers.
#' @author Michael Dietze
#' @seealso \code{\link{animate.surface}}, \code{\link{read.CHILD}}
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
#'  # load example data set
#'  data(hillslope1)
#'  
#'  # display the hillslope1 model...
#'  display.surface(dataset = hillslope1, 
#'                  timestep = 10,                   # at time step 10
#'                  type = "map",                    # as a map output
#'                  contours = TRUE,                 # with contours
#'                  stream = TRUE,                   # and two streams,
#'                  startpoints = cbind(c(50, 100),  # originating at
#'                                      c(90, 120)), # these coordinates
#'                  zlim = c(0, 100))                # and this legend range
#'  
#'  # display the hillslope1 model...
#'   display.surface(dataset = hillslope1, 
#'                   timestep = 20,                   # at time step 20
#'                   type = "map",                    # as a map output
#'                   contours = TRUE,                 # with contours
#'                   hillshade = TRUE,                # with hillshade overlay
#'                   erosion = TRUE)                  # and defined value scale
#' 
#' # display the hillslope1 model...
#'  display.surface(dataset = hillslope1, 
#'                  timestep = 20,                   # at time step 20
#'                  type = "wireframe",              # as a wireframe output
#'                  stream = TRUE,                   # and two streams,
#'                  startpoints = cbind(c(50, 100),  # originating at
#'                                      c(90, 120))) # these coordinates
#'  
#'  # display the hillslope1 model...
#'  display.surface(dataset = hillslope1, 
#'                  timestep = 20,                   # at time step 20
#'                  type = "wireframe",              # as a wireframe output
#'                  stream = TRUE,                   # and two streams,
#'                  startpoints = cbind(c(50, 100),  # originating at
#'                                      c(90, 120)), # these coordinates
#'                  theta = 90,                      # with frontal view
#'                  phi = 20)                        # and lower tilting angle
#' 
#' @export display.surface
display.surface <- function(
  dataset,
  timestep,
  resolution,
  projection,
  type,
  exaggeration,
  stream = FALSE,
  startpoints,
  network = FALSE,
  area_min,
  width_scale,
  width_max,
  erosion = FALSE,
  upliftrate,
  hillshade = FALSE,
  contours = FALSE,
  theta,
  phi,
  zlim
){
  
  ## load required libraries
  require(raster)
  require(rgdal)
  require(rgl)
 
  ## set/adjust input parameters
  
  ## time steps
  if(length(timestep) == 2) { # optionally extract time step t0
    ts_erosion <- timestep
  } else if(timestep == 1) {ts_erosion  <- c(1, 
      1)} else {ts_erosion <- c(timestep, timestep - 1)}
  
  ## check/set type of visualisation
  if(missing(type) == TRUE) type <- "map"
  
  ## check/set resolution of raster interpolation
  if (missing(resolution) == TRUE) {
    ## extract mesh coordinates
    xy <- dataset@nodes[[timestep[1]]][,1:2]
    ## calculate mean node distance
    resolution <- mean(max(xy[,1], na.rm = TRUE) - 
                         min(xy[,1], na.rm = TRUE),
                       max(xy[,2], na.rm = TRUE) - 
                         min(xy[,2], na.rm = TRUE)) / 
       sqrt(nrow(xy))
  }
  
  ## check/set projection of the raster
  if(missing(projection) == TRUE) {projection <- 
    "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"}
  
  ## check/set exaggeration value
  if(missing(exaggeration) == TRUE) {exaggeration <- 2}
  
  ## check/set wireframe angles
  if(missing(theta) == TRUE) {theta <- 30}
  if(missing(phi) == TRUE) {phi <- 30}

  ## get uplift rate for erosion rate calculation
  upliftoption <- dataset@inputs[seq(1:length(
    dataset@inputs))[dataset@inputs == "UPTYPE"] + 1]
  if(missing(upliftrate) == TRUE & upliftoption >= 1) {
    upliftrate <- as.numeric(dataset@inputs[seq(1:length(
      dataset@inputs))[dataset@inputs == "UPRATE"] + 1])
  } else upliftrate <- 0
  
  ## check/set drainage area threshold for stream network tracing
  if(missing(area_min) == TRUE) area_min <- 0
  
  ## check/set line width scaling for streams
  if(missing(width_scale) == TRUE) width_scale <- "none"
  
  ## check/set maximum line width for streams
  if(missing(width_max) == TRUE) width_max <- 1

  ## Calculation of elevation and plot surface data sets
  ## create a GRID surface for given time step
  elevation <- TIN.raster(read.TIN(dataset,
                                   timestep = timestep[1]), 
                                   resolution = resolution)

  ## Depending on erosion == T: elevation or erosion rate
  if(erosion == FALSE) {
    if(length(timestep) > 1) {
      ## create a GRID surface 1
      plotsurface1 <- TIN.raster(read.TIN(dataset, 
                                 timestep = ts_erosion[1]), 
                                 resolution = resolution)
      ## create a GRID surface 2
      plotsurface2 <- TIN.raster(read.TIN(dataset,
                                 timestep = ts_erosion[2]), 
                                 resolution = resolution)
      ## calculate difference
      plotsurface <- plotsurface1 - plotsurface2
    } else {plotsurface <- elevation}
  } else {
    ## calculate erosion rate
    plotsurface <- TIN.raster(calc.erosion(dataset, 
                                timesteps = ts_erosion),
                                resolution = resolution)
  }
  
  ## Set projections of raster data sets
  projection(plotsurface) <- projection
  projection(elevation) <- projection
  
  ## Create hillshade data set
  if(hillshade == TRUE) {
    slope     <- terrain(elevation * exaggeration, opt = "slope")
    aspect    <- terrain(elevation * exaggeration, opt = "aspect")
    hs <- hillShade(slope, aspect, angle = 45, direction = 0)
  }
  
  ## get/set z-scale limits
  if(missing(zlim) == TRUE) {
    zlim <-c(min(values(plotsurface), na.rm = TRUE), 
    max(values(plotsurface), na.rm = TRUE))
  }
  
  ## Set colour map
  max_tot = zlim[2]
  max_loc = max(values(plotsurface), na.rm = TRUE)        
  n_col_loc <- round(250 * max_loc/ max_tot, 0)
  if(erosion == FALSE) {
    colourmap <- terrain.colors(250)
  } else {
    colourmap <- rev(heat.colors(250))
  }
  
  ## Display type MAP
  if(type == "map") {
    ## plot surface
    plot(plotsurface,
         col = colourmap,
         zlim = c(zlim[1], max_tot))
    ## optionally overlay hillshade and second plot surface
    if(hillshade == TRUE) {
      plot(hs, col = grey(0:100 / 100), 
           legend = FALSE)
      plot(plotsurface,
           alpha = 0.7, 
           col = colourmap[1:n_col_loc],
           zlim = c(zlim[1], max_tot),
           add = TRUE)  
    }
    ## optionally, add contours
    if(contours == TRUE) {
      contour(
        TIN.raster(
          read.TIN(dataset,
                   timestep = timestep[1]),
          resolution = resolution), 
        nlevels = 15,
        add = TRUE)
    }
    ## optionally, add streams
    if(stream == TRUE) {
      ## check if startpoints are provided
      if(missing(startpoints) == TRUE) {
        stop("No start points for streams provided.")}
      ## trace streams
      stream_nodes <- trace.stream(dataset, timestep[1], 
                                   startpoints)
      ## get contribution area for each stream data
      for(i in 1:nrow(startpoints)) {
        area_all <- stream_nodes[[1]][[i]]$area
        
      ## normalise line width, scale it and re-scale it
      width_line <- (area_all - min(area_all, na.rm = TRUE)) / (
        max((area_all - min(area_all, na.rm = TRUE)), na.rm = TRUE))
      if(width_scale == "none") {
        width_scaled <- rep(1, length(width_line))
      } else if(width_scale == "linear") {
        width_scaled <- width_line
      } else if(width_scale == "root") {
        width_scaled <- sqrt(width_line)
      } else if(width_scale == "power") {
        width_scaled <- width_line^2
      } 
      stream_width <- 1 + width_scaled * (width_max - 1)
      
      ## create stream data matrix
      stream_data <- cbind(
        stream_nodes[[1]][[i]]$x_coord[1:(length(stream_width) - 1)],
        stream_nodes[[1]][[i]]$x_coord[2:length(stream_width)],
        stream_nodes[[1]][[i]]$y_coord[1:(length(stream_width) - 1)],
        stream_nodes[[1]][[i]]$y_coord[2:length(stream_width)],
        stream_width[1:(length(stream_width) - 1)])
            
      ## plot all streams
      for(j in 1:nrow(stream_data)) {lines(stream_data[j,c(1, 2)], 
                                             stream_data[j,c(3, 4)], 
                                             col = 4, 
                                             lwd = stream_data[j,5])}
      }
    }
    ## optionally, add stream network
    if(network == TRUE) {
      ## trace stream network
      drainage_network <- trace.network(dataset, 
                                        timestep[1], 
                                        area_min, 
                                        width_scale, 
                                        width_max)
      ## plot stream segment lines
      for(i in 1:nrow(drainage_network)) {lines(
        drainage_network[i,c(1, 4)], 
        drainage_network[i,c(2, 5)], 
        col = 4, 
        lwd = drainage_network[i,8])}
    }
  }  else if (
    
  ## Display type WIREFRAME  
    type == "wireframe") {
    ## assign additional data sets
    Z <- elevation
    P <- plotsurface
    Zm <- matrix(values(Z), 
                nrow = Z@nrows,
                ncol = Z@ncols)
    Pm <- matrix(values(flip(P, "y")), 
                nrow = P@nrows,
                ncol = P@ncols)
    
    ## define colour map
    if(range(Pm, na.rm = TRUE)[1] == range(Pm, na.rm = TRUE)[2]) {
      facetcol <- 1
    }  else { 
      zfacet <- Pm[-1, -1] + Pm[-1, -ncol(Pm)] + 
        Pm[-nrow(Pm), -1] + Pm[-nrow(Pm), -ncol(Pm)]
      facetcol <- cut(zfacet, 250)
    }
    
    ## check/set z-limit for colouring
    if(missing(zlim) == TRUE) zlim <- c(min(Zm, na.rm = TRUE), max(Zm, na.rm = TRUE))
    
    ## create perspective plot
    persp(Z, 
          theta = theta, 
          phi = phi, 
          expand = exaggeration * 0.05, 
          shade = 0.01, 
          col = colourmap[facetcol],
          border = "grey",
          zlab = "z"
    ) -> res
    
    ## optionally, add streams
    if(stream == TRUE) {
      ## check data set
      if(missing(startpoints) == TRUE) {
        stop("No start points for streams provided.")
      }

      ## trace streams
      stream_nodes <- trace.stream(dataset, 
                                   timestep[1], 
                                   startpoints)
      
      ## get contribution area for each stream data
      for(i in 1:nrow(startpoints)) {
        area_all <- stream_nodes[[1]][[i]]$area
        
        ## normalise line width, scale it and re-scale it
        width_line <- (area_all - min(area_all, na.rm = TRUE)) / (
          max((area_all - min(area_all, na.rm = TRUE)), na.rm = TRUE))
        if(width_scale == "none") {
          width_scaled <- rep(1, length(width_line))
        } else if(width_scale == "linear") {
          width_scaled <- width_line
        } else if(width_scale == "root") {
          width_scaled <- sqrt(width_line)
        } else if(width_scale == "power") {
          width_scaled <- width_line^2
        } 
        stream_width <- 1 + width_scaled * (width_max - 1)
        
        ## create stream data matrix
        stream_data <- cbind(
          stream_nodes[[1]][[i]]$x_coord[1:(length(stream_width) - 1)],
          stream_nodes[[1]][[i]]$x_coord[2:length(stream_width)],
          stream_nodes[[1]][[i]]$y_coord[1:(length(stream_width) - 1)],
          stream_nodes[[1]][[i]]$y_coord[2:length(stream_width)],
          stream_nodes[[1]][[i]]$elevation[1:(length(stream_width) - 1)],
          stream_nodes[[1]][[i]]$elevation[2:length(stream_width)],
          stream_width[1:(length(stream_width) - 1)])
        
        ## plot all streams
        for(j in 1:nrow(stream_data)) {lines(
          trans3d(stream_data[j,c(1, 2)], 
                  stream_data[j,c(3, 4)], 
                  stream_data[j,c(5, 6)], 
                  pmat = res),
          col = 4, 
          lwd = stream_data[j,7])}
      }
    }
    ## optionally, add stream network
    if(network == TRUE) {
      ## trace stream network
      drainage_network <- trace.network(dataset, 
                                        timestep[1], 
                                        area_min, 
                                        width_scale, 
                                        width_max)
      
      ## plot stream segment lines
      for(i in 1:nrow(drainage_network)) {lines(
        trans3d(drainage_network[i,c(1, 4)], 
                drainage_network[i,c(2, 5)],
                drainage_network[i,c(3, 6)],
                pmat = res),
        col = 4, 
        lwd = drainage_network[i,8])}
    }
  }
  else if (
    
  ## Display type SCENE  
    type == "scene") {
    ## define 3D-data
    Z <- as.matrix(values(elevation)) * 2 * exaggeration
    X <- elevation@extent@xmin + resolution * 1:elevation@ncols
    Y <- elevation@extent@ymin + resolution * 1:elevation@nrows
    
    ## open rgl-graphics device
    rgl.open() 
    
    ## show erosion rate surface
    if(erosion == TRUE) {
      ## calculate erosion rate
      E <- TIN.raster(calc.erosion(dataset, 
                                   timesteps = ts_erosion,),
                      resolution = resolution)
      ## multiply erosion rate by arbitrary constant, may be modified in future
      E <-  as.matrix(values(E)) * 10000
      ## determine z-range
      lims <- max(E, na.rm = TRUE) - min(E, na.rm = TRUE)
      ## create colour table for z-values
      col_tab <- rev(heat.colors(list(lims)))
      ## assign colours to z-values
      col <- col_tab[E - min(E, na.rm = TRUE) + 1]
      ## add surface with erosion rate drape
      rgl.surface(X, Y, Z, color = col, back="lines")
    } else {
    
    ## show elevation surface
      ## determine z-range
      lims <- max(Z, na.rm = TRUE) - min(Z, na.rm = TRUE)
      ## create colour table for z-values
      col_tab <- terrain.colors(list(lims)) 
      ## assign colours to z-values
      col <- col_tab[Z - min(Z, na.rm = TRUE) + 1] 
      ## add surface with erosion rate drape
      rgl.surface(X, Y, Z, color = col, back="lines")
      }
    
    ## optionally, add streams
    if(stream == TRUE) {
      ## check if stream startpoints are present
      if(missing(startpoints) == TRUE) {stop(
        "No start points for streams provided.")}
      ## trace streams
      stream_nodes <- trace.stream(dataset, 
                                   timestep[1], 
                                   startpoints)
      ## plot all streams
      for(i in 1:nrow(startpoints)) {
        lines3d(x = stream_nodes[[1]][[i]]$x_coord,
                z = stream_nodes[[1]][[i]]$elevation * 
                  0.5 * exaggeration + 0.5,
                y = stream_nodes[[1]][[i]]$y_coord,
                color = rep("blue", nrow(startpoints)),
                lwd = 2)}
    }
    
  } else stop("Unsupported visualisation type!")
}