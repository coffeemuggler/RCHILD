#' Function to read files to a CHILD-object.
#' 
#' This function reads all present output files of a CHILD model run and
#' creates an S4-object (CHILD) with the respective data.
#' 
#' 
#' @param dataset (character scalar) Name of the CHILD run, i.e. common file
#' names of the output files, without extension. All the files must be present
#' in the same directory. Either the workspace or a path to this CHILD run
#' directory must be set appropriately.
#' @return An S4-object with all output data of a CHILD model run.
#' @author Michael Dietze
#' @seealso \code{\link{read.IN}}, \code{\link{write.IN}},
#' \code{\link{write.raster}}, \code{\link{display.surface}}
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
#' # Examples are not run, remove hashes to run them
#' 
#' # # Run CHILD to create a model output
#'   # run.CHILD("hillslope1", "hillslope1.in")
#' 
#' # # Import the model run output data set
#'   # hillslope1 <- read.CHILD("hillslope1/hillslope1")
#' 
#' # # Show the summary of the imported data set
#'   # hillslope1@summary  
#' 
#' @export read.CHILD
read.CHILD <- function(
  dataset
){
  
  ## create separate filename variables for data input
  inputs_filename       <- paste(dataset, ".inputs", sep = "")
  pts_filename          <- paste(dataset, ".pts", sep = "")
  upliftmap_filename    <- paste(dataset, ".upliftmap", sep = "")
  layers_filename       <- paste(dataset, ".lay0", sep = "")
  nodes_filename        <- paste(dataset, ".nodes", sep = "")
  edges_filename        <- paste(dataset, ".edges", sep = "")
  tri_filename          <- paste(dataset, ".tri", sep = "")
  random_filename       <- paste(dataset, ".random", sep = "")
  varea_filename        <- paste(dataset, ".varea", sep = "")
  slp_filename          <- paste(dataset, ".slp", sep = "")
  area_filename         <- paste(dataset, ".area", sep = "")
  vols_filename         <- paste(dataset, ".vols", sep = "")
  dvols_filename        <- paste(dataset, ".dvols", sep = "")
  q_filename            <- paste(dataset, ".q", sep = "")
  net_filename          <- paste(dataset, ".net", sep = "")
  storm_filename        <- paste(dataset, ".storm", sep = "")
  tau_filename          <- paste(dataset, ".tau", sep = "")
  tx_filename           <- paste(dataset, ".tx", sep = "")
  z_filename            <- paste(dataset, ".z", sep = "")
  
  ## open and read CHILD file *.inputs if present  
  if (file.exists(inputs_filename) == FALSE) inputs <- as.character(
    NA) else inputs <- readLines(inputs_filename)
  
  if (file.exists(inputs_filename) == FALSE) {
    creation_date <- as.numeric(NA)
    RUNTIME       <- as.numeric(NA)
    OPINTRVL      <- as.numeric(NA)
    n_timesteps   <- as.numeric(NA)
  } else {
    creation_date <- inputs[1] # read creation date
    RUNTIME       <- as.numeric(inputs[seq(1:length(
      inputs))[inputs == "RUNTIME"] + 1]) # read rundtime
    OPINTRVL      <- as.numeric(inputs[seq(1:length(
      inputs))[inputs == "OPINTRVL"] + 1]) # read time interval
    n_timesteps   <- RUNTIME / OPINTRVL + 1 # calculate time steps
  }

  ## open and read CHILD file *.pts if present
  if (file.exists(pts_filename) == FALSE) pts <- as.matrix(NA) else {
    pts_data <- readLines(pts_filename) # read pts file
    pts_data <- pts_data[2:length(pts_data)] # remove pts header section
    pts <- matrix(nrow = length(pts_data), ncol = 4) # create pts matrix
    for (i in 1:length(pts_data)) pts[i,] <- as.numeric(
      strsplit(pts_data[i], " ")[[1]]) # write data to matrix
  }
  
  ## open and read CHILD file *.upliftmap if present
  upliftmap <- if (file.exists(upliftmap_filename) == FALSE) as.list(
    NA) else readLines(upliftmap_filename)
  
  ## open and read CHILD file *.lay if present
  layers <- if (file.exists(layers_filename) == FALSE) as.list(
    NA) else list(readLines(layers_filename))
  
  ## open and read CHILD file *.nodes if present
  if (file.exists(nodes_filename) == FALSE) nodes <- as.list(NA) else {  
    nodes_data <- readLines(nodes_filename) # read nodes file
    n_nodes <- as.numeric(nodes_data[2]) # read nodes header
    timesteps <- seq(0, n_timesteps) # create sequence of time steps
    nodes <- list(matrix(nrow = n_nodes, ncol = 3)) # create dummy list
    for (i in 1:n_timesteps) {
      ## create start of node readout interval
      line_start <- (n_nodes + 2) * timesteps[i] + 3
      ## create end of node readout interval    
      line_end <- line_start + n_nodes - 1 
      ## extract nodes for given timestep
      nodes_data_sub <- nodes_data[line_start:line_end]
      ## convert data to matrix
      nodes_matrix <- matrix(nrow = n_nodes, ncol = 4)
      for (i in 1:n_nodes) nodes_matrix[i,] <- as.numeric(
        strsplit(nodes_data_sub[i], " ")[[1]]) # write nodes to matrix
      nodes[[length(nodes) + 1]] <- nodes_matrix # append nodes to list
    }
    nodes[1] <- NULL # remove dummy matrix from list
  }
  
  ## open and read CHILD file *.edges if present
  if (file.exists(edges_filename) == FALSE) edges <- as.list(NA) else {  
    edges_data <- readLines(edges_filename) # read edges file
    n_edges <- as.numeric(edges_data[2]) # read edges header
    timesteps <- seq(0, n_timesteps) # create sequence of time steps
    edges <- list(matrix(nrow = n_edges, ncol = 3)) # create dummy list
    for (i in 1:n_timesteps) {
      ## create start of edge readout interval
      line_start <- (n_edges + 2) * timesteps[i] + 3
      ## create end of edge readout interval
      line_end <- line_start + n_edges - 1
      ## extract edges for given timestep
      edges_data_sub <- edges_data[line_start:line_end]
      ## convert data to matrix
      edges_matrix <- matrix(nrow = n_edges, ncol = 3)
      for (i in 1:n_edges) edges_matrix[i,] <- as.numeric(
        strsplit(edges_data_sub[i], " ")[[1]]) # write edges to matrix
      edges[[length(edges) + 1]] <- edges_matrix # append edges to list
    }
    edges[1] <- NULL # remove dummy matrix from list
  }
  
  ## open and read CHILD file *.tri if present
  if (file.exists(tri_filename) == FALSE) tri <- as.list(NA) else {  
    tri_data <- readLines(tri_filename) # read tri file
    n_tri <- as.numeric(tri_data[2]) # read tri header
    timesteps <- seq(0, n_timesteps) # create sequence of time steps
    tri <- list(matrix(nrow = n_tri, ncol = 3)) # create dummy list
    for (i in 1:n_timesteps) {
      ## create start of tri readout interval
      line_start <- (n_tri + 2) * timesteps[i] + 3 
      ## create end of tri readout interval
      line_end <- line_start + n_tri - 1
      ## extract tri for given timestep
      tri_data_sub <- tri_data[line_start:line_end]
      ## convert data to matrix
      tri_matrix <- matrix(nrow = n_tri, ncol = 9)
      for (i in 1:n_tri) tri_matrix[i,] <- as.numeric(
        strsplit(tri_data_sub[i], " ")[[1]]) # write tri to matrix
      tri[[length(tri) + 1]] <- tri_matrix # append tri matrix to list
    }
    tri[1] <- NULL # remove dummy matrix from list
  }
  
  ## open and read CHILD file *.random if present
  if (file.exists(random_filename) == FALSE) random <- as.numeric  (
    NA) else random <- as.numeric(readLines(random_filename))
  
  ## open and read CHILD file *.varea if present
  if (file.exists(varea_filename) == FALSE) varea <- as.list(NA) else {  
    varea_data <- readLines(varea_filename) # read varea file
    n_varea <- as.numeric(varea_data[2]) # read varea header
    timesteps <- seq(0, n_timesteps) # create sequence of time steps
    varea <- list(rep(NA, n_varea)) # create dummy list structure
    for (i in 1:n_timesteps) {
      ## create start of varea readout interval
      line_start <- (n_varea + 2) * timesteps[i] + 3
      ## create end of varea readout interval
      line_end <- line_start + n_varea - 1
      ## extract varea for given timestep
      varea_data_sub <- as.numeric(varea_data[line_start:line_end])
      ## append varea matrix to list
      varea[[length(varea) + 1]] <- varea_data_sub
    }
    varea[1] <- NULL # remove dummy matrix from list
  }
  
  ## open and read CHILD file *.slp if present
  if (file.exists(slp_filename) == FALSE) slp <- as.list(NA) else {  
    slp_data <- readLines(slp_filename) # read slp file
    n_slp <- as.numeric(slp_data[2]) # read slp header
    timesteps <- seq(0, n_timesteps) # create sequence of time steps
    slp <- list(rep(NA, n_slp)) # create dummy list
    for (i in 1:n_timesteps) {
      ## create start of slp readout interval
      line_start <- (n_slp + 2) * timesteps[i] + 3
      ## create end of slp readout interval
      line_end <- line_start + n_slp - 1
      ## extract slp for given timestep
      slp_data_sub <- as.numeric(slp_data[line_start:line_end])
      ## append slp matrix to list
      slp[[length(slp) + 1]] <- slp_data_sub
    }
    slp[1] <- NULL # remove dummy matrix from list
  }

  ## open and read CHILD file *.area if present
  if (file.exists(area_filename) == FALSE) area <- as.list(NA) else {  
    area_data <- readLines(area_filename) # read area file
    n_area <- as.numeric(area_data[2]) # read area header
    timesteps <- seq(0, n_timesteps) # create sequence of time steps
    area <- list(rep(NA, n_area)) # create dummy list structure
    for (i in 1:n_timesteps) {
      ## create start of area readout interval
      line_start <- (n_area + 2) * timesteps[i] + 3
      ## create end of area readout interval
      line_end <- line_start + n_area - 1
      ## extract area for given timestep
      area_data_sub <- as.numeric(area_data[line_start:line_end])
      ## append area matrix to list
      area[[length(area) + 1]] <- area_data_sub
    }
    area[1] <- NULL # remove dummy matrix from list
  }
  
  ## open and read CHILD file *.vols if present
  if (file.exists(vols_filename) == FALSE) vols <- as.numeric(
    NA) else vols <- as.numeric(readLines(vols_filename))
  
  ## open and read CHILD file *.dvols if present
  if (file.exists(dvols_filename) == FALSE) dvols <- as.numeric(
    NA) else dvols <- as.numeric(readLines(dvols_filename))
  
  ## open and read CHILD file *.q if present
  if (file.exists(q_filename) == FALSE) q <- as.list(NA) else {  
    q_data <- readLines(q_filename) # read q file
    n_q <- as.numeric(q_data[2]) # read q header
    timesteps <- seq(0, n_timesteps) # create sequence of time steps
    q <- list(rep(NA, n_q)) # create dummy list structure
    for (i in 1:n_timesteps) {
      ## create start of q readout interval
      line_start <- (n_q + 2) * timesteps[i] + 3
      ## create end of q readout interval
      line_end <- line_start + n_q - 1
      ## extract q for given timestep
      q_data_sub <- as.numeric(q_data[line_start:line_end])
      ## append q matrix to list
      q[[length(q) + 1]] <- q_data_sub
    }
    q[1] <- NULL # remove dummy matrix from list
  }
  
  ## open and read CHILD file *.net if present
  if (file.exists(net_filename) == FALSE) net <- as.list(NA) else {  
    net_data <- readLines(net_filename) # read net file
    n_net <- as.numeric(net_data[2]) # read net header
    timesteps <- seq(0, n_timesteps) # create senetuence of time steps
    net <- list(rep(NA, n_net)) # create dummy list
    for (i in 1:n_timesteps) {
      ## create start of net readout interval
      line_start <- (n_net + 2) * timesteps[i] + 3
      ## create end of net readout interval
      line_end <- line_start + n_net - 1
      ## extract net for given timestep
      net_data_sub <- as.numeric(net_data[line_start:line_end])
      ## append net matrix to list
      net[[length(net) + 1]] <- net_data_sub
    }
    net[1] <- NULL # remove dummy matrix from list
  }
  
  ## open and read CHILD file *.storm if present
  if (file.exists(storm_filename) == FALSE) storm <- as.matrix(
    NA) else {
      storm_data <- readLines(storm_filename)
      storm_data <- strsplit(storm_data, split = " ")
      storm <- matrix(as.numeric(unlist(storm_data)), 
                      ncol = 3, byrow = TRUE)
    }
  
  ## open and read CHILD file *.tau if present
  if (file.exists(tau_filename) == FALSE) tau <- as.list(NA) else {  
    tau_data <- readLines(tau_filename) # read tau file
    n_tau <- as.numeric(tau_data[2]) # read tau header
    timesteps <- seq(0, n_timesteps) # create setauuence of time steps
    tau <- list(rep(NA, n_tau)) # create dummy list
    for (i in 1:n_timesteps) {
      ## create start of tau readout interval
      line_start <- (n_tau + 2) * timesteps[i] + 3
      ## create end of tau readout interval
      line_end <- line_start + n_tau - 1
      ## extract tau for given timestep
      tau_data_sub <- as.numeric(tau_data[line_start:line_end])
      ## append tau matrix to list
      tau[[length(tau) + 1]] <- tau_data_sub
    }
    tau[1] <- NULL # remove dummy matrix from list
  }
  
  ## open and read CHILD file *.tx if present
  if (file.exists(tx_filename) == FALSE) tx <- as.list(NA) else {  
    tx_data <- readLines(tx_filename) # read tx file
    n_tx <- as.numeric(tx_data[2]) # read tx header
    timesteps <- seq(0, n_timesteps) # create sequence of time steps
    tx <- list(rep(NA, n_tx)) # create dummy list
    for (i in 1:n_timesteps) {
      ## create start of tx readout interval
      line_start <- (n_tx + 2) * timesteps[i] + 3
      ## create end of tx readout interval
      line_end <- line_start + n_tx - 1
      ## extract tx for given timestep
      tx_data_sub <- as.numeric(tx_data[line_start:line_end])
      ## append tx matrix to list
      tx[[length(tx) + 1]] <- tx_data_sub
    }
    tx[1] <- NULL # remove dummy matrix from list
  }
    
  ## open and read CHILD file *.z if present
  if (file.exists(z_filename) == FALSE) z <- as.list(NA) else {  
    z_data <- readLines(z_filename) # read z file
    n_z <- as.numeric(z_data[2]) # read z header
    timesteps <- seq(0, n_timesteps) # create sequence of time steps
    z <- list(rep(NA, n_z)) # create dummy list
    for (i in 1:n_timesteps) {
      ## create start of z readout interval
      line_start <- (n_z + 2) * timesteps[i] + 3
      ## create end of z readout interval
      line_end <- line_start + n_z - 1
      ## extract z for given timestep
      z_data_sub <- as.numeric(z_data[line_start:line_end])
      ## append z matrix to list
      z[[length(z) + 1]] <- z_data_sub
    }
    z[1] <- NULL # remove dummy matrix from list
  }
  
  ## create summary vector of imported CHILD model run data
  summary <- as.character(c(
    paste("Creation date:        ", creation_date), 
    paste("Run time:             ", RUNTIME), 
    paste("Output interval:      ", OPINTRVL), 
    paste("Number of time steps: ", n_timesteps), 
    paste("Number of nodes:      ", n_nodes)))
  
  ## create new S4-object CHILD from imported data  
  new("CHILD",
      summary      = summary,
      inputs       = inputs,
      timesteps    = timesteps,
      pts          = pts,
      upliftmap    = upliftmap,
      layers       = layers,
      nodes        = nodes,
      edges        = edges,
      tri          = tri,
      random       = random,
      varea        = varea,
      slp          = slp,
      area         = area,
      vols         = vols,
      dvols        = dvols,
      q            = q,
      net          = net,
      storm        = storm,
      tau          = tau,
      tx           = tx,
      z            = z
  )
}