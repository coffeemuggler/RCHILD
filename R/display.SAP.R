#' Function to plot slope vs. area.
#' 
#' This function creates a plot of slope va. area for all interiour nodes of a
#' CHILD model run, for each specified time step separately. The plot data sets
#' are returned as well.
#' 
#' No details yet
#' 
#' @param dataset (S4-object) CHILD model run output data set.
#' @param timesteps (numeric vector) Time steps use for calculation. If
#' missing, all modelled time steps are used.
#' @param plot (logical scalar) Option to show or hide plot output (dafault is
#' TRUE).
#' @param \dots Further plot parameters to pass to the plot object.
#' @return A list object \item{data}{A list with area and slope data per time
#' step.} \item{ranges}{A matrix with data ranges of area and slope.}
#' @author Michael Dietze
#' @seealso \code{\link{display.surface}},\code{\link{calc.storm}}
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
#' # plot slope vs. area for time steps 2 to 5 in a log-log-plot
#' SAP <- display.SAP(hillslope1, 
#'                   timesteps = 2:5, 
#'                   main = "Slope-area-plot", 
#'                   xlab = "area [square metres]", 
#'                   ylab = "slope [degree]",
#'                   log = "xy",
#'                   cex = 0.4)
#' 
#' # show some numeric output for time step 2
#' SAP$data[[2]][1:5,]
#' SAP$ranges
#' 
#' @export display.SAP
display.SAP <- function (
  dataset,
  timesteps,
  plot = TRUE,
  ...
){
  
  ## get/set timesteps
  if(missing(timesteps) == TRUE) timesteps <- 1:(length(dataset@timesteps))
  
  ## create dummy list
  data <- list(matrix(nrow = 1, ncol = 2))
  
  ## get all interiour slope and area values
  for(i in 1:length(timesteps)) {
    area <- dataset@area[[timesteps[i]]][
      dataset@nodes[[timesteps[i]]][,4] == 0]
    slope <- dataset@slp[[timesteps[i]]][
      dataset@nodes[[timesteps[i]]][,4] == 0]
    ## append nodes matrix to list
    data[[length(data) + 1]] <- cbind(area, slope)
  }
  ## remove dummy matrix from list
  data[1] <- NULL
  
  ## create time-step-overarching data set
  data_all <- data[[1]]
  if(length(timesteps) > 1) for(i in 2:length(timesteps)) {
    data_all <- rbind(data_all, data[[i]])
  }

  ## define overall ranges matrix
  ranges <- matrix(nrow = 2, ncol = 2) 
  rownames(ranges) <- c("area", "slope")
  colnames(ranges) <- c("min", "max")
  ## retrieve overall area and slope range
  ranges[1,] <- range(data_all[,1]) 
  ranges[2,] <- range(data_all[,2]) # retrieve overall slope range
  ## set first values to minimum values
  ranges[1,1] <- min(data_all[,1][data_all[,1] != 0], na.rm = TRUE)
  ranges[2,1] <- min(data_all[,2][data_all[,2] != 0], na.rm = TRUE)
  
  ## optional slope-area-plot
  if(plot == TRUE) {
    ## read out additional arguments list
    extraArgs <- list(...)
    ## assign point characters
    pch      <- if("pch" %in% names(extraArgs)) {extraArgs$pch} else
    {as.character(timesteps[1:length(timesteps)])}
    ## assign colour vector
    col      <- if("col" %in% names(extraArgs)) {extraArgs$col} else
    {1:length(timesteps)}
    ## assign scaling factor
    cex      <- if("cex" %in% names(extraArgs)) {extraArgs$cex} else 
    {0.6}
    
    ## create empty plot to scale plot area
    plot(NA,
         xlim = ranges[1,],
         ylim = ranges[2,],
         type = "n", 
         ...)
    
    ## plot all slope-area-values
    for(i in 1:length(timesteps)) {
      points(data[[i]][,1], data[[i]][,2], 
             pch = pch[i], 
             col = col[i],
             cex = cex
             )
    }
  }

  list(data = data,      ##<< A list with area and slope data per time step.
       ranges = ranges)  ##<< A matrix with data ranges of area and slope.
}