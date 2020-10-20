#' Function to calculate erosion rates.
#' 
#' This function calculates average erosion rates between specified time steps.
#' Optional uplift rates are considered.
#' 
#' 
#' @param dataset (S4-object) CHILD model run output data set.
#' 
#' @param timesteps (numeric vector or scalar) ID of the time steps for which
#' the erosion rate is calculated, should be specified as c(timestep1,
#' timestep0). If only one timestep is provided, the preceeding one (if
#' present) will be used automatically.
#' 
#' @param upliftrate (numeric vector or scalar) Optional uplift rate(s), if not
#' specified the upliftrate is taken from the CHILD model run output data set.
#' 
#' @param coordinates (logical scalar) Option to return node coordinates and
#' respective erosion rates or only erosion rates, default is TRUE.
#' 
#' @return If coordinates parameter is set TRUE a numeric matrix with node
#' coordinates and average erosion rates, otherwise a vector with average
#' erosion rates, only.
#' 
#' @author Michael Dietze
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
#' data(hillslope1) # read data set
#' 
#' # calculate erosion rate between time steps 1 and 6 without uplift
#' ER <- calc.erosion(hillslope1, timesteps = c(5,1))
#' 
#' # display result in native code
#' ERplot <- TIN.raster(TIN = ER, resolution = 5)
#' plot(ERplot, col = rev(heat.colors(250)))
#' 
#' # calculate and display result as map overlay using the package function
#' display.surface(hillslope1, 
#'                 timestep = c(5, 1), 
#'                 erosion = TRUE, 
#'                 type = "map")
#' 
#' @export calc.erosion
calc.erosion <- function(
  dataset,
  timesteps,
  upliftrate,
  coordinates = TRUE
) {
  
  ## get/set timestep values
  if(length(timesteps) == 2 & timesteps[1] > timesteps[2]) {
    ts1 <- timesteps[1]
    ts0 <- timesteps[2]
  } else if(length(timesteps) == 1) {
    ts0 <- ifelse(timesteps[1] <= 1, timesteps[1], timesteps[1] - 1)
    ts1 <- timesteps[1]
  } else stop("Unconform timestep format")

  ## infer uplift option from inputs data set
  upliftoption <- as.numeric(dataset@inputs[seq(1:length(
    dataset@inputs))[dataset@inputs == "UPTYPE"] + 1])
                            
  ## get/set uplift rate
  if(missing(upliftrate) == TRUE & upliftoption >= 1) {
    upliftrate <- as.numeric(dataset@inputs[seq(1:length(
      dataset@inputs))[dataset@inputs == "UPRATE"] + 1])
  } else upliftrate <- 0

  ## assign time interval
  dt <- as.numeric(dataset@inputs[seq(1:length(
    dataset@inputs))[dataset@inputs == "OPINTRVL"] + 1])
  
  ## calculate ersoion rate
  erosionrate <- upliftrate - (dataset@z[[ts1]] - dataset@z[[ts0]]) / (
    dt * (ts1) - dt * (ts0))

  ## create output matrix
  result <- cbind(dataset@nodes[[1]][,1:2], erosionrate)
  colnames(result) <- c("x_node", "y_node", "erosion_rate")

  
  if (coordinates == TRUE) {
    return(result)
  } else {
    return(erosionrate)
  }
}