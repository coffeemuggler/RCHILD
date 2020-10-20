#' Function to create a storm plot.
#' 
#' This function creates a time series of storm-derived rainfall intensity
#' [mm/hour] and returns a list object with the vectors time and intensity. The
#' parameter interval allows to create output for a subset of the entire time
#' series. Plot output is enabled by default.
#' 
#' 
#' @param dataset (S4-object) CHILD model run output data set.
#' 
#' @param interval (numerical scalar) Optional time interval to evaluate. If
#' specified, the vector must contain two elements: start and end time, values
#' in hours.
#' 
#' @param plot Optional plot output, default is TRUE.
#' 
#' @return A list object \item{time}{Numeric vector of time [hours]}
#' \item{intensity}{Numeric vector of rainfall intensity}
#' 
#' @author Michael Dietze
#' @seealso \code{\link{display.SAP}}
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
#' data(rainfall1)
#' 
#' # create and plot the storm intensity time series for a given hour interval
#' STORM <- calc.storm(rainfall1, interval = c(1000, 2000))
#' STORM
#' 
#' @export calc.storm
calc.storm <- function(
  dataset,
  interval,
  plot = TRUE
){
  
  ## read storm data
  storm <- dataset@storm

  ## get/set time interval
  if(missing(interval) == TRUE) interval <- FALSE
  
  ## define time conversion factors
  fhpa <- (24 * 365.25) # conversion factor hours per year
  fmmph <- 1000 / fhpa # conversion factor mm per hour
    
  ## create million-year-based storm data set
  storm[,c(1, 3)] <- storm[,c(1, 3)] * fhpa

  ## convert storm intensity to mm per hour
  intensity <- storm[,2] * fmmph
  
  ## create intensity for inter-storm times
  zero <- rep(0, length(intensity))
  
  # create time series of storm times and inter-storm time
  ti_cum <- c(cumsum((storm[,1] + c(0, storm[1:(nrow(storm) - 1),3]))), 0)
  ts_cum <- c(0, cumsum((storm[,1] + storm[,3])))
  time <- as.numeric(t(cbind(ts_cum, ts_cum, ti_cum, ti_cum)))
  intensity <- c(0,0, as.numeric(t(cbind(zero, 
                                         intensity, 
                                         intensity, 
                                         zero))), 0, 0)
  
  ## optionally, trim time series to specified sub time interval
  if(is.logical(interval) == FALSE) {
    time <- ifelse(time < interval[1] | time > interval[2], NA, time)
    intensity <- ifelse(time < interval[1] | 
      intensity > interval[2], NA, intensity)
    time <- time[!is.na(time)]
    intensity <- intensity[!is.na(intensity)]
  }
  
  ## optionally, plot the time series
  if(plot == TRUE) {
    plot(time, intensity, 
         type = "l",
         main = "Storm plot",
         xlab = "Time [hours]",
         ylab = "Intensity [mm/hour]")
  }
  
  ## return the time series as list object
  return(list(time = time,            ##<< Numeric vector of time [hours]
              intensity = intensity)) ##<< Numeric vector of rainfall intensity

}