#' Function to check a CHILD input file for
#' 
#' This function checks an S4-object \code{IN} for consistency. It can work
#' consistency. (detected issues noticed) or active (detected issues changed)
#' mode. A list of in passive detected issues is returned. Not assgined (empty)
#' and unused parameters are checked.
#' 
#' This function is primarily designed for two reasons: i) to identify and
#' potentially adjust common errors in the input-file prior to run CHILD and
#' ii) to remove uncessary parameters from the input-file. Although CHILD
#' reports some errors in the input-file, treating errors directly in R allows
#' for automatic error-handling rather than user- interaction with printed
#' error or warning messages.\cr The function is of preliminary stage, yet. So
#' far, only the presence of mandatory parameters ist checked and potentially
#' corrected, i.e. assigned with dummy values from some of the example
#' input-files from the CSDMS website. PLEASE HELP AND REPORT MISSING
#' FUNCTIONALITY AND BUGS TO THE MAINTAINER!
#' 
#' @param IN (character scalar) Name of the S4-object that will be checked.
#' 
#' @param mode (logical) Mode of how to deal with detected issues: active
#' (issues will be changed) as far as possible) or passive (issues will be
#' reported only), default is passive.
#' 
#' @return A list object \item{IN}{S4-object with the (potentially modified)
#' input-file} \item{warnings}{Character matrix with warnings}
#' \item{notifications}{Character matrix with notes}
#' 
#' @author Michael Dietze
#' 
#' @seealso \code{\link{write.IN}}, \code{\link{read.IN}},
#' \code{\link{create.IN}}
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
#' data(hillslope1.in)
#' 
#' # check inputfile in active mode
#' check <- check.IN(hillslope1.in, mode = "active")
#' 
#' # extract checked and corrected IN-object
#' hillslope1c.in <- check$IN
#' 
#' # show warnings report
#' check$warnings
#' 
#' @export check.IN
check.IN <- function(
  IN,
  mode
) {
  
  if(missing(mode) == TRUE) {mode = "passive"}
  
  warnings <- matrix(c("", "no warnings, congratulations!", ""), 
                     nrow = 1, 
                     ncol = 3) # create dummy output
  notifications <- matrix(c("", "no notifications, congratulations!", ""), 
                          nrow = 1, 
                          ncol = 3) # create dummy output
  colnames(notifications) <-  c("parameter", "issue", 
                                "solution") # assign column names
  colnames(warnings)      <-  c("parameter", "issue", 
                                "solution") # assign column names

  # 1. CHECK OF MANDATORY PARAMETERS
  
  # OUTFILENAME present?
  if((is.na(IN@OUTFILENAME) == TRUE)) {
    stamp <- Sys.time()
    stamp <- gsub(pattern = " ", replacement = "_", x = stamp)
    stamp <- gsub(pattern = ":", replacement = "_", x = stamp)
    stamp <- gsub(pattern = "-", replacement = "_", x = stamp)
    stamp <- paste("child_run_", stamp, sep = "")
    if(mode == "active") IN@OUTFILENAME <- stamp
    result <- c("OUTFILENAME", 
                "Warning: parameter OUFILENAME missing.", 
                "Set to default name.")
    warnings <- rbind(warnings, result)
  }
  
  # RUNTIME present?
  if((is.na(IN@RUNTIME) == TRUE)) {
    if(mode == "active") IN@RUNTIME <- 1
    result <- c("RUNTIME", 
                "Warning: parameter RUNTIME missing.", 
                "Set to 1 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # OPINTRVL present?
  if((is.na(IN@OPINTRVL) == TRUE)) {
    if(mode == "active") IN@OPINTRVL <- 1
    result <- c("OPINTRVL", 
                "Warning: parameter OPINTRVL missing.", 
                "Set to 1 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # OPTREADINPUT present?
  if((is.na(IN@OPTREADINPUT) == TRUE)) {
    if(mode == "active") IN@OPTREADINPUT <- 10
    result <- c("OPTREADINPUT", 
                "Warning: parameter OPTREADINPUT missing.", 
                "Set to 10 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # OPTVAR present?
  if((is.na(IN@OPTVAR) == TRUE)) {
    if(mode == "active") IN@OPTVAR <- 0
    result <- c("OPTVAR", 
                "Warning: parameter OPTVAR missing.", 
                "Set to 0 by default.")
    warnings <- rbind(warnings, result)
  }

  # ST_PMEAN present?
  if((is.na(IN@ST_PMEAN) == TRUE)) {
    if(mode == "active") IN@ST_PMEAN <- 16.4
    result <- c("ST_PMEAN", 
                "Warning: parameter ST_PMEAN missing.", 
                "Set to 16.4 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # ST_STDUR present?
  if((is.na(IN@ST_STDUR) == TRUE)) {
    if(mode == "active") IN@ST_STDUR <- 0.1
    result <- c("ST_STDUR", 
                "Warning: parameter ST_STDUR missing.", 
                "Set to 0.1 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # ST_ISTDUR present?
  if((is.na(IN@ST_ISTDUR) == TRUE)) {
    if(mode == "active") IN@ST_ISTDUR <- 0.9
    result <- c("ST_ISTDUR", 
                "Warning: parameter ST_ISTDUR missing.", 
                "Set to 0.9 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # FLOWGEN present?
  if((is.na(IN@FLOWGEN) == TRUE)) {
    if(mode == "active") IN@FLOWGEN <- 0
    result <- c("FLOWGEN", 
                "Warning: parameter FLOWGEN missing.", 
                "Set to 0 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # OPTMEANDER present?
  if((is.na(IN@OPTMEANDER) == TRUE)) {
    if(mode == "active") IN@OPTMEANDER <- 0
    result <- c("OPTMEANDER", 
                "Warning: parameter OPTMEANDER missing.", 
                "Set to 0 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # OPTMNDR present?
  if((is.na(IN@OPTMNDR) == TRUE)) {
    if(mode == "active") IN@OPTMNDR <- 0
    result <- c("OPTMNDR", 
                "Warning: parameter OPTMNDR missing.", 
                "Set to 0 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # OPTDETACHLIM present?
  if((is.na(IN@OPTDETACHLIM) == TRUE)) {
    if(mode == "active") IN@OPTDETACHLIM <- 1
    result <- c("OPTDETACHLIM", 
                "Warning: parameter OPTDETACHLIM missing.", 
                "Set to 1 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # OPTREADLAYER present?
  if((is.na(IN@OPTREADLAYER) == TRUE)) {
    if(mode == "active") IN@OPTREADLAYER <- 0
    result <- c("OPTREADLAYER", 
                "Warning: parameter OPTREADLAYER missing.", 
                "Set to 0 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # OPTLAYEROUTPUT present?
  if((is.na(IN@OPTLAYEROUTPUT) == TRUE)) {
    if(mode == "active") IN@OPTLAYEROUTPUT <- 0
    result <- c("OPTLAYEROUTPUT", 
                "Warning: parameter OPTLAYEROUTPUT missing.", 
                "Set to 0 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # OPTTSOUTPUT present?
  if((is.na(IN@OPTTSOUTPUT) == TRUE)) {
    if(mode == "active") IN@OPTTSOUTPUT <- 0
    result <- c("OPTTSOUTPUT", 
                "Warning: parameter OPTTSOUTPUT missing.", 
                "Set to 0 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # OPTINTERPLAYER present?
  if((is.na(IN@OPTINTERPLAYER) == TRUE)) {
    if(mode == "active") IN@OPTINTERPLAYER <- 0
    result <- c("OPTINTERPLAYER", 
                "Warning: parameter OPTINTERPLAYER missing.", 
                "Set to 0 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # OPTSTRATGRID present?
  if((is.na(IN@OPTSTRATGRID) == TRUE)) {
    if(mode == "active") IN@OPTSTRATGRID <- 0
    result <- c("OPTSTRATGRID", 
                "Warning: parameter OPTSTRATGRID missing.", 
                "Set to 0 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # OPTFLOODPLAIN present?
  if((is.na(IN@OPTFLOODPLAIN) == TRUE)) {
    if(mode == "active") IN@OPTFLOODPLAIN <- 0
    result <- c("OPTFLOODPLAIN", 
                "Warning: parameter OPTFLOODPLAIN missing.", 
                "Set to 0 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # OPTLOESSDEP present?
  if((is.na(IN@OPTLOESSDEP) == TRUE)) {
    if(mode == "active") IN@OPTLOESSDEP <- 0
    result <- c("OPTLOESSDEP", 
                "Warning: parameter OPTLOESSDEP missing.", 
                "Set to 0 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # OPTEXPOSURETIME present?
  if((is.na(IN@OPTEXPOSURETIME) == TRUE)) {
    if(mode == "active") IN@OPTEXPOSURETIME <- 0
    result <- c("OPTEXPOSURETIME", 
                "Warning: parameter OPTEXPOSURETIME missing.", 
                "Set to 0 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # OPTVEG present?
  if((is.na(IN@OPTVEG) == TRUE)) {
    if(mode == "active") IN@OPTVEG <- 0
    result <- c("OPTVEG", 
                "Warning: parameter OPTVEG missing.", 
                "Set to 0 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # OPTKINWAVE present?
  if((is.na(IN@OPTKINWAVE) == TRUE)) {
    if(mode == "active") IN@OPTKINWAVE <- 0
    result <- c("OPTKINWAVE", 
                "Warning: parameter OPTKINWAVE missing.", 
                "Set to 0 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # DETACHMENT_LAW present?
  if((is.na(IN@DETACHMENT_LAW) == TRUE)) {
    if(mode == "active") IN@DETACHMENT_LAW <- 0
    result <- c("DETACHMENT_LAW", 
                "Warning: parameter DETACHMENT_LAW missing.", 
                "Set to 0 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # MB present?
  if((is.na(IN@MB) == TRUE)) {
    if(mode == "active") IN@MB <- 0.6
    result <- c("MB", 
                "Warning: parameter MB missing.", 
                "Set to 0.6 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # NB present?
  if((is.na(IN@NB) == TRUE)) {
    if(mode == "active") IN@NB <- 0.7
    result <- c("NB", 
                "Warning: parameter NB missing.", 
                "Set to 0.7 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # PB present?
  if((is.na(IN@PB) == TRUE)) {
    if(mode == "active") IN@PB <- 1.5
    result <- c("PB", 
                "Warning: parameter PB missing.", 
                "Set to 1.5 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # TAUCB present?
  if((is.na(IN@TAUCB) == TRUE)) {
    if(mode == "active") IN@TAUCB <- 0
    result <- c("TAUCB", 
                "Warning: parameter TAUCB missing.", 
                "Set to 0 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # TAUCR present?
  if((is.na(IN@TAUCR) == TRUE)) {
    if(mode == "active") IN@TAUCR <- 0
    result <- c("TAUCR", 
                "Warning: parameter TAUCR missing.", 
                "Set to 0 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # TRANSPORT_LAW present?
  if((is.na(IN@TRANSPORT_LAW) == TRUE)) {
    if(mode == "active") IN@TRANSPORT_LAW <- 0
    result <- c("TRANSPORT_LAW", 
                "Warning: parameter TRANSPORT_LAW missing.", 
                "Set to 0 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # KD present?
  if((is.na(IN@KD) == TRUE)) {
    if(mode == "active") IN@KD <- 0.01
    result <- c("KD", 
                "Warning: parameter KD missing.", 
                "Set to 0.01 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # DIFFUSIONTHRESHOLD present?
  if((is.na(IN@DIFFUSIONTHRESHOLD) == TRUE)) {
    if(mode == "active") IN@DIFFUSIONTHRESHOLD <- 0
    result <- c("DIFFUSIONTHRESHOLD", 
                "Warning: parameter DIFFUSIONTHRESHOLD missing.", 
                "Set to 0 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # OPTDIFFDEP present?
  if((is.na(IN@OPTDIFFDEP) == TRUE)) {
    if(mode == "active") IN@OPTDIFFDEP <- 0
    result <- c("OPTDIFFDEP", 
                "Warning: parameter OPTDIFFDEP missing.", 
                "Set to 0 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # BEDROCKDEPTH present?
  if((is.na(IN@BEDROCKDEPTH) == TRUE)) {
    if(mode == "active") IN@BEDROCKDEPTH <- 1000000
    result <- c("BEDROCKDEPTH", 
                "Warning: parameter BEDROCKDEPTH missing.", 
                "Set to 1000000 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # REGINIT present?
  if((is.na(IN@REGINIT) == TRUE)) {
    if(mode == "active") IN@REGINIT <- 1
    result <- c("REGINIT", 
                "Warning: parameter REGINIT missing.", 
                "Set to 1 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # MAXREGDEPTH present?
  if((is.na(IN@MAXREGDEPTH) == TRUE)) {
    if(mode == "active") IN@MAXREGDEPTH <- 100
    result <- c("MAXREGDEPTH", 
                "Warning: parameter MAXREGDEPTH missing.", 
                "Set to 100 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # UPTYPE present?
  if((is.na(IN@UPTYPE) == TRUE)) {
    if(mode == "active") IN@UPTYPE <- 0
    result <- c("UPTYPE", 
                "Warning: parameter UPTYPE missing.", 
                "Set to 0 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # NUMGRNSIZE present?
  if((is.na(IN@NUMGRNSIZE) == TRUE)) {
    if(mode == "active") IN@NUMGRNSIZE <- 1
    result <- c("NUMGRNSIZE", 
                "Warning: parameter NUMGRNSIZE missing.", 
                "Set to 1 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # GRAINDIAM1 present?
  if((is.na(IN@GRAINDIAM1) == TRUE)) {
    if(mode == "active") IN@GRAINDIAM1 <- 0.001
    result <- c("GRAINDIAM1", 
                "Warning: parameter GRAINDIAM1 missing.", 
                "Set to 0.001 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # BRPROPORTION1 present?
  if((is.na(IN@BRPROPORTION1) == TRUE)) {
    if(mode == "active") IN@BRPROPORTION1 <- 1
    result <- c("BRPROPORTION1", 
                "Warning: parameter BRPROPORTION1 missing.", 
                "Set to 1 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # REGPROPORTION1 present?
  if((is.na(IN@REGPROPORTION1) == TRUE)) {
    if(mode == "active") IN@REGPROPORTION1 <- 1
    result <- c("REGPROPORTION1", 
                "Warning: parameter REGPROPORTION1 missing.", 
                "Set to 1 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # CHAN_GEOM_MODEL present?
  if((is.na(IN@CHAN_GEOM_MODEL) == TRUE)) {
    if(mode == "active") IN@CHAN_GEOM_MODEL <- 1
    result <- c("CHAN_GEOM_MODEL", 
                "Warning: parameter CHAN_GEOM_MODEL missing.", 
                "Set to 1 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # HYDR_WID_COEFF_DS present?
  if((is.na(IN@HYDR_WID_COEFF_DS) == TRUE)) {
    if(mode == "active") IN@HYDR_WID_COEFF_DS <- 10
    result <- c("HYDR_WID_COEFF_DS", 
                "Warning: parameter HYDR_WID_COEFF_DS missing.",
                "Set to 10 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # HYDR_WID_EXP_DS present?
  if((is.na(IN@HYDR_WID_EXP_DS) == TRUE)) {
    if(mode == "active") IN@HYDR_WID_EXP_DS <- 0.5
    result <- c("HYDR_WID_EXP_DS", 
                "Warning: parameter HYDR_WID_EXP_DS missing.", 
                "Set to 0.5 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # HYDR_WID_EXP_STN present?
  if((is.na(IN@HYDR_WID_EXP_STN) == TRUE)) {
    if(mode == "active") IN@HYDR_WID_EXP_STN <- 0.5
    result <- c("HYDR_WID_EXP_STN", 
                "Warning: parameter HYDR_WID_EXP_STN missing.", 
                "Set to 0.5 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # HYDR_DEP_COEFF_DS present?
  if((is.na(IN@HYDR_DEP_COEFF_DS) == TRUE)) {
    if(mode == "active") IN@HYDR_DEP_COEFF_DS <- 1
    result <- c("HYDR_DEP_COEFF_DS", 
                "Warning: parameter HYDR_DEP_COEFF_DS missing.", 
                "Set to 1 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # HYDR_DEP_EXP_DS present?
  if((is.na(IN@HYDR_DEP_EXP_DS) == TRUE)) {
    if(mode == "active") IN@HYDR_DEP_EXP_DS <- 0
    result <- c("HYDR_DEP_EXP_DS", 
                "Warning: parameter HYDR_DEP_EXP_DS missing.", 
                "Set to 0 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # HYDR_DEP_EXP_STN present?
  if((is.na(IN@HYDR_DEP_EXP_STN) == TRUE)) {
    if(mode == "active") IN@HYDR_DEP_EXP_STN <- 0
    result <- c("HYDR_DEP_EXP_STN", 
                "Warning: parameter HYDR_DEP_EXP_STN missing.", 
                "Set to 0 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # HYDR_ROUGH_COEFF_DS present?
  if((is.na(IN@HYDR_ROUGH_COEFF_DS) == TRUE)) {
    if(mode == "active") IN@HYDR_ROUGH_COEFF_DS <- 0.03
    result <- c("HYDR_ROUGH_COEFF_DS", 
                "Warning: parameter HYDR_ROUGH_COEFF_DS missing.", 
                "Set to 0.03 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # HYDR_ROUGH_EXP_DS present?
  if((is.na(IN@HYDR_ROUGH_EXP_DS) == TRUE)) {
    if(mode == "active") IN@HYDR_ROUGH_EXP_DS <- 0
    result <- c("HYDR_ROUGH_EXP_DS", 
                "Warning: parameter HYDR_ROUGH_EXP_DS missing.", 
                "Set to 0 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # HYDR_ROUGH_EXP_STN present?
  if((is.na(IN@HYDR_ROUGH_EXP_STN) == TRUE)) {
    if(mode == "active") IN@HYDR_ROUGH_EXP_STN <- 0
    result <- c("HYDR_ROUGH_EXP_STN", 
                "Warning: parameter HYDR_ROUGH_EXP_STN missing.", 
                "Set to 0 by default.")
    warnings <- rbind(warnings, result)
  }
  
  # BANKFULLEVENT present?
  if((is.na(IN@BANKFULLEVENT) == TRUE)) {
    if(mode == "active") IN@BANKFULLEVENT <- 0.5
    result <- c("BANKFULLEVENT", 
                "Warning: parameter BANKFULLEVENT missing.", 
                "Set to 0.5 by default.")
    warnings <- rbind(warnings, result)
  }
  
  
  
  # ACHTUNG, if OPTREADINPUT is missing it is set to 10 (create mesh) by default, set other params apprpriately
  # ACHTUNG, if FLOWGEN is missing it is set to 0 (uniform Hortonian) by default, INFILTRATION, OPTSINVARINFILT (optional) and depending on the latter PERIOD_INFILT
  # ACHTUNG, check interfingering with CHAN_GEOM_MODEL = 1
  
  # INPUTTIME present if POINTFILENAME or INPUTDATAFILE present?
  if((is.na(IN@POINTFILENAME) != TRUE | 
        is.na(IN@INPUTDATAFILE) != TRUE) && 
       is.na(IN@INPUTTIME) == TRUE) {
    result <- c("INPUTTIME", 
                "Warning: parameter INPUTTIME missing.", 
                "Set to 0 by default")
    warnings <- rbind(warnings, result)
  }
  
  # Redundant parameter INPUTTIME?
  if((is.na(IN@POINTFILENAME) == TRUE & 
        is.na(IN@INPUTDATAFILE) == TRUE) && 
       is.na(IN@INPUTTIME) != TRUE) {
    if(mode == "active") IN@INPUTTIME <- as.numeric(NA)
    result <- c("INPUTTIME", 
                "Notification: redundant parameter INPUTTIME.", 
                "Set to NA by default")
    notifications <- rbind(notifications, result)
  }
  
  # Consisteny in OUTLET_X_COORD and OUTLET_Y_COORD
  if(is.na(IN@OUTLET_X_COORD) == FALSE & is.na(IN@OUTLET_Y_COORD) == TRUE) {
    if(mode == "active") IN@OUTLET_X_COORD <- as.numeric(NA)
    result <- c("OUTLET_Y_COORD", 
                "Warning: parameter OUTLET_X_COORD but no OUTLET_Y_COORD.", 
                "OUTLET_X_COORD set to NA.")
    warnings <- rbind(warnings, result)
  }
  if(is.na(IN@OUTLET_Y_COORD) == FALSE & is.na(IN@OUTLET_X_COORD) == TRUE) {
    if(mode == "active") IN@OUTLET_Y_COORD <- as.numeric(NA)
    result <- c("OUTLET_X_COORD", 
                "Warning: parameter OUTLET_Y_COORD but no OUTLET_X_COORD.", 
                "OUTLET_Y_COORD set to NA.")
    warnings <- rbind(warnings, result)
  }
  
  # Redundant parameter CRITICAL_SLOPE?
  if(IN@OPT_NONLINEAR_DIFFUSION == 0 & 
       is.na(IN@CRITICAL_SLOPE) == FALSE) {
    if(mode == "active") IN@CRITICAL_SLOPE <- as.numeric(NA)
    result <- c("CRITICAL_SLOPE", 
                "Notification: redundant parameter CRITICAL_SLOPE.", 
                "Set to NA by default")
    notifications <- rbind(notifications, result)
  }
  
  
  
    
  warnings_out <- matrix(nrow = nrow(warnings), ncol = 1)
  for(i in 1:nrow(warnings)) {
    if(mode == "passive") warnings_out[i,1] <- warnings[i,2] else
      warnings_out[i,1] <- paste(warnings[i,2], warnings[i,3])
  }

  notifications_out <- matrix(nrow = nrow(notifications), ncol = 1)
  for(i in 1:nrow(notifications)) {
    if(mode == "passive") notifications_out[i,1] <- notifications[i,2] else
      notifications_out[i,1] <- paste(notifications[i,2], 
                                      notifications[i,3])
  }
  
  if(nrow(warnings_out) > 1) warnings_out <- warnings_out[
    2:nrow(warnings_out),] # if necessary, remove dummy output
  if(nrow(notifications_out) > 1) notifications_out <- notifications_out[
    2:nrow(notifications_out),] # if necessary, remove dummy output
  
  warnings_out <- as.matrix(warnings_out)
  notifications_out <- as.matrix(notifications_out)
  
  print(rbind(warnings_out, notifications_out))
  
  return(list(IN = IN,
              warnings = warnings_out,
              notifications = notifications_out))
}