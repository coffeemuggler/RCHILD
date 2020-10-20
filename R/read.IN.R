#' Function to read a CHILD input file.
#' 
#' This function reads the parameters of a CHILD input file (*.in) and writes
#' them to an S4-object. Non-specified parameters are assigned NA.
#' 
#' There may occur a warning message (In initialize(value, ...) : NAs
#' introduced by coercion). This is the result of non-conform characters in the
#' input-file such as brackets and space-separated words. This warning message
#' does not change the content of the created S4-object.\cr It is important to
#' specify OPINTRVL above TSOPINTRVL in the input-file to read. This is due to
#' the text pattern matching algorithm. All other parameters may be specified
#' in any order desired.
#' 
#' @param IN (character scalar) Name of the CHILD input file to be read, with
#' extention.
#' @return An S4-object with all possible parameters as separate slots. Empty
#' parameters are assgined NA.
#' @author Michael Dietze
#' @seealso \code{\link{write.IN}}, \code{\link{read.CHILD}}
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
#' # hillslope1.in <- read.IN("hillslope1")
#' 
#' @export read.IN
read.IN <- function(
  IN
) {
  
  ## create in-file filename
  infile_filename <- IN
  
  ## read in-file when it exists
  if (file.exists(infile_filename) == FALSE) stop(
    "No such file found.") else infile <- readLines(infile_filename)
  
  ## read all model parameters and create new IN-object
  new("IN", # create IN-object
      ACCEL_REL_UPTIME = if(length(grep("^ACCEL_REL_UPTIME", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^ACCEL_REL_UPTIME", infile) + 1]), 
        # assign ACCEL_REL_UPTIME
      ANTICLINEXCOORD = if(length(grep("^ANTICLINEXCOORD", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^ANTICLINEXCOORD", infile) + 1]), 
        # assign ANTICLINEXCOORD
      ANTICLINEYCOORD = if(length(grep("^ANTICLINEYCOORD", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^ANTICLINEYCOORD", infile) + 1]), 
        # assign ANTICLINEYCOORD
      BANK_ROUGH_COEFF = if(length(grep("^BANK_ROUGH_COEFF", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^BANK_ROUGH_COEFF", infile) + 1]), 
        # assign BANK_ROUGH_COEFF
      BANK_ROUGH_EXP = if(length(grep("^BANK_ROUGH_EXP", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^BANK_ROUGH_EXP", infile) + 1]), 
        # assign BANK_ROUGH_EXP
      BANK_ERO = if(length(grep("^BANK_ERO", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(	
        infile[grep("^BANK_ERO", infile) + 1]), 
        # assign BANK_ERO
      BANKFULLEVENT = if(length(grep("^BANKFULLEVENT", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^BANKFULLEVENT", infile) + 1]), 
        # assign BANKFULLEVENT
      BEDROCKDEPTH = if(length(grep("^BEDROCKDEPTH", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^BEDROCKDEPTH", infile) + 1]), 
        # assign BEDROCKDEPTH
      BETA = if(length(grep("^BETA", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^BETA", infile) + 1]), 
        # assign BETA
      BLDIVIDINGLINE = if(length(grep("^BLDIVIDINGLINE", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^BLDIVIDINGLINE", infile) + 1]), 
        # assign BLDIVIDINGLINE
      BLFALL_UPPER = if(length(grep("^BLFALL_UPPER", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^BLFALL_UPPER", infile) + 1]), 
        # assign BLFALL_UPPER
      BNKHTDEP = if(length(grep("^BNKHTDEP", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^BNKHTDEP", infile) + 1]), 
        # assign BNKHTDEP
      BRPROPORTION1 = if(length(grep("^BRPROPORTION1", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^BRPROPORTION1", infile) + 1]), 
        # assign BRPROPORTION1
      BRPROPORTION2 = if(length(grep("^BRPROPORTION2", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^BRPROPORTION2", infile) + 1]), 
        # assign BRPROPORTION2
      BRPROPORTION3 = if(length(grep("^BRPROPORTION3", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^BRPROPORTION3", infile) + 1]), 
        # assign BRPROPORTION3
      BRPROPORTION4 = if(length(grep("^BRPROPORTION4", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^BRPROPORTION4", infile) + 1]), 
        # assign BRPROPORTION4
      BRPROPORTION5 = if(length(grep("^BRPROPORTION5", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^BRPROPORTION5", infile) + 1]), 
        # assign BRPROPORTION5
      BRPROPORTION6 = if(length(grep("^BRPROPORTION6", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^BRPROPORTION6", infile) + 1]), 
        # assign BRPROPORTION6
      BRPROPORTION7 = if(length(grep("^BRPROPORTION7", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^BRPROPORTION7", infile) + 1]), 
        # assign BRPROPORTION7
      BRPROPORTION8 = if(length(grep("^BRPROPORTION8", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^BRPROPORTION8", infile) + 1]), 
        # assign BRPROPORTION8
      BRPROPORTION9 = if(length(grep("^BRPROPORTION9", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^BRPROPORTION9", infile) + 1]), 
        # assign BRPROPORTION9
      BRPROPORTION10 = if(length(grep("^BRPROPORTION10", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^BRPROPORTION10", infile) + 1]), 
        # assign BRPROPORTION10
      BRPROPORTION11 = if(length(grep("^BRPROPORTION11", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^BRPROPORTION11", infile) + 1]), 
        # assign BRPROPORTION11
      BRPROPORTION12 = if(length(grep("^BRPROPORTION12", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^BRPROPORTION12", infile) + 1]), 
        # assign BRPROPORTION12
      BRPROPORTION13 = if(length(grep("^BRPROPORTION13", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^BRPROPORTION13", infile) + 1]), 
        # assign BRPROPORTION13
      BRPROPORTION14 = if(length(grep("^BRPROPORTION14", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^BRPROPORTION14", infile) + 1]), 
        # assign BRPROPORTION14
      BRPROPORTION15 = if(length(grep("^BRPROPORTION15", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^BRPROPORTION15", infile) + 1]), 
        # assign BRPROPORTION15
      BRPROPORTION16 = if(length(grep("^BRPROPORTION16", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^BRPROPORTION16", infile) + 1]), 
        # assign BRPROPORTION16
      BRPROPORTION17 = if(length(grep("^BRPROPORTION17", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^BRPROPORTION17", infile) + 1]), 
        # assign BRPROPORTION17
      BRPROPORTION18 = if(length(grep("^BRPROPORTION18", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^BRPROPORTION18", infile) + 1]), 
        # assign BRPROPORTION18
      BRPROPORTION19 = if(length(grep("^BRPROPORTION19", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^BRPROPORTION19", infile) + 1]), 
        # assign BRPROPORTION19
      BRPROPORTION20 = if(length(grep("^BRPROPORTION20", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^BRPROPORTION20", infile) + 1]), 
        # assign BRPROPORTION20
      CHAN_GEOM_MODEL = if(length(grep("^CHAN_GEOM_MODEL", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^CHAN_GEOM_MODEL", infile) + 1]), 
        # assign CHAN_GEOM_MODEL
      CRITICAL_AREA = if(length(grep("^CRITICAL_AREA", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^CRITICAL_AREA", infile) + 1]), 
        # assign CRITICAL_AREA
      CRITICAL_SLOPE = if(length(grep("^CRITICAL_SLOPE", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^CRITICAL_SLOPE", infile) + 1]), 
        # assign CRITICAL_SLOPE
      DECAY_PARAM_UPLIFT = if(length(grep("^DECAY_PARAM_UPLIFT", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^DECAY_PARAM_UPLIFT", infile) + 1]), 
        # assign DECAY_PARAM_UPLIFT
      DEF_CHAN_DISCR = if(length(grep("^DEF_CHAN_DISCR", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^DEF_CHAN_DISCR", infile) + 1]), 
        # assign DEF_CHAN_DISCR
      DETACHMENT_LAW = if(length(grep("^DETACHMENT_LAW", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^DETACHMENT_LAW", infile) + 1]), 
        # assign DETACHMENT_LAW
      DIFFUSIONTHRESHOLD = if(length(grep("^DIFFUSIONTHRESHOLD", 
        infile) + 1) == 0) as.numeric(
        0) else as.numeric(
        infile[grep("^DIFFUSIONTHRESHOLD", infile) + 1]), 
        # assign DIFFUSIONTHRESHOLD
      FAULT_PIVOT_DISTANCE = if(length(grep("^FAULT_PIVOT_DISTANCE", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^FAULT_PIVOT_DISTANCE", infile) + 1]), 
        # assign FAULT_PIVOT_DISTANCE
      FAULTPOS = if(length(grep("^FAULTPOS", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^FAULTPOS", infile) + 1]), 
        # assign FAULTPOS
      FLATDEPTH = if(length(grep("^FLATDEPTH", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^FLATDEPTH", infile) + 1]), 
        # assign FLATDEPTH
      FLOWGEN = if(length(grep("^FLOWGEN", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^FLOWGEN", infile) + 1]), 
        # assign FLOWGEN
      FLOWVELOCITY = if(length(grep("^FLOWVELOCITY", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^FLOWVELOCITY", infile) + 1]), 
        # assign FLOWVELOCITY
      FOLDLATRATE = if(length(grep("^FOLDLATRATE", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^FOLDLATRATE", infile) + 1]), 
        # assign FOLDLATRATE
      FOLDPOSITION = if(length(grep("^FOLDPOSITION", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^FOLDPOSITION", infile) + 1]), 
        # assign FOLDPOSITION
      FOLDPROPRATE = if(length(grep("^FOLDPROPRATE", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^FOLDPROPRATE", infile) + 1]), 
        # assign FOLDPROPRATE
      FOLDUPRATE = if(length(grep("^FOLDUPRATE", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^FOLDUPRATE", infile) + 1]), 
        # assign FOLDUPRATE
      FOLDWAVELEN = if(length(grep("^FOLDWAVELEN", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^FOLDWAVELEN", infile) + 1]), 
        # assign FOLDWAVELEN
      FP_BANKFULLEVENT = if(length(grep("^FP_BANKFULLEVENT", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^FP_BANKFULLEVENT", infile) + 1]), 
        # assign FP_BANKFULLEVENT
      FP_DRAREAMIN = if(length(grep("^FP_DRAREAMIN", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^FP_DRAREAMIN", infile) + 1]), 
        # assign FP_DRAREAMIN
      FP_INLET_ELEVATION = if(length(grep("^FP_INLET_ELEVATION", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^FP_INLET_ELEVATION", infile) + 1]), 
        # assign FP_INLET_ELEVATION
      FP_LAMBDA = if(length(grep("^FP_LAMBDA", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^FP_LAMBDA", infile) + 1]), 
        # assign FP_LAMBDA
      FP_MU = if(length(grep("^FP_MU", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^FP_MU", infile) + 1]), 
        # assign FP_MU
      FP_OPTCONTROLCHAN = if(length(grep("^FP_OPTCONTROLCHAN", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^FP_OPTCONTROLCHAN", infile) + 1]), 
        # assign FP_OPTCONTROLCHAN
      FP_VALDROP = if(length(grep("^FP_VALDROP", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^FP_VALDROP", infile) + 1]), 
        # assign FP_VALDROP
      FRAC_WID_ADD = if(length(grep("^FRAC_WID_ADD", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^FRAC_WID_ADD", infile) + 1]), 
        # assign FRAC_WID_ADD
      FRAC_WID_MOVE = if(length(grep("^FRAC_WID_MOVE", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^FRAC_WID_MOVE", infile) + 1]), 
        # assign FRAC_WID_MOVE
      FRONT_PROP_RATE = if(length(grep("^FRONT_PROP_RATE", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^FRONT_PROP_RATE", infile) + 1]), 
        # assign FRONT_PROP_RATE
      GRAINDIAM1 = if(length(grep("^GRAINDIAM1", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^GRAINDIAM1", infile) + 1]), 
        # assign GRAINDIAM1
      GRAINDIAM2 = if(length(grep("^GRAINDIAM2", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^GRAINDIAM2", infile) + 1]), 
        # assign GRAINDIAM2
      GRAINDIAM3 = if(length(grep("^GRAINDIAM3", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^GRAINDIAM3", infile) + 1]), 
        # assign GRAINDIAM3
      GRAINDIAM4 = if(length(grep("^GRAINDIAM4", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^GRAINDIAM4", infile) + 1]), 
        # assign GRAINDIAM4
      GRAINDIAM5 = if(length(grep("^GRAINDIAM5", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^GRAINDIAM5", infile) + 1]), 
        # assign GRAINDIAM5
      GRAINDIAM6 = if(length(grep("^GRAINDIAM6", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^GRAINDIAM6", infile) + 1]), 
        # assign GRAINDIAM6
      GRAINDIAM7 = if(length(grep("^GRAINDIAM7", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^GRAINDIAM7", infile) + 1]), 
        # assign GRAINDIAM7
      GRAINDIAM8 = if(length(grep("^GRAINDIAM8", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^GRAINDIAM8", infile) + 1]), 
        # assign GRAINDIAM8
      GRAINDIAM9 = if(length(grep("^GRAINDIAM9", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^GRAINDIAM9", infile) + 1]), 
        # assign GRAINDIAM9
      GRAINDIAM10 = if(length(grep("^GRAINDIAM10", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^GRAINDIAM10", infile) + 1]), 
        # assign GRAINDIAM10
      GRAINDIAM11 = if(length(grep("^GRAINDIAM11", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^GRAINDIAM11", infile) + 1]), 
        # assign GRAINDIAM11
      GRAINDIAM12 = if(length(grep("^GRAINDIAM12", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^GRAINDIAM12", infile) + 1]), 
        # assign GRAINDIAM12
      GRAINDIAM13 = if(length(grep("^GRAINDIAM13", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^GRAINDIAM13", infile) + 1]), 
        # assign GRAINDIAM13
      GRAINDIAM14 = if(length(grep("^GRAINDIAM14", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^GRAINDIAM14", infile) + 1]), 
        # assign GRAINDIAM14
      GRAINDIAM15 = if(length(grep("^GRAINDIAM15", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^GRAINDIAM15", infile) + 1]), 
        # assign GRAINDIAM15
      GRAINDIAM16 = if(length(grep("^GRAINDIAM16", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^GRAINDIAM16", infile) + 1]), 
        # assign GRAINDIAM16
      GRAINDIAM17 = if(length(grep("^GRAINDIAM17", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^GRAINDIAM17", infile) + 1]), 
        # assign GRAINDIAM17
      GRAINDIAM18 = if(length(grep("^GRAINDIAM18", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^GRAINDIAM18", infile) + 1]), 
        # assign GRAINDIAM18
      GRAINDIAM19 = if(length(grep("^GRAINDIAM19", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^GRAINDIAM19", infile) + 1]), 
        # assign GRAINDIAM19
      GRAINDIAM20 = if(length(grep("^GRAINDIAM20", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^GRAINDIAM20", infile) + 1]), 
        # assign GRAINDIAM20
      GRID_LENGTH = if(length(grep("^GRID_LENGTH", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^GRID_LENGTH", infile) + 1]), 
        # assign GRID_LENGTH
      GRID_WIDTH = if(length(grep("^GRID_WIDTH", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^GRID_WIDTH", infile) + 1]), 
        # assign GRID_WIDTH
      GRID_SPACING = if(length(grep("^GRID_SPACING", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^GRID_SPACING", infile) + 1]), 
        # assign GRID_SPACING
      GRIDDX = if(length(grep("^GRIDDX", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^GRIDDX", infile) + 1]), 
        # assign GRIDDX
      HIDINGEXP = if(length(grep("^HIDINGEXP", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^HIDINGEXP", infile) + 1]), 
        # assign HIDINGEXP
      HYDR_DEP_COEFF_DS = if(length(grep("^HYDR_DEP_COEFF_DS", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^HYDR_DEP_COEFF_DS", infile) + 1]), 
        # assign HYDR_DEP_COEFF_DS
      HYDR_DEP_EXP_DS = if(length(grep("^HYDR_DEP_EXP_DS", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^HYDR_DEP_EXP_DS", infile) + 1]), 
        # assign HYDR_DEP_EXP_DS
      HYDR_DEP_EXP_STN = if(length(grep("^HYDR_DEP_EXP_STN", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^HYDR_DEP_EXP_STN", infile) + 1]), 
        # assign HYDR_DEP_EXP_STN
      HYDR_ROUGH_COEFF_DS = if(length(grep("^HYDR_ROUGH_COEFF_DS", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^HYDR_ROUGH_COEFF_DS", infile) + 1]), 
        # assign HYDR_ROUGH_COEFF_DS
      HYDR_ROUGH_EXP_DS = if(length(grep("^HYDR_ROUGH_EXP_DS", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^HYDR_ROUGH_EXP_DS", infile) + 1]), 
        # assign HYDR_ROUGH_EXP_DS
      HYDR_ROUGH_EXP_STN = if(length(grep("^HYDR_ROUGH_EXP_STN", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^HYDR_ROUGH_EXP_STN", infile) + 1]), 
        # assign HYDR_ROUGH_EXP_STN
      HYDR_WID_COEFF_DS = if(length(grep("^HYDR_WID_COEFF_DS", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^HYDR_WID_COEFF_DS", infile) + 1]), 
        # assign HYDR_WID_COEFF_DS
      HYDR_WID_EXP_DS = if(length(grep("^HYDR_WID_EXP_DS", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^HYDR_WID_EXP_DS", infile) + 1]), 
        # assign HYDR_WID_EXP_DS
      HYDR_WID_EXP_STN = if(length(grep("^HYDR_WID_EXP_STN", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^HYDR_WID_EXP_STN", infile) + 1]), 
        # assign HYDR_WID_EXP_STN
      HYDROSHAPEFAC = if(length(grep("^HYDROSHAPEFAC", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^HYDROSHAPEFAC", infile) + 1]), 
        # assign HYDROSHAPEFAC
      INDRAREA = if(length(grep("^INDRAREA", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^INDRAREA", infile) + 1]), 
        # assign INDRAREA
      INFILTRATION = if(length(grep("^INFILTRATION", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^INFILTRATION", infile) + 1]), 
        # assign INFILTRATION
      INLET_OPTCALCSEDFEED = if(length(grep("^INLET_OPTCALCSEDFEED", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^INLET_OPTCALCSEDFEED", infile) + 1]), 
        # assign INLET_OPTCALCSEDFEED
      INLET_SLOPE = if(length(grep("^INLET_SLOPE", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^INLET_SLOPE", infile) + 1]), 
        # assign INLET_SLOPE
      INLET_X = if(length(grep("^INLET_X", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^INLET_X", infile) + 1]), 
        # assign INLET_X
      INLET_Y = if(length(grep("^INLET_Y", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^INLET_Y", infile) + 1]), 
        # assign INLET_Y
      INPUTDATAFILE = if(length(grep("^INPUTDATAFILE", 
        infile) + 1) == 0) as.character(NA) else as.character(
        infile[grep("^INPUTDATAFILE", infile) + 1]), 
        # assign INPUTDATAFILE
      INPUTTIME = if(length(grep("^INPUTTIME", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^INPUTTIME", infile) + 1]), 
        # assign INPUTTIME
      INSEDLOAD1 = if(length(grep("^INSEDLOAD1", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^INSEDLOAD1", infile) + 1]), 
        # assign INSEDLOAD1
      INSEDLOAD2 = if(length(grep("^INSEDLOAD2", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^INSEDLOAD2", infile) + 1]), 
        # assign INSEDLOAD2
      INSEDLOAD3 = if(length(grep("^INSEDLOAD3", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^INSEDLOAD3", infile) + 1]), 
        # assign INSEDLOAD3
      INSEDLOAD4 = if(length(grep("^INSEDLOAD4", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^INSEDLOAD4", infile) + 1]), 
        # assign INSEDLOAD4
      INSEDLOAD5 = if(length(grep("^INSEDLOAD5", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^INSEDLOAD5", infile) + 1]), 
        # assign INSEDLOAD5
      INSEDLOAD6 = if(length(grep("^INSEDLOAD6", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^INSEDLOAD6", infile) + 1]), 
        # assign INSEDLOAD6
      INSEDLOAD7 = if(length(grep("^INSEDLOAD7", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^INSEDLOAD7", infile) + 1]), 
        # assign INSEDLOAD7
      INSEDLOAD8 = if(length(grep("^INSEDLOAD8", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^INSEDLOAD8", infile) + 1]), 
        # assign INSEDLOAD8
      INSEDLOAD9 = if(length(grep("^INSEDLOAD9", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^INSEDLOAD9", infile) + 1]), 
        # assign INSEDLOAD9
      INSEDLOAD10 = if(length(grep("^INSEDLOAD10", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^INSEDLOAD10", infile) + 1]), 
        # assign INSEDLOAD10
      INSEDLOAD11 = if(length(grep("^INSEDLOAD11", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^INSEDLOAD11", infile) + 1]), 
        # assign INSEDLOAD11
      INSEDLOAD12 = if(length(grep("^INSEDLOAD12", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^INSEDLOAD12", infile) + 1]), 
        # assign INSEDLOAD12
      INSEDLOAD13 = if(length(grep("^INSEDLOAD13", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^INSEDLOAD13", infile) + 1]), 
        # assign INSEDLOAD13
      INSEDLOAD14 = if(length(grep("^INSEDLOAD14", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^INSEDLOAD14", infile) + 1]), 
        # assign INSEDLOAD14
      INSEDLOAD15 = if(length(grep("^INSEDLOAD15", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^INSEDLOAD15", infile) + 1]), 
        # assign INSEDLOAD15
      INSEDLOAD16 = if(length(grep("^INSEDLOAD16", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^INSEDLOAD16", infile) + 1]), 
        # assign INSEDLOAD16
      INSEDLOAD17 = if(length(grep("^INSEDLOAD17", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^INSEDLOAD17", infile) + 1]), 
        # assign INSEDLOAD17
      INSEDLOAD18 = if(length(grep("^INSEDLOAD18", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^INSEDLOAD18", infile) + 1]), 
        # assign INSEDLOAD18
      INSEDLOAD19 = if(length(grep("^INSEDLOAD19", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^INSEDLOAD19", infile) + 1]), 
        # assign INSEDLOAD19
      INSEDLOAD20 = if(length(grep("^INSEDLOAD20", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^INSEDLOAD20", infile) + 1]), 
        # assign INSEDLOAD20
      KB = if(length(grep("^KB", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^KB", infile) + 1]), 
        # assign KB
      KD = if(length(grep("^KD", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^KD", infile) + 1]), 
        # assign KD
      KF = if(length(grep("^KF", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^KF", infile) + 1]), 
        # assign KF
      KINKDIP = if(length(grep("^KINKDIP", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^KINKDIP", infile) + 1]), 
        # assign KINKDIP
      KINWAVE_HQEXP = if(length(grep("^KINWAVE_HQEXP", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^KINWAVE_HQEXP", infile) + 1]), 
        # assign KINWAVE_HQEXP
      KR = if(length(grep("^KR", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^KR", infile) + 1]), 
        # assign KR
      KT = if(length(grep("^KT", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^KT", infile) + 1]), 
        # assign KT
      LAKEFILL = if(length(grep("^LAKEFILL", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^LAKEFILL", infile) + 1]), 
        # assign LAKEFILL
      LOESS_DEP_RATE = if(length(grep("^LOESS_DEP_RATE", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^LOESS_DEP_RATE", infile) + 1]), 
        # assign LOESS_DEP_RATE
      MAXICMEAN = if(length(grep("^MAXICMEAN", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^MAXICMEAN", infile) + 1]), 
        # assign MAXICMEAN
      MAXREGDEPTH = if(length(grep("^MAXREGDEPTH", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^MAXREGDEPTH", infile) + 1]), 
        # assign MAXREGDEPTH
      MB = if(length(grep("^MB", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^MB", infile) + 1]), 
        # assign MB
      MEAN_ELEV = if(length(grep("^MEAN_ELEV", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^MEAN_ELEV", infile) + 1]), 
        # assign MEAN_ELEV
      MEDIAN_DIAMETER = if(length(grep("^MEDIAN_DIAMETER", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^MEDIAN_DIAMETER", infile) + 1]), 
        # assign MEDIAN_DIAMETER
      MESHADAPT_MAXNODEFLUX = if(length(grep("^MESHADAPT_MAXNODEFLUX", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^MESHADAPT_MAXNODEFLUX", infile) + 1]), 
        # assign MESHADAPT_MAXNODEFLUX
      MESHADAPTAREA_MINAREA = if(length(grep("^MESHADAPTAREA_MINAREA", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^MESHADAPTAREA_MINAREA", infile) + 1]), 
        # assign MESHADAPTAREA_MINAREA
      MESHADAPTAREA_MAXVAREA = if(length(grep("^MESHADAPTAREA_MAXVAREA", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^MESHADAPTAREA_MAXVAREA", infile) + 1]), 
        # assign MESHADAPTAREA_MAXVAREA
      MF = if(length(grep("^MF", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^MF", infile) + 1]), 
        # assign MF
      MINIMUM_UPRATE = if(length(grep("^MINIMUM_UPRATE", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^MINIMUM_UPRATE", infile) + 1]), 
        # assign MINIMUM_UPRATE
      NB = if(length(grep("^NB", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^NB", infile) + 1]), 
        # assign NB
      NF = if(length(grep("^NF", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^NF", infile) + 1]), 
        # assign NF
      NUM_PTS = if(length(grep("^NUM_PTS", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^NUM_PTS", infile) + 1]), 
        # assign NUM_PTS
      NUMGRNSIZE = if(length(grep("^NUMGRNSIZE", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^NUMGRNSIZE", infile) + 1]), 
        # assign NUMGRNSIZE
      NUMUPLIFTMAPS = if(length(grep("^NUMUPLIFTMAPS", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^NUMUPLIFTMAPS", infile) + 1]), 
        # assign NUMUPLIFTMAPS
      OPT_INCREASE_TO_FRONT = if(length(grep("^OPT_INCREASE_TO_FRONT", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^OPT_INCREASE_TO_FRONT", infile) + 1]), 
        # assign OPT_INCREASE_TO_FRONT
      OPT_NONLINEAR_DIFFUSION = if(length(grep("^OPT_NONLINEAR_DIFFUSION", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^OPT_NONLINEAR_DIFFUSION", infile) + 1]), 
        # assign OPT_NONLINEAR_DIFFUSION
      OPT_PT_PLACE = if(length(grep("^OPT_PT_PLACE", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^OPT_PT_PLACE", infile) + 1]), 
        # assign OPT_PT_PLACE
      OPT_VAR_SIZE = if(length(grep("^OPT_VAR_SIZE", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^OPT_VAR_SIZE", infile) + 1]), 
        # assign OPT_VAR_SIZE
      OPINTRVL = if(length(grep("^OPINTRVL", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^OPINTRVL", infile) + 1]), 
        # assign OPINTRVL
      OPTDETACHLIM = if(length(grep("^OPTDETACHLIM", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^OPTDETACHLIM", infile) + 1]), 
        # assign OPTDETACHLIM
      OPTDIFFDEP = if(length(grep("^OPTDIFFDEP", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^OPTDIFFDEP", infile) + 1]), 
        # assign OPTDIFFDEP
      OPTEXPOSURETIME = if(length(grep("^OPTEXPOSURETIME", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^OPTEXPOSURETIME", infile) + 1]), 
        # assign OPTEXPOSURETIME
      OPTFLOODPLAIN = if(length(grep("^OPTFLOODPLAIN", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^OPTFLOODPLAIN", infile) + 1]), 
        # assign OPTFLOODPLAIN
      OPTFOLDDENS = if(length(grep("^OPTFOLDDENS", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^OPTFOLDDENS", infile) + 1]), 
        # assign OPTFOLDDENS
      OPTINITMESHDENS = if(length(grep("^OPTINITMESHDENS", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^OPTINITMESHDENS", infile) + 1]), 
        # assign OPTINITMESHDENS
      OPTINLET = if(length(grep("^OPTINLET", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^OPTINLET", infile) + 1]), 
        # assign OPTINLET
      OPTINTERPLAYER = if(length(grep("^OPTINTERPLAYER", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^OPTINTERPLAYER", infile) + 1]), 
        # assign OPTINTERPLAYER
      OPTKINWAVE = if(length(grep("^OPTKINWAVE", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^OPTKINWAVE", infile) + 1]), 
        # assign OPTKINWAVE
      OPTLAYEROUTPUT = if(length(grep("^OPTLAYEROUTPUT", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^OPTLAYEROUTPUT", infile) + 1]), 
        # assign OPTLAYEROUTPUT
      OPTLOESSDEP = if(length(grep("^OPTLOESSDEP", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^OPTLOESSDEP", infile) + 1]), 
        # assign OPTLOESSDEP
      OPTMEANDER = if(length(grep("^OPTMEANDER", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^OPTMEANDER", infile) + 1]), 
        # assign OPTMEANDER
      OPTMESHADAPTAREA = if(length(grep("^OPTMESHADAPTAREA", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^OPTMESHADAPTAREA", infile) + 1]), 
        # assign OPTMESHADAPTAREA
      OPTMESHADAPTDZ = if(length(grep("^OPTMESHADAPTDZ", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^OPTMESHADAPTDZ", infile) + 1]), 
        # assign OPTMESHADAPTDZ
      OPTMNDR = if(length(grep("^OPTMNDR", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^OPTMNDR", infile) + 1]), 
        # assign OPTMNDR
      OPTREADINPUT = if(length(grep("^OPTREADINPUT", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^OPTREADINPUT", infile) + 1]), 
        # assign OPTREADINPUT
      OPTREADLAYER = if(length(grep("^OPTREADLAYER", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^OPTREADLAYER", infile) + 1]), 
        # assign OPTREADLAYER
      OPTSINVARINFILT = if(length(grep("^OPTSINVARINFILT", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^OPTSINVARINFILT", infile) + 1]), 
        # assign OPTSINVARINFILT
      OPTSTRATGRID = if(length(grep("^OPTSTRATGRID", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^OPTSTRATGRID", infile) + 1]), 
        # assign OPTSTRATGRID
      OPTTSOUTPUT = if(length(grep("^OPTTSOUTPUT", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^OPTTSOUTPUT", infile) + 1]), 
        # assign OPTTSOUTPUT
      OPTVAR = if(length(grep("^OPTVAR", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^OPTVAR", infile) + 1]), 
        # assign OPTVAR
      OPTVEG = if(length(grep("^OPTVEG", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^OPTVEG", infile) + 1]), 
        # assign OPTVEG
      OUTFILENAME = if(length(grep("^OUTFILENAME", 
        infile) + 1) == 0) as.character(NA) else as.character(
        infile[grep("^OUTFILENAME", infile) + 1]), 
        # assign OUTFILENAME
      OUTLET_X_COORD = if(length(grep("^OUTLET_X_COORD", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^OUTLET_X_COORD", infile) + 1]), 
        # assign OUTLET_X_COORD
      OUTLET_Y_COORD = if(length(grep("^OUTLET_Y_COORD", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^OUTLET_Y_COORD", infile) + 1]), 
        # assign OUTLET_Y_COORD
      PB = if(length(grep("^PB", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^PB", infile) + 1]), 
        # assign PB
      PERIOD_INFILT = if(length(grep("^PERIOD_INFILT", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^PERIOD_INFILT", infile) + 1]), 
        # assign PERIOD_INFILT
      PF = if(length(grep("^PF", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^PF", infile) + 1]), 
        # assign PF
      POINTFILENAME = if(length(grep("^POINTFILENAME", 
        infile) + 1) == 0) as.character(NA) else as.character(
        infile[grep("^POINTFILENAME", infile) + 1]), 
        # assign POINTFILENAME
      RAMPDIP = if(length(grep("^RAMPDIP", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^RAMPDIP", infile) + 1]), 
        # assign RAMPDIP
      RAND_ELEV = if(length(grep("^RAND_ELEV", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^RAND_ELEV", infile) + 1]), 
        # assign RAND_ELEV
      REGINIT = if(length(grep("^REGINIT", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^REGINIT", infile) + 1]), 
        # assign REGINIT
      REGPROPORTION1 = if(length(grep("^REGPROPORTION1", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^REGPROPORTION1", infile) + 1]), 
        # assign REGPROPORTION1
      REGPROPORTION2 = if(length(grep("^REGPROPORTION2", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^REGPROPORTION2", infile) + 1]), 
        # assign REGPROPORTION2
      REGPROPORTION3 = if(length(grep("^REGPROPORTION3", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^REGPROPORTION3", infile) + 1]), 
        # assign REGPROPORTION3
      REGPROPORTION4 = if(length(grep("^REGPROPORTION4", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^REGPROPORTION4", infile) + 1]), 
        # assign REGPROPORTION4
      REGPROPORTION5 = if(length(grep("^REGPROPORTION5", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^REGPROPORTION5", infile) + 1]), 
        # assign REGPROPORTION5
      REGPROPORTION6 = if(length(grep("^REGPROPORTION6", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^REGPROPORTION6", infile) + 1]), 
        # assign REGPROPORTION6
      REGPROPORTION7 = if(length(grep("^REGPROPORTION7", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^REGPROPORTION7", infile) + 1]), 
        # assign REGPROPORTION7
      REGPROPORTION8 = if(length(grep("^REGPROPORTION8", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^REGPROPORTION8", infile) + 1]), 
        # assign REGPROPORTION8
      REGPROPORTION9 = if(length(grep("^REGPROPORTION9", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^REGPROPORTION9", infile) + 1]), 
        # assign REGPROPORTION9
      REGPROPORTION10 = if(length(grep("^REGPROPORTION10", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^REGPROPORTION10", infile) + 1]), 
        # assign REGPROPORTION10
      REGPROPORTION11 = if(length(grep("^REGPROPORTION11", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^REGPROPORTION11", infile) + 1]), 
        # assign REGPROPORTION11
      REGPROPORTION12 = if(length(grep("^REGPROPORTION12", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^REGPROPORTION12", infile) + 1]), 
        # assign REGPROPORTION12
      REGPROPORTION13 = if(length(grep("^REGPROPORTION13", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^REGPROPORTION13", infile) + 1]), 
        # assign REGPROPORTION13
      REGPROPORTION14 = if(length(grep("^REGPROPORTION14", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^REGPROPORTION14", infile) + 1]), 
        # assign REGPROPORTION14
      REGPROPORTION15 = if(length(grep("^REGPROPORTION15", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^REGPROPORTION15", infile) + 1]), 
        # assign REGPROPORTION15
      REGPROPORTION16 = if(length(grep("^REGPROPORTION16", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^REGPROPORTION16", infile) + 1]), 
        # assign REGPROPORTION16
      REGPROPORTION17 = if(length(grep("^REGPROPORTION17", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^REGPROPORTION17", infile) + 1]), 
        # assign REGPROPORTION17
      REGPROPORTION18 = if(length(grep("^REGPROPORTION18", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^REGPROPORTION18", infile) + 1]), 
        # assign REGPROPORTION18
      REGPROPORTION19 = if(length(grep("^REGPROPORTION19", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^REGPROPORTION19", infile) + 1]), 
        # assign REGPROPORTION19
      REGPROPORTION20 = if(length(grep("^REGPROPORTION20", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^REGPROPORTION20", infile) + 1]), 
        # assign REGPROPORTION20
      RUNTIME = if(length(grep("^RUNTIME", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^RUNTIME", infile) + 1]), 
        # assign RUNTIME
      SEED = if(length(grep("^SEED", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^SEED", infile) + 1]), 
        # assign SEED
      SG_MAXREGDEPTH = if(length(grep("^SG_MAXREGDEPTH", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^SG_MAXREGDEPTH", infile) + 1]), 
        # assign SG_MAXREGDEPTH
      SHEAR_RATIO = if(length(grep("^SHEAR_RATIO", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^SHEAR_RATIO", infile) + 1]), 
        # assign SHEAR_RATIO
      SLIPRATE = if(length(grep("^SLIPRATE", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^SLIPRATE", infile) + 1]), 
        # assign SLIPRATE
      SLOPED_SURF = if(length(grep("^SLOPED_SURF", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^SLOPED_SURF", infile) + 1]), 
        # assign SLOPED_SURF
      SOILSTORE = if(length(grep("^SOILSTORE", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^SOILSTORE", infile) + 1]), 
        # assign SOILSTORE
      ST_ISTDUR = if(length(grep("^ST_ISTDUR", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^ST_ISTDUR", infile) + 1]), 
        # assign ST_ISTDUR
      ST_PMEAN = if(length(grep("^ST_PMEAN", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^ST_PMEAN", infile) + 1]), 
        # assign ST_PMEAN
      ST_STDUR = if(length(grep("^ST_STDUR", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^ST_STDUR", infile) + 1]), 
        # assign ST_STDUR
      STARTING_Y_COORD = if(length(grep("^STARTING_Y_COORD", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^STARTING_Y_COORD", infile) + 1]), 
        # assign STARTING_Y_COORD
      SUBSRATE = if(length(grep("^SUBSRATE", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^SUBSRATE", infile) + 1]), 
        # assign SUBSRATE
      SURFER = if(length(grep("^SURFER", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^SURFER", infile) + 1]), 
        # assign SURFER
      TAUCB = if(length(grep("^TAUCB", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^TAUCB", infile) + 1]), 
        # assign TAUCB
      TAUCR = if(length(grep("^TAUCR", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^TAUCR", infile) + 1]), 
        # assign TAUCR
      THETAC = if(length(grep("^THETAC", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^THETAC", infile) + 1]), 
        # assign THETAC
      TIGHTENINGRATE = if(length(grep("^TIGHTENINGRATE", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^TIGHTENINGRATE", infile) + 1]), 
        # assign TIGHTENINGRATE
      TRANSMISSIVITY = if(length(grep("^TRANSMISSIVITY", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^TRANSMISSIVITY", infile) + 1]), 
        # assign TRANSMISSIVITY
      TRANSPORT_LAW = if(length(grep("^TRANSPORT_LAW", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^TRANSPORT_LAW", infile) + 1]), 
        # assign TRANSPORT_LAW
      TSOPINTRVL = if(length(grep("^TSOPINTRVL", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^TSOPINTRVL", infile) + 1]), 
        # assign TSOPINTRVL
      TYPE_BOUND = if(length(grep("^TYPE_BOUND", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^TYPE_BOUND", infile) + 1]), 
        # assign TYPE_BOUND
      UPDUR = if(length(grep("^UPDUR", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^UPDUR", infile) + 1]), 
        # assign UPDUR
      UPLIFT_FRONT_GRADIENT = if(length(grep("^UPLIFT_FRONT_GRADIENT", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^UPLIFT_FRONT_GRADIENT", infile) + 1]), 
        # assign UPLIFT_FRONT_GRADIENT
      UPMAPFILENAME = if(length(grep("^UPMAPFILENAME", 
        infile) + 1) == 0) as.character(NA) else as.character(
        infile[grep("^UPMAPFILENAME", infile) + 1]), 
        # assign UPMAPFILENAME
      UPPER_BOUND_Z = if(length(grep("^UPPER_BOUND_Z", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^UPPER_BOUND_Z", infile) + 1]), 
        # assign UPPER_BOUND_Z
      UPRATE = if(length(grep("^UPRATE", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^UPRATE", infile) + 1]), 
        # assign UPRATE
      UPPERKINKDIP = if(length(grep("^UPPERKINKDIP", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^UPPERKINKDIP", infile) + 1]), 
        # assign UPPERKINKDIP
      UPSUBRATIO = if(length(grep("^UPSUBRATIO", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^UPSUBRATIO", infile) + 1]), 
        # assign UPSUBRATIO
      UPTIMEFILENAME = if(length(grep("^UPTIMEFILENAME", 
        infile) + 1) == 0) as.character(NA) else as.character(
        infile[grep("^UPTIMEFILENAME", infile) + 1]), 
        # assign UPTIMEFILENAME
      UPTYPE = if(length(grep("^UPTYPE", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^UPTYPE", infile) + 1]), 
        # assign UPTYPE
      VERTICAL_THROW = if(length(grep("^VERTICAL_THROW", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^VERTICAL_THROW", infile) + 1]), 
        # assign VERTICAL_THROW
      X_GRID_SIZE = if(length(grep("^X_GRID_SIZE", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^X_GRID_SIZE", infile) + 1]), 
        # assign X_GRID_SIZE
      XCORNER = if(length(grep("^XCORNER", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^XCORNER", infile) + 1]), 
        # assign XCORNER
      Y_GRID_SIZE = if(length(grep("^Y_GRID_SIZE", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^Y_GRID_SIZE", infile) + 1]), 
        # assign Y_GRID_SIZE
      YCORNER = if(length(grep("^YCORNER", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^YCORNER", infile) + 1]), 
        # assign YCORNER
      YFOLDINGSTART = if(length(grep("^YFOLDINGSTART", 
        infile) + 1) == 0) as.numeric(NA) else as.numeric(
        infile[grep("^YFOLDINGSTART", infile) + 1]) # assign YFOLDINGSTART

  )
}