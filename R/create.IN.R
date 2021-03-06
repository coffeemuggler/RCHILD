#' Function to create an empty CHILD input file.
#' 
#' This function creates an S4-object IN from the example data set
#' \code{empty.in()}. Primary use of this function is to create an
#' object-skeleton to modify.
#' 
#' 
#' @return An S4-object with all parameters as separate slots, parameters are
#' set to \code{NA}.
#' @author Michael Dietze
#' @seealso \code{\link{write.IN}}, \code{\link{read.IN}}
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
#' empty.IN <- create.IN()
#' 
#' @export create.IN
create.IN <- function(
  
) {
  
  new("IN", # create IN-object
      ACCEL_REL_UPTIME = as.numeric(NA),
      ANTICLINEXCOORD = as.numeric(NA),
      ANTICLINEYCOORD = as.numeric(NA),
      BANK_ROUGH_COEFF = as.numeric(NA),
      BANK_ROUGH_EXP = as.numeric(NA),
      BANK_ERO = as.numeric(NA),
      BANKFULLEVENT = as.numeric(NA),
      BEDROCKDEPTH = as.numeric(NA),
      BETA = as.numeric(NA),
      BLDIVIDINGLINE = as.numeric(NA),
      BLFALL_UPPER = as.numeric(NA),
      BNKHTDEP = as.numeric(NA),
      BRPROPORTION1 = as.numeric(NA),
      BRPROPORTION2 = as.numeric(NA),
      BRPROPORTION3 = as.numeric(NA),
      BRPROPORTION4 = as.numeric(NA),
      BRPROPORTION5 = as.numeric(NA),
      BRPROPORTION6 = as.numeric(NA),
      BRPROPORTION7 = as.numeric(NA),
      BRPROPORTION8 = as.numeric(NA),
      BRPROPORTION9 = as.numeric(NA),
      BRPROPORTION10 = as.numeric(NA),
      BRPROPORTION11 = as.numeric(NA),
      BRPROPORTION12 = as.numeric(NA),
      BRPROPORTION13 = as.numeric(NA),
      BRPROPORTION14 = as.numeric(NA),
      BRPROPORTION15 = as.numeric(NA),
      BRPROPORTION16 = as.numeric(NA),
      BRPROPORTION17 = as.numeric(NA),
      BRPROPORTION18 = as.numeric(NA),
      BRPROPORTION19 = as.numeric(NA),
      BRPROPORTION20 = as.numeric(NA),
      CHAN_GEOM_MODEL = as.numeric(NA),
      CRITICAL_AREA = as.numeric(NA),
      CRITICAL_SLOPE = as.numeric(NA),
      DECAY_PARAM_UPLIFT = as.numeric(NA),
      DEF_CHAN_DISCR = as.numeric(NA),
      DETACHMENT_LAW = as.numeric(NA),
      DIFFUSIONTHRESHOLD = as.numeric(NA),
      FAULT_PIVOT_DISTANCE = as.numeric(NA),
      FAULTPOS = as.numeric(NA),
      FLATDEPTH = as.numeric(NA),
      FLOWGEN = as.numeric(NA),
      FLOWVELOCITY = as.numeric(NA),
      FOLDLATRATE = as.numeric(NA),
      FOLDPOSITION = as.numeric(NA),
      FOLDPROPRATE = as.numeric(NA),
      FOLDUPRATE = as.numeric(NA),
      FOLDWAVELEN = as.numeric(NA),
      FP_BANKFULLEVENT = as.numeric(NA),
      FP_DRAREAMIN = as.numeric(NA),
      FP_INLET_ELEVATION = as.numeric(NA),
      FP_LAMBDA = as.numeric(NA),
      FP_MU = as.numeric(NA),
      FP_OPTCONTROLCHAN = as.numeric(NA),
      FP_VALDROP = as.numeric(NA),
      FRAC_WID_ADD = as.numeric(NA),
      FRAC_WID_MOVE = as.numeric(NA),
      FRONT_PROP_RATE = as.numeric(NA),
      GRAINDIAM1 = as.numeric(NA),
      GRAINDIAM2 = as.numeric(NA),
      GRAINDIAM3 = as.numeric(NA),
      GRAINDIAM4 = as.numeric(NA),
      GRAINDIAM5 = as.numeric(NA),
      GRAINDIAM6 = as.numeric(NA),
      GRAINDIAM7 = as.numeric(NA),
      GRAINDIAM8 = as.numeric(NA),
      GRAINDIAM9 = as.numeric(NA),
      GRAINDIAM10 = as.numeric(NA),
      GRAINDIAM11 = as.numeric(NA),
      GRAINDIAM12 = as.numeric(NA),
      GRAINDIAM13 = as.numeric(NA),
      GRAINDIAM14 = as.numeric(NA),
      GRAINDIAM15 = as.numeric(NA),
      GRAINDIAM16 = as.numeric(NA),
      GRAINDIAM17 = as.numeric(NA),
      GRAINDIAM18 = as.numeric(NA),
      GRAINDIAM19 = as.numeric(NA),
      GRAINDIAM20 = as.numeric(NA),
      GRID_LENGTH = as.numeric(NA),
      GRID_WIDTH = as.numeric(NA),
      GRID_SPACING = as.numeric(NA),
      GRIDDX = as.numeric(NA),
      HIDINGEXP = as.numeric(NA),
      HYDR_DEP_COEFF_DS = as.numeric(NA),
      HYDR_DEP_EXP_DS = as.numeric(NA),
      HYDR_DEP_EXP_STN = as.numeric(NA),
      HYDR_ROUGH_COEFF_DS = as.numeric(NA),
      HYDR_ROUGH_EXP_DS = as.numeric(NA),
      HYDR_ROUGH_EXP_STN = as.numeric(NA),
      HYDR_WID_COEFF_DS = as.numeric(NA),
      HYDR_WID_EXP_DS = as.numeric(NA),
      HYDR_WID_EXP_STN = as.numeric(NA),
      HYDROSHAPEFAC = as.numeric(NA),
      INDRAREA = as.numeric(NA),
      INFILTRATION = as.numeric(NA),
      INLET_OPTCALCSEDFEED = as.numeric(NA),
      INLET_SLOPE = as.numeric(NA),
      INLET_X = as.numeric(NA),
      INLET_Y = as.numeric(NA),
      INPUTDATAFILE = as.character(NA),
      INPUTTIME = as.numeric(NA),
      INSEDLOAD1 = as.numeric(NA),
      INSEDLOAD2 = as.numeric(NA),
      INSEDLOAD3 = as.numeric(NA),
      INSEDLOAD4 = as.numeric(NA),
      INSEDLOAD5 = as.numeric(NA),
      INSEDLOAD6 = as.numeric(NA),
      INSEDLOAD7 = as.numeric(NA),
      INSEDLOAD8 = as.numeric(NA),
      INSEDLOAD9 = as.numeric(NA),
      INSEDLOAD10 = as.numeric(NA),
      INSEDLOAD11 = as.numeric(NA),
      INSEDLOAD12 = as.numeric(NA),
      INSEDLOAD13 = as.numeric(NA),
      INSEDLOAD14 = as.numeric(NA),
      INSEDLOAD15 = as.numeric(NA),
      INSEDLOAD16 = as.numeric(NA),
      INSEDLOAD17 = as.numeric(NA),
      INSEDLOAD18 = as.numeric(NA),
      INSEDLOAD19 = as.numeric(NA),
      INSEDLOAD20 = as.numeric(NA),
      KB = as.numeric(NA),
      KD = as.numeric(NA),
      KF = as.numeric(NA),
      KINKDIP = as.numeric(NA),
      KINWAVE_HQEXP = as.numeric(NA),
      KR = as.numeric(NA),
      KT = as.numeric(NA),
      LAKEFILL = as.numeric(NA),
      LOESS_DEP_RATE = as.numeric(NA),
      MAXICMEAN = as.numeric(NA),
      MAXREGDEPTH = as.numeric(NA),
      MB = as.numeric(NA),
      MEAN_ELEV = as.numeric(NA),
      MEDIAN_DIAMETER = as.numeric(NA),
      MESHADAPT_MAXNODEFLUX = as.numeric(NA),
      MESHADAPTAREA_MINAREA = as.numeric(NA),
      MESHADAPTAREA_MAXVAREA = as.numeric(NA),
      MF = as.numeric(NA),
      MINIMUM_UPRATE = as.numeric(NA),
      NB = as.numeric(NA),
      NF = as.numeric(NA),
      NUM_PTS = as.numeric(NA),
      NUMGRNSIZE = as.numeric(NA),
      NUMUPLIFTMAPS = as.numeric(NA),
      OPT_INCREASE_TO_FRONT = as.numeric(NA),
      OPT_NONLINEAR_DIFFUSION = as.numeric(NA),
      OPT_PT_PLACE = as.numeric(NA),
      OPT_VAR_SIZE = as.numeric(NA),
      OPINTRVL = as.numeric(NA),
      OPTDETACHLIM = as.numeric(NA),
      OPTDIFFDEP = as.numeric(NA),
      OPTEXPOSURETIME = as.numeric(NA),
      OPTFLOODPLAIN = as.numeric(NA),
      OPTFOLDDENS = as.numeric(NA),
      OPTINITMESHDENS = as.numeric(NA),
      OPTINLET = as.numeric(NA),
      OPTINTERPLAYER = as.numeric(NA),
      OPTKINWAVE = as.numeric(NA),
      OPTLAYEROUTPUT = as.numeric(NA),
      OPTLOESSDEP = as.numeric(NA),
      OPTMEANDER = as.numeric(NA),
      OPTMESHADAPTAREA = as.numeric(NA),
      OPTMESHADAPTDZ = as.numeric(NA),
      OPTMNDR = as.numeric(NA),
      OPTREADINPUT = as.numeric(NA),
      OPTREADLAYER = as.numeric(NA),
      OPTSINVARINFILT = as.numeric(NA),
      OPTSTRATGRID = as.numeric(NA),
      OPTTSOUTPUT = as.numeric(NA),
      OPTVAR = as.numeric(NA),
      OPTVEG = as.numeric(NA),
      OUTFILENAME = as.character(NA),
      OUTLET_X_COORD = as.numeric(NA),
      OUTLET_Y_COORD = as.numeric(NA),
      PB = as.numeric(NA),
      PERIOD_INFILT = as.numeric(NA),
      PF = as.numeric(NA),
      POINTFILENAME = as.character(NA),
      RAMPDIP = as.numeric(NA),
      RAND_ELEV = as.numeric(NA),
      REGINIT = as.numeric(NA),
      REGPROPORTION1 = as.numeric(NA),
      REGPROPORTION2 = as.numeric(NA),
      REGPROPORTION3 = as.numeric(NA),
      REGPROPORTION4 = as.numeric(NA),
      REGPROPORTION5 = as.numeric(NA),
      REGPROPORTION6 = as.numeric(NA),
      REGPROPORTION7 = as.numeric(NA),
      REGPROPORTION8 = as.numeric(NA),
      REGPROPORTION9 = as.numeric(NA),
      REGPROPORTION10 = as.numeric(NA),
      REGPROPORTION11 = as.numeric(NA),
      REGPROPORTION12 = as.numeric(NA),
      REGPROPORTION13 = as.numeric(NA),
      REGPROPORTION14 = as.numeric(NA),
      REGPROPORTION15 = as.numeric(NA),
      REGPROPORTION16 = as.numeric(NA),
      REGPROPORTION17 = as.numeric(NA),
      REGPROPORTION18 = as.numeric(NA),
      REGPROPORTION19 = as.numeric(NA),
      REGPROPORTION20 = as.numeric(NA),
      RUNTIME = as.numeric(NA),
      SEED = as.numeric(NA),
      SG_MAXREGDEPTH = as.numeric(NA),
      SHEAR_RATIO = as.numeric(NA),
      SLIPRATE = as.numeric(NA),
      SLOPED_SURF = as.numeric(NA),
      SOILSTORE = as.numeric(NA),
      ST_ISTDUR = as.numeric(NA),
      ST_PMEAN = as.numeric(NA),
      ST_STDUR = as.numeric(NA),
      STARTING_Y_COORD = as.numeric(NA),
      SUBSRATE = as.numeric(NA),
      SURFER = as.numeric(NA),
      TAUCB = as.numeric(NA),
      TAUCR = as.numeric(NA),
      THETAC = as.numeric(NA),
      TIGHTENINGRATE = as.numeric(NA),
      TRANSMISSIVITY = as.numeric(NA),
      TRANSPORT_LAW = as.numeric(NA),
      TSOPINTRVL = as.numeric(NA),
      TYPE_BOUND = as.numeric(NA),
      UPDUR = as.numeric(NA),
      UPLIFT_FRONT_GRADIENT = as.numeric(NA),
      UPMAPFILENAME = as.character(NA),
      UPPER_BOUND_Z = as.numeric(NA),
      UPPERKINKDIP = as.numeric(NA),
      UPRATE = as.numeric(NA),
      UPSUBRATIO = as.numeric(NA),
      UPTIMEFILENAME = as.character(NA),
      UPTYPE = as.numeric(NA),
      VERTICAL_THROW = as.numeric(NA),
      X_GRID_SIZE = as.numeric(NA),
      XCORNER = as.numeric(NA),
      Y_GRID_SIZE = as.numeric(NA),
      YCORNER = as.numeric(NA),
      YFOLDINGSTART = as.numeric(NA)
      )
}