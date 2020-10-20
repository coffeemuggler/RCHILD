#' Function to write CHILD input files.
#' 
#' This function creates and writes a CHILD input file from a S4-object (IN).
#' 
#' The function only writes input parameters to the *.in-file from slots that
#' are not \code{NA}.
#' 
#' @param IN (character scalar) Name of the S4-object that will be converted.
#' 
#' @param filename (character scalar) File name of the CHILD input file that
#' will be created, with extension.
#' 
#' @return A CHILD input file (*.in).
#' 
#' @author Michael Dietze
#' @seealso \code{\link{read.IN}}
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
#' # write example CHILD input file
#' write.IN(hillslope1.in, filename = "hillslope1")
#' 
#' @export write.IN
write.IN <- function(
  IN,
  filename
){
  
  ## open connection to IN-file
  IN_file <- file(filename, "w")
  
  ## write in-file header information to file
  cat(c("#---------------------------------------------------------- \n",
        "# CHILD input file, written by write.IN(), R-package RCHILD \n",
        "#---------------------------------------------------------- \n",
        "#\n"), sep = "", file = IN_file)
  
  ## write section header to file
  cat(c("#\n",
        "# General model run control parameters                               \n",
        "#---------------------------------------------------------- \n",
        "#\n"), sep = "", file = IN_file)
  
  ## write non-empty parameters to file
  if (is.na(IN@OUTFILENAME) == FALSE) {
    cat(c("OUTFILENAME: MANDATORY, character, output file name of", 
          "the model run", "\n", 
    IN@OUTFILENAME,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@RUNTIME) == FALSE) {
    cat(c("RUNTIME: MANDATORY, numeric, duration of run [a]", "\n", 
          IN@RUNTIME,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@OPINTRVL) == FALSE) {
    cat(c("OPINTRVL: MANDATORY, numeric, output interval [a]", "\n", 
          IN@OPINTRVL,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@SEED) == FALSE) {
    cat(c("SEED: DEPENDS, numeric, random seed for storm sequence",
          "& mesh generation", "\n", 
          IN@SEED,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@SURFER) == FALSE) {
    cat(c("SURFER: OPTIONAL, logical (0/1), Surfer-compatible data", 
          "output format", "\n", 
          IN@SURFER,"\n"), sep = "", file = IN_file)
  }
  
  ## write section header to file
  cat(c("#\n",
        "# Various further parameters                                \n",
        "#---------------------------------------------------------- \n",
        "#\n"), sep = "", file = IN_file) # write sub-section header
  
  ## write non-empty parameters to file
  if (is.na(IN@TSOPINTRVL) == FALSE) {
    cat(c("TSOPINTRVL: MANDATORY, numeric, NO DESCRIPTION PRESENT", "\n", 
          IN@TSOPINTRVL,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@OUTLET_X_COORD) == FALSE) {
    cat(c("OUTLET_X_COORD: OPTIONAL, numeric, x-coordinate of", 
          "single-node outlet", "\n", 
          IN@OUTLET_X_COORD,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@OUTLET_Y_COORD) == FALSE) {
    cat(c("OUTLET_Y_COORD: OPTIONAL, numeric, y-coordinate of" ,
          "single-node outlet", "\n", 
          IN@OUTLET_Y_COORD,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@CRITICAL_SLOPE) == FALSE) {
    cat(c("CRITICAL_SLOPE: DEPENDS, numeric, threshold slope" ,
          "gradient for nonlinear creep law", "\n", 
          IN@CRITICAL_SLOPE,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@OPTMEANDER) == FALSE) {
    cat(c("OPTMEANDER: MANDATORY, logical (0/1), option for", 
          "meandering", "\n", 
          IN@OPTMEANDER,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@OPTMNDR) == FALSE) {
    cat(c("OPTMNDR: MANDATORY, logical (0/1), option for", 
          "meandering", "\n", 
          IN@OPTMNDR,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@OPTDETACHLIM) == FALSE) {
    cat(c("OPTDETACHLIM: MANDATORY, logical (0/1), option", 
          "for detachment-limited erosion only", "\n", 
          IN@OPTDETACHLIM,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@OPTREADLAYER) == FALSE) {
    cat(c("OPTREADLAYER: MANDATORY, logical (0/1), option", 
          "to read layer information (only if reading mesh)", "\n", 
          IN@OPTREADLAYER,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@OPTLAYEROUTPUT) == FALSE) {
    cat(c("OPTLAYEROUTPUT: MANDATORY, logical (0/1), option", 
          "for writing layer information", "\n", 
          IN@OPTLAYEROUTPUT,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@OPTTSOUTPUT) == FALSE) {
    cat(c("OPTTSOUTPUT: MANDATORY, logical (0/1), write further", 
          "output files at each time step", "\n", 
          IN@OPTTSOUTPUT,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@OPTINTERPLAYER) == FALSE) {
    cat(c("OPTINTERPLAYER: MANDATORY, logical (0/1), option for", 
          "layer interpolation due to moved nodes", "\n", 
          IN@OPTINTERPLAYER,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@LAKEFILL) == FALSE) {
    cat(c("LAKEFILL: MANDATORY, optional (0/1), option to", 
          "fill lakes", "\n", 
          IN@LAKEFILL,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@OPTINLET) == FALSE) {
    cat(c("OPTINLET: MANDATORY, logical (0/1), add inlet discharge", 
          "boundary condition", "\n", 
          IN@OPTINLET,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@OPTLOESSDEP) == FALSE) {
    cat(c("OPTLOESSDEP: MANDATORY, logical (0/1), surface", 
          "accumulation of aeolian sediment", "\n", 
          IN@OPTLOESSDEP,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@LOESS_DEP_RATE) == FALSE) {
    cat(c("LOESS_DEP_RATE: OPTIONAL, numeric, accumulation rate of", 
          "aeolian sediment [m/a]", "\n", 
          IN@LOESS_DEP_RATE,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@OPTEXPOSURETIME) == FALSE) {
    cat(c("OPTEXPOSURETIME: MANDATORY, logical (0/1), track", 
          "surface-layer exposure ages", "\n", 
          IN@OPTEXPOSURETIME,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@OPTVEG) == FALSE) {
    cat(c("OPTVEG: MANDATORY, logical (0/1), dynamic vegetation", 
          "growth and erosion", "\n", 
          IN@OPTVEG,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@OPTKINWAVE) == FALSE) {
    cat(c("OPTKINWAVE: MANDATORY, logical (0/1), kinematic-wave", 
          "flow routing (steady, 2D)", "\n", 
          IN@OPTKINWAVE,"\n"), sep = "", file = IN_file)
  }
  
  cat(c("#\n",
        "# Mesh setup parameters                                     \n",
        "#-----------------------------------------------------------\n",
        "#\n"), sep = "", file = IN_file) # write sub-section header
  if (is.na(IN@OPTREADINPUT) == FALSE) {
    cat(c("OPTREADINPUT: MANDATORY, logical, 10 = new mesh, 1 =", 
          "existing mesh, 12 = PTS", "\n", 
          IN@OPTREADINPUT,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@INPUTDATAFILE) == FALSE) {
    cat(c("INPUTDATAFILE: DEPENDS, character, mesh file name to read", 
          "input data from", "\n", 
          IN@INPUTDATAFILE,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@POINTFILENAME) == FALSE) {
    cat(c("POINTFILENAME: DEPENDS, character, PTS file name to read", 
          "mesh data from", "\n", 
          IN@POINTFILENAME,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@INPUTTIME) == FALSE) {
    cat(c("INPUTTIME: DEPENDS, numeric, the time step to read mesh", 
          "or PTS for", "\n", 
          IN@INPUTTIME,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@X_GRID_SIZE) == FALSE) {
    cat(c("X_GRID_SIZE: DEPENDS, numeric, horizontal size of", 
          "grid [m]", "\n", 
          IN@X_GRID_SIZE,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@Y_GRID_SIZE) == FALSE) {
    cat(c("Y_GRID_SIZE: DEPENDS, numeric, vertical size of", 
          "grid [m]", "\n", 
          IN@Y_GRID_SIZE,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@OPT_PT_PLACE) == FALSE) {
    cat(c("OPT_PT_PLACE: DEPENDS, logical, type of node placement;", 
          "0 = unif, 1 = pert, 2 = rand", "\n", 
          IN@OPT_PT_PLACE,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@GRID_SPACING) == FALSE) {
    cat(c("GRID_SPACING: DEPENDS, numeric, mean distance between", 
          "grid nodes [m]", "\n", 
          IN@GRID_SPACING,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@NUM_PTS) == FALSE) {
    cat(c("NUM_PTS: DEPENDS, numeric, for random grid, number of", 
          "points to place", "\n", 
          IN@NUM_PTS,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@OPTINITMESHDENS) == FALSE) {
    cat(c("OPTINITMESHDENS: DEPENDS, numeric, number of mesh", 
          "densifying iterations", "\n", 
          IN@OPTINITMESHDENS,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@MEAN_ELEV) == FALSE) {
    cat(c("MEAN_ELEV: DEPENDS, numeric, initial elevation", "\n", 
          IN@MEAN_ELEV,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@RAND_ELEV) == FALSE) {
    cat(c("RAND_ELEV: DEPENDS, numeric, maximum amplitude of", 
          "random noise", "\n", 
          IN@RAND_ELEV,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@SLOPED_SURF) == FALSE) {
    cat(c("SLOPED_SURF: DEPENDS, logical (0/1), sloping initial", 
          "surface", "\n", 
          IN@SLOPED_SURF,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@UPPER_BOUND_Z) == FALSE) {
    cat(c("UPPER_BOUND_Z: DEPENDS, numeric, elevation along", 
          "upper boundary", "\n", 
          IN@UPPER_BOUND_Z,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@TYPE_BOUND) == FALSE) {
    cat(c("TYPE_BOUND: DEPENDS, logical (0,...,4), type of open", 
          "boundary", "\n", 
          IN@TYPE_BOUND,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@OPTMESHADAPTDZ) == FALSE) {
    cat(c("OPTMESHADAPTDZ: OPTIONAL, logical (0/1), dynamic", 
          "adaptive meshing based on erosion rates", "\n", 
          IN@OPTMESHADAPTDZ,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@MESHADAPT_MAXNODEFLUX) == FALSE) {
    cat(c("MESHADAPT_MAXNODEFLUX: OPTIONAL, numeric, minimum", 
          "volumetric erosion rate to add node", "\n", 
          IN@MESHADAPT_MAXNODEFLUX,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@OPTMESHADAPTAREA) == FALSE) {
    cat(c("OPTMESHADAPTAREA: OPTIONAL, logical (0/1), dynamic", 
          "adaptive meshing based on drainage area", "\n", 
          IN@OPTMESHADAPTAREA,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@MESHADAPTAREA_MINAREA) == FALSE) {
    cat(c("MESHADAPTAREA_MINAREA: OPTIONAL, numeric, minimum", 
          "drainage area for adaptive re-meshing", "\n", 
          IN@MESHADAPTAREA_MINAREA,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@MESHADAPTAREA_MAXVAREA) == FALSE) {
    cat(c("MESHADAPTAREA_MAXVAREA: OPTIONAL, numeric, maximum", 
          "Voronoi area", "\n", 
          IN@MESHADAPTAREA_MAXVAREA,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@OPTFOLDDENS) == FALSE) {
    cat(c("OPTFOLDDENS: OPTIONAL, logical (0/1), mesh densification", 
          "around a growing fold", "\n", 
          IN@OPTFOLDDENS,"\n"), sep = "", file = IN_file)
  }

  cat(c("#\n",
        "# Climate parameters                                        \n",
        "#---------------------------------------------------------- \n",
        "#\n"), sep = "", file = IN_file) # write sub-section header
  if (is.na(IN@OPTVAR) == FALSE) {
    cat(c("OPTVAR: MANDATORY, logical (0/1), option for rainfall", 
          "variation", "\n", 
          IN@OPTVAR,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@ST_PMEAN) == FALSE) {
    cat(c("ST_PMEAN: MANDATORY, numeric, mean annual precipitation", 
          "(m)", "\n", 
          IN@ST_PMEAN,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@ST_STDUR) == FALSE) {
    cat(c("ST_STDUR: MANDATORY, numeric, mean storm duration (a)", "\n", 
          IN@ST_STDUR,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@ST_ISTDUR) == FALSE) {
    cat(c("ST_ISTDUR: MANDATORY, numeric, mean time between storms", 
          "(a)", "\n", 
          IN@ST_ISTDUR,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@TRANSMISSIVITY) == FALSE) {
    cat(c("TRANSMISSIVITY: DEPENDS, logical (0/1), option for shallow", 
          "subsurface flow", "\n", 
          IN@TRANSMISSIVITY,"\n"), sep = "", file = IN_file)
  }

  cat(c("#\n",
        "# Flow generation parameters                                \n",
        "#---------------------------------------------------------- \n",
        "#\n"), sep = "", file = IN_file) # write sub-section header
  if (is.na(IN@FLOWGEN) == FALSE) {
    cat(c("FLOWGEN: MANDATORY, logical (0,...,3), option for flow", 
          "generation", "\n", 
          IN@FLOWGEN,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@INFILTRATION) == FALSE) {
    cat(c("INFILTRATION: DEPENDS, numeric, infiltration capacity for", 
          "Hortonian flow [m/a]", "\n", 
          IN@INFILTRATION,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@OPTSINVARINFILT) == FALSE) {
    cat(c("OPTSINVARINFILT: OPTIONAL, logical (0/1), sinusoidal", 
          "infiltration capacity variation", "\n", 
          IN@OPTSINVARINFILT,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@PERIOD_INFILT) == FALSE) {
    cat(c("PERIOD_INFILT: DEPENDS, numeric, period for sinusoidal", 
          "soil infiltration capacity variations [a]", "\n", 
          IN@PERIOD_INFILT,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@MAXICMEAN) == FALSE) {
    cat(c("MAXICMEAN: DEPENDS, numeric, maximum sinusoidity for", 
          "soil infiltration capacity", "\n", 
          IN@MAXICMEAN,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@SOILSTORE) == FALSE) {
    cat(c("SOILSTORE: DEPENDS, numeric, soil water storage", 
          "capacity [m]", "\n", 
          IN@SOILSTORE,"\n"), sep = "", file = IN_file)
  }
  
  cat(c("#\n",
        "# Hydraulic geometry parameters                             \n",
        "#---------------------------------------------------------- \n",
        "#\n"), sep = "", file = IN_file) # write sub-section header
  if (is.na(IN@CHAN_GEOM_MODEL) == FALSE) {
    cat(c("CHAN_GEOM_MODEL: MANDATORY, logical (1,...,9) channel width", 
          "closure option", "\n", 
          IN@CHAN_GEOM_MODEL,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@HYDR_WID_COEFF_DS) == FALSE) {
    cat(c("HYDR_WID_COEFF_DS: MANDATORY, numeric, coefficient on", 
          "downstream hydraulic width relation", "\n", 
          IN@HYDR_WID_COEFF_DS,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@HYDR_WID_EXP_DS) == FALSE) {
    cat(c("HYDR_WID_EXP_DS: MANDATORY, numeric, exponent on", 
          "downstream hydraulic width relation ", "\n", 
          IN@HYDR_WID_EXP_DS,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@HYDR_WID_EXP_STN) == FALSE) {
    cat(c("HYDR_WID_EXP_STN: MANDATORY, numeric, exponent on", 
          "at-a-station hydraulic width relation", "\n", 
          IN@HYDR_WID_EXP_STN,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@HYDR_DEP_COEFF_DS) == FALSE) {
    cat(c("HYDR_DEP_COEFF_DS: MANDATORY, numeric, coefficient", 
          "on downstream hydraulic depth relation", "\n", 
          IN@HYDR_DEP_COEFF_DS,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@HYDR_DEP_EXP_DS) == FALSE) {
    cat(c("HYDR_DEP_EXP_DS: MANDATORY, numeric, exponent on", 
          "downstream hydraulic depth relation", "\n", 
          IN@HYDR_DEP_EXP_DS,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@HYDR_DEP_EXP_STN) == FALSE) {
    cat(c("HYDR_DEP_EXP_STN: MANDATORY, numeric, exponent on", 
          "at-a-station hydraulic depth relation", "\n", 
          IN@HYDR_DEP_EXP_STN,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@HYDR_ROUGH_COEFF_DS) == FALSE) {
    cat(c("HYDR_ROUGH_COEFF_DS: MANDATORY, numeric, coefficient", 
          "on downstrm hydraulic roughness relation", "\n", 
          IN@HYDR_ROUGH_COEFF_DS,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@HYDR_ROUGH_EXP_DS) == FALSE) {
    cat(c("HYDR_ROUGH_EXP_DS: MANDATORY, numeric, exponent on", 
          "downstream hydraulic roughness", "\n", 
          IN@HYDR_ROUGH_EXP_DS,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@HYDR_ROUGH_EXP_STN) == FALSE) {
    cat(c("HYDR_ROUGH_EXP_STN: MANDATORY, numeric, exponent on", 
          "at-a-station hydraulic roughness", "\n", 
          IN@HYDR_ROUGH_EXP_STN,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@BANK_ROUGH_COEFF) == FALSE) {
    cat(c("BANK_ROUGH_COEFF: DEPENDS, numeric, coefficient on", 
          "downstream bank roughness relation", "\n", 
          IN@BANK_ROUGH_COEFF,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@BANK_ROUGH_EXP) == FALSE) {
    cat(c("BANK_ROUGH_EXP: DEPENDS, numeric, exponent on", 
          "discharge for downstream bank roughness", "\n", 
          IN@BANK_ROUGH_EXP,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@BANK_ERO) == FALSE) {
    cat(c("BANK_ERO: DEPENDS, numeric, stream-bank erodibility", 
          "coefficient", "\n", 
          IN@BANK_ERO,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@BNKHTDEP) == FALSE) {
    cat(c("BNKHTDEP: DEPENDS, numeric, bank erosion rate vs.", 
          "bank height", "\n", 
          IN@BNKHTDEP,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@OPT_VAR_SIZE) == FALSE) {
    cat(c("OPT_VAR_SIZE: OPTIONAL, logical (0/1), use of", 
          "multiple grain sizes in meanders", "\n", 
          IN@OPT_VAR_SIZE,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@MEDIAN_DIAMETER) == FALSE) {
    cat(c("MEDIAN_DIAMETER: DEPENDS, numeric, median", 
          "bed-sediment grain diameter [m]", "\n", 
          IN@MEDIAN_DIAMETER,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@CRITICAL_AREA) == FALSE) {
    cat(c("CRITICAL_AREA: DEPENDS, numeric, minimum drainage", 
          "area for a meandering channel", "\n", 
          IN@CRITICAL_AREA,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@DEF_CHAN_DISCR) == FALSE) {
    cat(c("DEF_CHAN_DISCR: DEPENDS, numeric, default channel", 
          "node spacing", "\n", 
          IN@DEF_CHAN_DISCR,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@FRAC_WID_ADD) == FALSE) {
    cat(c("FRAC_WID_ADD: DEPENDS, numeric, maximum distance of", 
          "a meandering channel node", "\n", 
          IN@FRAC_WID_ADD,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@FRAC_WID_MOVE) == FALSE) {
    cat(c("FRAC_WID_MOVE: DEPENDS, numeric, maximum meander", 
          "node migration per time step", "\n", 
          IN@FRAC_WID_MOVE,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@BANKFULLEVENT) == FALSE) {
    cat(c("BANKFULLEVENT: MANDATORY, numeric, precipitation", 
          "rate of a bankfull event [m/a]", "\n", 
          IN@BANKFULLEVENT,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@FLOWVELOCITY) == FALSE) {
    cat(c("FLOWVELOCITY: DEPENDS, numeric, speed of channel", 
          "flow", "\n", 
          IN@FLOWVELOCITY,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@SHEAR_RATIO) == FALSE) {
    cat(c("SHEAR_RATIO: DEPENDS, numeric, ratio of actual to", 
          "threshold shear stress", "\n", 
          IN@SHEAR_RATIO,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@THETAC) == FALSE) {
    cat(c("THETAC: DEPENDS, numeric, critical Shields stress", "\n", 
          IN@THETAC,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@HYDROSHAPEFAC) == FALSE) {
    cat(c("HYDROSHAPEFAC: DEPENDS, numeric, hydrograph shape", 
          "factor", "\n", 
          IN@HYDROSHAPEFAC,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@INDRAREA) == FALSE) {
    cat(c("INDRAREA: DEPENDS, numeric, drainage area of inlet", 
          "stream [sq.m]", "\n", 
          IN@INDRAREA,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@INLET_OPTCALCSEDFEED) == FALSE) {
    cat(c("INLET_OPTCALCSEDFEED: DEPENDS, logical (0/1), calculate", 
          "slope-based sediment input at inlet", "\n", 
          IN@INLET_OPTCALCSEDFEED,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@INLET_SLOPE) == FALSE) {
    cat(c("INLET_SLOPE: DEPENDS, numeric, slope to calculate inlet", 
          "sediment discharge", "\n", 
          IN@INLET_SLOPE,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@INLET_X) == FALSE) {
    cat(c("INLET_X: DEPENDS, numeric, inlet x-coordinate [m]", "\n", 
          IN@INLET_X,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@INLET_Y) == FALSE) {
    cat(c("INLET_Y: DEPENDS, numeric, inlet y-coordinate [m]", "\n", 
          IN@INLET_Y,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@KINWAVE_HQEXP) == FALSE) {
    cat(c("KINWAVE_HQEXP: DEPENDS, numeric, exponent on", 
          "depth-discharge relationship", "\n", 
          IN@KINWAVE_HQEXP,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@INSEDLOAD1) == FALSE) {
    cat(c("INSEDLOAD1: DEPENDS, numeric, inlet sediment load of", 
          "grain size 1", "\n", 
          IN@INSEDLOAD1,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@INSEDLOAD2) == FALSE) {
    cat(c("INSEDLOAD2: DEPENDS, numeric, inlet sediment load", 
          "of grain size 2", "\n", 
          IN@INSEDLOAD2,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@INSEDLOAD3) == FALSE) {
    cat(c("INSEDLOAD3: DEPENDS, numeric, inlet sediment load", 
          "of grain size 3", "\n", 
          IN@INSEDLOAD3,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@INSEDLOAD4) == FALSE) {
    cat(c("INSEDLOAD4: DEPENDS, numeric, inlet sediment load", 
          "of grain size 4", "\n", 
          IN@INSEDLOAD4,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@INSEDLOAD5) == FALSE) {
    cat(c("INSEDLOAD5: DEPENDS, numeric, inlet sediment load", 
          "of grain size 5", "\n", 
          IN@INSEDLOAD5,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@INSEDLOAD6) == FALSE) {
    cat(c("INSEDLOAD6: DEPENDS, numeric, inlet sediment load", 
          "of grain size 6", "\n", 
          IN@INSEDLOAD6,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@INSEDLOAD7) == FALSE) {
    cat(c("INSEDLOAD7: DEPENDS, numeric, inlet sediment load", 
          "of grain size 7", "\n", 
          IN@INSEDLOAD7,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@INSEDLOAD8) == FALSE) {
    cat(c("INSEDLOAD8: DEPENDS, numeric, inlet sediment load", 
          "of grain size 8", "\n", 
          IN@INSEDLOAD8,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@INSEDLOAD9) == FALSE) {
    cat(c("INSEDLOAD9: DEPENDS, numeric, inlet sediment load", 
          "of grain size 9", "\n", 
          IN@INSEDLOAD9,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@INSEDLOAD10) == FALSE) {
    cat(c("INSEDLOAD10: DEPENDS, numeric, inlet sediment load", 
          "of grain size 10", "\n", 
          IN@INSEDLOAD10,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@INSEDLOAD11) == FALSE) {
    cat(c("INSEDLOAD11: DEPENDS, numeric, inlet sediment load", 
          "of grain size 11", "\n", 
          IN@INSEDLOAD11,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@INSEDLOAD12) == FALSE) {
    cat(c("INSEDLOAD12: DEPENDS, numeric, inlet sediment load", 
          "of grain size 12", "\n", 
          IN@INSEDLOAD12,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@INSEDLOAD13) == FALSE) {
    cat(c("INSEDLOAD13: DEPENDS, numeric, inlet sediment load", 
          "of grain size 13", "\n", 
          IN@INSEDLOAD13,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@INSEDLOAD14) == FALSE) {
    cat(c("INSEDLOAD14: DEPENDS, numeric, inlet sediment load", 
          "of grain size 14", "\n", 
          IN@INSEDLOAD14,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@INSEDLOAD15) == FALSE) {
    cat(c("INSEDLOAD15: DEPENDS, numeric, inlet sediment load", 
          "of grain size 15", "\n", 
          IN@INSEDLOAD15,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@INSEDLOAD16) == FALSE) {
    cat(c("INSEDLOAD16: DEPENDS, numeric, inlet sediment load", 
          "of grain size 16", "\n", 
          IN@INSEDLOAD16,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@INSEDLOAD17) == FALSE) {
    cat(c("INSEDLOAD17: DEPENDS, numeric, inlet sediment load", 
          "of grain size 17", "\n", 
          IN@INSEDLOAD17,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@INSEDLOAD18) == FALSE) {
    cat(c("INSEDLOAD18: DEPENDS, numeric, inlet sediment load", 
          "of grain size 18", "\n", 
          IN@INSEDLOAD18,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@INSEDLOAD19) == FALSE) {
    cat(c("INSEDLOAD19: DEPENDS, numeric, inlet sediment load", 
          "of grain size 19", "\n", 
          IN@INSEDLOAD19,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@INSEDLOAD20) == FALSE) {
    cat(c("INSEDLOAD20: DEPENDS, numeric, inlet sediment load", 
          "of grain size 20", "\n", 
          IN@INSEDLOAD20,"\n"), sep = "", file = IN_file)
  }
  
  cat(c("#\n",
        "# Floodplain option parameters                              \n",
        "#---------------------------------------------------------- \n",
        "#\n"), sep = "", file = IN_file) # write sub-section header
  if (is.na(IN@OPTFLOODPLAIN) == FALSE) {
    cat(c("OPTFLOODPLAIN: MANDATORY, logical (0/1), option for", 
          "overbank deposition modelling", "\n", 
          IN@OPTFLOODPLAIN,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@FP_BANKFULLEVENT) == FALSE) {
    cat(c("FP_BANKFULLEVENT: DEPENDS, numeric, minimum runoff rate", 
          "for flood generation", "\n", 
          IN@FP_BANKFULLEVENT,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@FP_DRAREAMIN) == FALSE) {
    cat(c("FP_DRAREAMIN: DEPENDS, numeric, minimum major channel", 
          "drainage area", "\n", 
          IN@FP_DRAREAMIN,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@FP_INLET_ELEVATION) == FALSE) {
    cat(c("FP_INLET_ELEVATION: DEPENDS, numeric, altitude of main", 
          "channel inlet", "\n", 
          IN@FP_INLET_ELEVATION,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@FP_LAMBDA) == FALSE) {
    cat(c("FP_LAMBDA: DEPENDS, numeric, sedimentation rate distance", 
          "decay coefficient", "\n", 
          IN@FP_LAMBDA,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@FP_MU) == FALSE) {
    cat(c("FP_MU: DEPENDS, numeric, rate coefficient for overbank", 
          "sedimentation", "\n", 
          IN@FP_MU,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@FP_OPTCONTROLCHAN) == FALSE) {
    cat(c("FP_OPTCONTROLCHAN: DEPENDS, logical (0/1), main channel", 
          "altitude as boundary", "\n", 
          IN@FP_OPTCONTROLCHAN,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@FP_VALDROP) == FALSE) {
    cat(c("FP_VALDROP: DEPENDS, numeric, main channel inlet-outlet", 
          "altitude difference", "\n", 
          IN@FP_VALDROP,"\n"), sep = "", file = IN_file)
  }
  
 cat(c("#\n",
        "# Erosion and sediment transport parameters                 \n",
        "#---------------------------------------------------------- \n",
        "#\n"), sep = "", file = IN_file) # write sub-section header
  if (is.na(IN@DETACHMENT_LAW) == FALSE) {
    cat(c("DETACHMENT_LAW: MANDATORY, logical (0,1), detachment law", 
          "formula", "\n", 
          IN@DETACHMENT_LAW,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@MB) == FALSE) {
    cat(c("MB: MANDATORY, numeric, bedrock erodibility specific", 
          "discharge exponent", "\n", 
          IN@MB,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@NB) == FALSE) {
    cat(c("NB: MANDATORY, numeric, bedrock erodibility slope", 
          "exponent", "\n", 
          IN@NB,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@PB) == FALSE) {
    cat(c("PB: MANDATORY, numeric, exponent of excess erosion", 
          "capacity", "\n", 
          IN@PB,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@TAUCB) == FALSE) {
    cat(c("TAUCB: MANDATORY, numeric, critical shear stress for", 
          "bedrock detachment-limited-erosion", "\n", 
          IN@TAUCB,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@TAUCR) == FALSE) {
    cat(c("TAUCR: MANDATORY, numeric, critical shear stress for", 
          "regolith detachment-limited-erosion", "\n", 
          IN@TAUCR,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@TRANSPORT_LAW) == FALSE) {
    cat(c("TRANSPORT_LAW: MANDATORY, logical (0,...,6), transport", 
          "law formula", "\n", 
          IN@TRANSPORT_LAW,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@KF) == FALSE) {
    cat(c("KF: DEPENDS, numeric, sediment transport efficiency", 
          "factor", "\n", 
          IN@KF,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@MF) == FALSE) {
    cat(c("MF: DEPENDS, numeric, sediment transport capacity", 
          "discharge exponent", "\n", 
          IN@MF,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@NF) == FALSE) {
    cat(c("NF: DEPENDS, numeric, sediment transport capacity", 
          "slope exponent", "\n", 
          IN@NF,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@PF) == FALSE) {
    cat(c("PF: DEPENDS, numeric, excess shear stress exponent", "\n", 
          IN@PF,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@KB) == FALSE) {
    cat(c("KB: DEPENDS, numeric, bedrock erodibility coefficient", "\n", 
          IN@KB,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@KR) == FALSE) {
    cat(c("KR: DEPENDS, numeric, regolith erodibility coefficient", "\n", 
          IN@KR,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@KT) == FALSE) {
    cat(c("KT: DEPENDS, numeric, Shear stress (or stream power)", 
          "coefficient", "\n", 
          IN@KT,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@KD) == FALSE) {
    cat(c("KD: MANDATORY, numeric, hillslope diffusivity", 
          "coeficient [sqm/a]", "\n", 
          IN@KD,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@OPT_NONLINEAR_DIFFUSION) == FALSE) {
    cat(c("OPT_NONLINEAR_DIFFUSION: OPTIONAL, logical (0/1),", 
          "nonlinear soil creep diffusion model", "\n", 
          IN@OPT_NONLINEAR_DIFFUSION,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@DIFFUSIONTHRESHOLD) == FALSE) {
    cat(c("DIFFUSIONTHRESHOLD: MANDATORY, numeric, zero to disable,", 
          "drainage area above which diffusie creep is zero", "\n", 
          IN@DIFFUSIONTHRESHOLD,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@OPTDIFFDEP) == FALSE) {
    cat(c("OPTDIFFDEP: MANDATORY, logical (0/1) 0 = diffusion erodes", 
          "and deposits, 1 = diffusion only erodes", "\n", 
          IN@OPTDIFFDEP,"\n"), sep = "", file = IN_file)
  }

  cat(c("#\n",
        "# Bedrock and regolith parameters                           \n",
        "#---------------------------------------------------------- \n",
        "#\n"), sep = "", file = IN_file) # write sub-section header
  if (is.na(IN@BEDROCKDEPTH) == FALSE) {
    cat(c("BEDROCKDEPTH: MANDATORY, numeric, initial depth of", 
          "bedrock [m]", "\n", 
          IN@BEDROCKDEPTH,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@REGINIT) == FALSE) {
    cat(c("REGINIT: MANDATORY, numeric, initial regolith", 
          "thickness [m]", "\n", 
          IN@REGINIT,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@MAXREGDEPTH) == FALSE) {
    cat(c("MAXREGDEPTH: MANDATORY, numeric, maximum depth of a", 
          "single regolith layer [m]", "\n", 
          IN@MAXREGDEPTH,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@XCORNER) == FALSE) {
    cat(c("XCORNER: DEPENDS, numeric, x-coordinate of corner of", 
          "stratigraphy grid [m]", "\n", 
          IN@XCORNER,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@YCORNER) == FALSE) {
    cat(c("YCORNER: DEPENDS, numeric, y-coordinate of corner of", 
          "stratigraphy grid [m]", "\n", 
          IN@YCORNER,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@GRID_LENGTH) == FALSE) {
    cat(c("GRID_LENGTH: DEPENDS, numeric, stratigraphy grid length", 
          "[m]", "\n", 
          IN@GRID_LENGTH,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@GRID_WIDTH) == FALSE) {
    cat(c("GRID_WIDTH: DEPENDS, numeric, stratigraphy grid width", 
          "[m]", "\n", 
          IN@GRID_WIDTH,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@GRIDDX) == FALSE) {
    cat(c("GRIDDX: DEPENDS, numeric, stratigraphy grid spacing", 
          "[m]", "\n", 
          IN@GRIDDX,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@SG_MAXREGDEPTH) == FALSE) {
    cat(c("SG_MAXREGDEPTH: DEPENDS, stratigraphic layer thickness", 
          "[m]", "\n", 
          IN@SG_MAXREGDEPTH,"\n"), sep = "", file = IN_file)
  }

  cat(c("#\n",
        "# Grain size parameters                                     \n",
        "#---------------------------------------------------------- \n",
        "#\n"), sep = "", file = IN_file) # write sub-section header
  if (is.na(IN@NUMGRNSIZE) == FALSE) {
    cat(c("NUMGRNSIZE: MANDATORY, numeric, number of grain size", 
          "classes", "\n", 
          IN@NUMGRNSIZE,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@GRAINDIAM1) == FALSE) {
    cat(c("GRAINDIAM1: MANDATORY, numeric, representative diameter", 
          "of grain size class 1 [m] ", "\n", 
          IN@GRAINDIAM1,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@REGPROPORTION1) == FALSE) {
    cat(c("REGPROPORTION1: MANDATORY, numeric, proportion of", 
          "sediments of grain size 1 in regolith", "\n", 
          IN@REGPROPORTION1,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@BRPROPORTION1) == FALSE) {
    cat(c("BRPROPORTION1: MANDATORY, numeric, proportion of", 
          "sediments of grain size 1 in bedrock", "\n", 
          IN@BRPROPORTION1,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@GRAINDIAM2) == FALSE) {
    cat(c("GRAINDIAM2: DEPENDS, numeric, representative diameter", 
          "of grain size class 2 [m] ", "\n", 
          IN@GRAINDIAM2,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@REGPROPORTION2) == FALSE) {
    cat(c("REGPROPORTION2: DEPENDS, numeric, proportion of", 
          "sediments of grain size 2 in regolith", "\n", 
          IN@REGPROPORTION2,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@BRPROPORTION2) == FALSE) {
    cat(c("BRPROPORTION2: DEPENDS, numeric, proportion of", 
          "sediments of grain size 2 in bedrock", "\n", 
          IN@BRPROPORTION2,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@GRAINDIAM3) == FALSE) {
    cat(c("GRAINDIAM3: DEPENDS, numeric, representative diameter", 
          "of grain size class 3 [m] ", "\n", 
          IN@GRAINDIAM3,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@REGPROPORTION3) == FALSE) {
    cat(c("REGPROPORTION3: DEPENDS, numeric, proportion of", 
          "sediments of grain size 3 in regolith", "\n", 
          IN@REGPROPORTION3,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@BRPROPORTION3) == FALSE) {
    cat(c("BRPROPORTION3: DEPENDS, numeric, proportion of", 
          "sediments of grain size 3 in bedrock", "\n", 
          IN@BRPROPORTION3,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@GRAINDIAM4) == FALSE) {
    cat(c("GRAINDIAM4: DEPENDS, numeric, representative diameter", 
          "of grain size class 4 [m] ", "\n", 
          IN@GRAINDIAM4,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@REGPROPORTION4) == FALSE) {
    cat(c("REGPROPORTION4: DEPENDS, numeric, proportion of", 
          "sediments of grain size 4 in regolith", "\n", 
          IN@REGPROPORTION4,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@BRPROPORTION4) == FALSE) {
    cat(c("BRPROPORTION4: DEPENDS, numeric, proportion of", 
          "sediments of grain size 4 in bedrock", "\n", 
          IN@BRPROPORTION4,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@GRAINDIAM5) == FALSE) {
    cat(c("GRAINDIAM5: DEPENDS, numeric, representative diameter", 
          "of grain size class 5 [m] ", "\n", 
          IN@GRAINDIAM5,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@REGPROPORTION5) == FALSE) {
    cat(c("REGPROPORTION5: DEPENDS, numeric, proportion of", 
          "sediments of grain size 5 in regolith", "\n", 
          IN@REGPROPORTION5,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@BRPROPORTION5) == FALSE) {
    cat(c("BRPROPORTION5: DEPENDS, numeric, proportion of", 
          "sediments of grain size 5 in bedrock", "\n", 
          IN@BRPROPORTION5,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@GRAINDIAM6) == FALSE) {
    cat(c("GRAINDIAM6: DEPENDS, numeric, representative diameter", 
          "of grain size class 6 [m] ", "\n", 
          IN@GRAINDIAM6,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@REGPROPORTION6) == FALSE) {
    cat(c("REGPROPORTION6: DEPENDS, numeric, proportion of", 
          "sediments of grain size 6 in regolith", "\n", 
          IN@REGPROPORTION6,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@BRPROPORTION6) == FALSE) {
    cat(c("BRPROPORTION6: DEPENDS, numeric, proportion of", 
          "sediments of grain size 6 in bedrock", "\n", 
          IN@BRPROPORTION6,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@GRAINDIAM7) == FALSE) {
    cat(c("GRAINDIAM7: DEPENDS, numeric, representative diameter", 
          "of grain size class 7 [m] ", "\n", 
          IN@GRAINDIAM7,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@REGPROPORTION7) == FALSE) {
    cat(c("REGPROPORTION7: DEPENDS, numeric, proportion of", 
          "sediments of grain size 7 in regolith", "\n", 
          IN@REGPROPORTION7,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@BRPROPORTION7) == FALSE) {
    cat(c("BRPROPORTION7: DEPENDS, numeric, proportion of", 
          "sediments of grain size 7 in bedrock", "\n", 
          IN@BRPROPORTION7,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@GRAINDIAM8) == FALSE) {
    cat(c("GRAINDIAM8: DEPENDS, numeric, representative diameter", 
          "of grain size class 8 [m] ", "\n", 
          IN@GRAINDIAM8,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@REGPROPORTION8) == FALSE) {
    cat(c("REGPROPORTION8: DEPENDS, numeric, proportion of", 
          "sediments of grain size 8 in regolith", "\n", 
          IN@REGPROPORTION8,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@BRPROPORTION8) == FALSE) {
    cat(c("BRPROPORTION8: DEPENDS, numeric, proportion of", 
          "sediments of grain size 8 in bedrock", "\n", 
          IN@BRPROPORTION8,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@GRAINDIAM9) == FALSE) {
    cat(c("GRAINDIAM9: DEPENDS, numeric, representative diameter", 
          "of grain size class 9 [m] ", "\n", 
          IN@GRAINDIAM9,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@REGPROPORTION9) == FALSE) {
    cat(c("REGPROPORTION9: DEPENDS, numeric, proportion of", 
          "sediments of grain size 9 in regolith", "\n", 
          IN@REGPROPORTION9,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@BRPROPORTION9) == FALSE) {
    cat(c("BRPROPORTION9: DEPENDS, numeric, proportion of", 
          "sediments of grain size 9 in bedrock", "\n", 
          IN@BRPROPORTION9,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@GRAINDIAM10) == FALSE) {
    cat(c("GRAINDIAM10: DEPENDS, numeric, representative diameter", 
          "of grain size class 10 [m] ", "\n", 
          IN@GRAINDIAM10,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@REGPROPORTION10) == FALSE) {
    cat(c("REGPROPORTION10: DEPENDS, numeric, proportion of", 
          "sediments of grain size 10 in regolith", "\n", 
          IN@REGPROPORTION10,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@BRPROPORTION10) == FALSE) {
    cat(c("BRPROPORTION10: DEPENDS, numeric, proportion of", 
          "sediments of grain size 10 in bedrock", "\n", 
          IN@BRPROPORTION10,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@GRAINDIAM11) == FALSE) {
    cat(c("GRAINDIAM11: DEPENDS, numeric, representative diameter", 
          "of grain size class 11 [m] ", "\n", 
          IN@GRAINDIAM11,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@REGPROPORTION11) == FALSE) {
    cat(c("REGPROPORTION11: DEPENDS, numeric, proportion of", 
          "sediments of grain size 11 in regolith", "\n", 
          IN@REGPROPORTION11,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@BRPROPORTION11) == FALSE) {
    cat(c("BRPROPORTION11: DEPENDS, numeric, proportion of", 
          "sediments of grain size 11 in bedrock", "\n", 
          IN@BRPROPORTION11,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@GRAINDIAM12) == FALSE) {
    cat(c("GRAINDIAM12: DEPENDS, numeric, representative diameter", 
          "of grain size class 12 [m] ", "\n", 
          IN@GRAINDIAM12,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@REGPROPORTION12) == FALSE) {
    cat(c("REGPROPORTION12: DEPENDS, numeric, proportion of", 
          "sediments of grain size 12 in regolith", "\n", 
          IN@REGPROPORTION12,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@BRPROPORTION12) == FALSE) {
    cat(c("BRPROPORTION12: DEPENDS, numeric, proportion of", 
          "sediments of grain size 12 in bedrock", "\n", 
          IN@BRPROPORTION12,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@GRAINDIAM13) == FALSE) {
    cat(c("GRAINDIAM13: DEPENDS, numeric, representative diameter", 
          "of grain size class 13 [m] ", "\n", 
          IN@GRAINDIAM13,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@REGPROPORTION13) == FALSE) {
    cat(c("REGPROPORTION13: DEPENDS, numeric, proportion of", 
          "sediments of grain size 13 in regolith", "\n", 
          IN@REGPROPORTION13,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@BRPROPORTION13) == FALSE) {
    cat(c("BRPROPORTION13: DEPENDS, numeric, proportion of", 
          "sediments of grain size 13 in bedrock", "\n", 
          IN@BRPROPORTION13,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@GRAINDIAM14) == FALSE) {
    cat(c("GRAINDIAM14: DEPENDS, numeric, representative diameter", 
          "of grain size class 14 [m] ", "\n", 
          IN@GRAINDIAM14,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@REGPROPORTION14) == FALSE) {
    cat(c("REGPROPORTION14: DEPENDS, numeric, proportion of", 
          "sediments of grain size 14 in regolith", "\n", 
          IN@REGPROPORTION14,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@BRPROPORTION14) == FALSE) {
    cat(c("BRPROPORTION14: DEPENDS, numeric, proportion of", 
          "sediments of grain size 14 in bedrock", "\n", 
          IN@BRPROPORTION14,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@GRAINDIAM15) == FALSE) {
    cat(c("GRAINDIAM15: DEPENDS, numeric, representative diameter", 
          "of grain size class 15 [m] ", "\n", 
          IN@GRAINDIAM15,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@REGPROPORTION15) == FALSE) {
    cat(c("REGPROPORTION15: DEPENDS, numeric, proportion of", 
          "sediments of grain size 15 in regolith", "\n", 
          IN@REGPROPORTION15,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@BRPROPORTION15) == FALSE) {
    cat(c("BRPROPORTION15: DEPENDS, numeric, proportion of", 
          "sediments of grain size 15 in bedrock", "\n", 
          IN@BRPROPORTION15,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@GRAINDIAM16) == FALSE) {
    cat(c("GRAINDIAM16: DEPENDS, numeric, representative diameter", 
          "of grain size class 16 [m] ", "\n", 
          IN@GRAINDIAM16,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@REGPROPORTION16) == FALSE) {
    cat(c("REGPROPORTION16: DEPENDS, numeric, proportion of", 
          "sediments of grain size 16 in regolith", "\n", 
          IN@REGPROPORTION16,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@BRPROPORTION16) == FALSE) {
    cat(c("BRPROPORTION16: DEPENDS, numeric, proportion of", 
          "sediments of grain size 16 in bedrock", "\n", 
          IN@BRPROPORTION16,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@GRAINDIAM17) == FALSE) {
    cat(c("GRAINDIAM17: DEPENDS, numeric, representative diameter", 
          "of grain size class 17 [m] ", "\n", 
          IN@GRAINDIAM17,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@REGPROPORTION17) == FALSE) {
    cat(c("REGPROPORTION17: DEPENDS, numeric, proportion of", 
          "sediments of grain size 17 in regolith", "\n", 
          IN@REGPROPORTION17,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@BRPROPORTION17) == FALSE) {
    cat(c("BRPROPORTION17: DEPENDS, numeric, proportion of", 
          "sediments of grain size 17 in bedrock", "\n", 
          IN@BRPROPORTION17,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@GRAINDIAM18) == FALSE) {
    cat(c("GRAINDIAM18: DEPENDS, numeric, representative diameter", 
          "of grain size class 18 [m] ", "\n", 
          IN@GRAINDIAM18,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@REGPROPORTION18) == FALSE) {
    cat(c("REGPROPORTION18: DEPENDS, numeric, proportion of", 
          "sediments of grain size 18 in regolith", "\n", 
          IN@REGPROPORTION18,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@BRPROPORTION18) == FALSE) {
    cat(c("BRPROPORTION18: DEPENDS, numeric, proportion of", 
          "sediments of grain size 18 in bedrock", "\n", 
          IN@BRPROPORTION18,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@GRAINDIAM19) == FALSE) {
    cat(c("GRAINDIAM19: DEPENDS, numeric, representative diameter", 
          "of grain size class 19 [m] ", "\n", 
          IN@GRAINDIAM19,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@REGPROPORTION19) == FALSE) {
    cat(c("REGPROPORTION19: DEPENDS, numeric, proportion of", 
          "sediments of grain size 19 in regolith", "\n", 
          IN@REGPROPORTION19,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@BRPROPORTION19) == FALSE) {
    cat(c("BRPROPORTION19: DEPENDS, numeric, proportion of", 
          "sediments of grain size 19 in bedrock", "\n", 
          IN@BRPROPORTION19,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@GRAINDIAM20) == FALSE) {
    cat(c("GRAINDIAM20: DEPENDS, numeric, representative diameter", 
          "of grain size class 20 [m] ", "\n", 
          IN@GRAINDIAM20,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@REGPROPORTION20) == FALSE) {
    cat(c("REGPROPORTION20: DEPENDS, numeric, proportion of", 
          "sediments of grain size 20 in regolith", "\n", 
          IN@REGPROPORTION20,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@BRPROPORTION20) == FALSE) {
    cat(c("BRPROPORTION20: DEPENDS, numeric, proportion of", 
          "sediments of grain size 20 in bedrock", "\n", 
          IN@BRPROPORTION20,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@BETA) == FALSE) {
    cat(c("BETA: DEPENDS, numeric, fraction of sediment to", 
          "bedload", "\n", 
          IN@BETA,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@HIDINGEXP) == FALSE) {
    cat(c("HIDINGEXP: hiding exponent for multiple grain sizes", 
          "in bed cover", "\n", 
          IN@HIDINGEXP,"\n"), sep = "", file = IN_file)
  }

  cat(c("#\n",
        "# Stratigraphy parameters                                   \n",
        "#---------------------------------------------------------- \n",
        "#\n"), sep = "", file = IN_file) # write sub-section header
  if (is.na(IN@OPTSTRATGRID) == FALSE) {
    cat(c("OPTSTRATGRID: MANDATORY, logical (0/1), track stratigraphy", 
          "in underlying regular grid", "\n", 
          IN@OPTSTRATGRID,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@INPUTDATAFILE) == FALSE) {
    cat(c("INPUTDATAFILE: DEPENDS, character, name of the layer input", 
          "file .lay", "\n", 
          IN@INPUTDATAFILE,"\n"), sep = "", file = IN_file)
  }
  
  cat(c("#\n",
        "# Tectonics and base level parameters                       \n",
        "#---------------------------------------------------------- \n",
        "#\n"), sep = "", file = IN_file) # write sub-section header
  if (is.na(IN@UPTYPE) == FALSE) {
    cat(c("UPTYPE: MANDATORY, logical (0,...,14), type of uplift", "\n", 
          IN@UPTYPE,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@UPDUR) == FALSE) {
    cat(c("UPDUR: DEPENDS, numeric, uplift duration [a]", "\n", 
          IN@UPDUR,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@UPRATE) == FALSE) {
    cat(c("UPRATE: DEPENDS, numeric, uplift rate [m/a]", "\n", 
          IN@UPRATE,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@ACCEL_REL_UPTIME) == FALSE) {
    cat(c("ACCEL_REL_UPTIME: DEPENDS, numeric, time fraction of", 
          "accelerated fault motion", "\n", 
          IN@ACCEL_REL_UPTIME,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@ANTICLINEXCOORD) == FALSE) {
    cat(c("ANTICLINEXCOORD: DEPENDS, numeric, x-coordinate of", 
          "anticline crest [m]", "\n", 
          IN@ANTICLINEXCOORD,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@ANTICLINEYCOORD) == FALSE) {
    cat(c("ANTICLINEYCOORD: DEPENDS, numeric, y-coordinate of", 
          "anticline crest [m]", "\n", 
          IN@ANTICLINEYCOORD,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@BLDIVIDINGLINE) == FALSE) {
    cat(c("BLDIVIDINGLINE: DEPENDS, numeric, y-coordinate", 
          "separating baselevel fall zones [m]", "\n", 
          IN@BLDIVIDINGLINE,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@BLFALL_UPPER) == FALSE) {
    cat(c("BLFALL_UPPER: DEPENDS, numeric, baselevel fall rate at", 
          "upper boundary [m/a]", "\n", 
          IN@BLFALL_UPPER,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@DECAY_PARAM_UPLIFT) == FALSE) {
    cat(c("DECAY_PARAM_UPLIFT: DEPENDS, numeric, decay parameter", 
          "for uplift function", "\n", 
          IN@DECAY_PARAM_UPLIFT,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@FAULT_PIVOT_DISTANCE) == FALSE) {
    cat(c("FAULT_PIVOT_DISTANCE: DEPENDS, numeric, distance normal", 
          "fault pivot point [m]", "\n", 
          IN@FAULT_PIVOT_DISTANCE,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@FAULTPOS) == FALSE) {
    cat(c("FAULTPOS: DEPENDS, numeric, fault y-coordinate", 
          "perpendicular to x-axis [m]", "\n", 
          IN@FAULTPOS,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@FLATDEPTH) == FALSE) {
    cat(c("FLATDEPTH: DEPENDS, numeric, depth to flat portion of", 
          "fault plane [m]", "\n", 
          IN@FLATDEPTH,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@FOLDLATRATE) == FALSE) {
    cat(c("FOLDLATRATE: DEPENDS, numeric, lateral propagation rate", 
          "of fold [m]", "\n", 
          IN@FOLDLATRATE,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@FOLDPOSITION) == FALSE) {
    cat(c("FOLDPOSITION: DEPENDS, numeric, y-coordinate for fold", 
          "position [m]", "\n", 
          IN@FOLDPOSITION,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@FOLDPROPRATE) == FALSE) {
    cat(c("FOLDPROPRATE: DEPENDS, numeric, fold proparation rate", 
          "[m/a]", "\n", 
          IN@FOLDPROPRATE,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@FOLDUPRATE) == FALSE) {
    cat(c("FOLDUPRATE: DEPENDS, numeric, uplift rate of fold", 
          "axis [m/a]", "\n", 
          IN@FOLDUPRATE,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@FOLDWAVELEN) == FALSE) {
    cat(c("FOLDWAVELEN: DEPENDS, numeric, fold wavelength [m]", "\n", 
          IN@FOLDWAVELEN,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@FRONT_PROP_RATE) == FALSE) {
    cat(c("FRONT_PROP_RATE: DEPENDS, numeric, horizontal fron", 
          "propagation rate [m/a]", "\n", 
          IN@FRONT_PROP_RATE,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@KINKDIP) == FALSE) {
    cat(c("KINKDIP: DEPENDS, numeric, dip of fault kink [m]", "\n", 
          IN@KINKDIP,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@MINIMUM_UPRATE) == FALSE) {
    cat(c("MINIMUM_UPRATE, DEPENDS, numeric, minimum uplift rate", 
          "[m/a]", "\n", 
          IN@MINIMUM_UPRATE,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@NUMUPLIFTMAPS) == FALSE) {
    cat(c("NUMUPLIFTMAPS: DEPENDS, numeric, number of uplift rate", 
          "maps", "\n", 
          IN@NUMUPLIFTMAPS,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@OPT_INCREASE_TO_FRONT) == FALSE) {
    cat(c("OPT_INCREASE_TO_FRONT: DEPENDS, logical (0/1), uplift", 
          "rate increase toward y = 0", "\n", 
          IN@OPT_INCREASE_TO_FRONT,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@RAMPDIP) == FALSE) {
    cat(c("RAMPDIP: DEPENDS, numeric, dip of fault ramp [m]", "\n", 
          IN@RAMPDIP,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@SLIPRATE) == FALSE) {
    cat(c("SLIPRATE: DEPENDS, numeric, strike-slip or dip-slip", 
          "rate [m/a]", "\n", 
          IN@SLIPRATE,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@STARTING_Y_COORD) == FALSE) {
    cat(c("STARTING_Y_COORD: DEPENDS, numeric, y-coordinate of", 
          "propagating deformation front origin", "\n", 
          IN@STARTING_Y_COORD,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@SUBSRATE) == FALSE) {
    cat(c("SUBSRATE: DEPENDS, numeric, subsidence rate [m/a]", "\n", 
          IN@SUBSRATE,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@TIGHTENINGRATE) == FALSE) {
    cat(c("TIGHTENINGRATE: DEPENDS, numeric, fold tightening rate", 
          "[m/a]", "\n", 
          IN@TIGHTENINGRATE,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@UPLIFT_FRONT_GRADIENT) == FALSE) {
    cat(c("UPLIFT_FRONT_GRADIENT: DEPENDS, numeric, azimut of", 
          "uplift front", "\n", 
          IN@UPLIFT_FRONT_GRADIENT,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@UPMAPFILENAME) == FALSE) {
    cat(c("UPMAPFILENAME: DEPENDS, character, base name of uplift", 
          "rate files", "\n", 
          IN@UPMAPFILENAME,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@UPTIMEFILENAME) == FALSE) {
    cat(c("UPTIMEFILENAME: DEPENDS, character, name of file", 
          "containing uplift map times", "\n", 
          IN@UPTIMEFILENAME,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@UPPERKINKDIP) == FALSE) {
    cat(c("UPPERKINKDIP: DEPENDS, numeric, dip of upper fault", 
          "kink [m]", "\n", 
          IN@UPPERKINKDIP,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@UPSUBRATIO) == FALSE) {
    cat(c("UPSUBRATIO: DEPENDS, numeric, uplift-subsidence ratio", "\n", 
          IN@UPSUBRATIO,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@VERTICAL_THROW) == FALSE) {
    cat(c("VERTICAL_THROW, DEPENDS, numeric, total fault throw [m]", "\n", 
          IN@VERTICAL_THROW,"\n"), sep = "", file = IN_file)
  }
  if (is.na(IN@YFOLDINGSTART) == FALSE) {
    cat(c("YFOLDINGSTART, DEPENDS, numeric, starting time of fold", 
          "deformation [a]", "\n", 
          IN@YFOLDINGSTART), sep = "", file = IN_file)
  }
  
  ## close the written in-file
  close(IN_file)
}