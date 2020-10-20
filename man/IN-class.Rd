\name{IN-class}
\Rdversion{1.1}
\docType{class}
\alias{IN-class}
\alias{IN}

\title{Class \code{"IN"}}
\description{An IN-object contains slots for all possible input parameters
  of the CHILD model (cf. Tucker, 2010). Each slot for which an input-file 
  cannot deliver a parameter is assigned NA. IN-objects can be 
  created from a CHILD input file with the function read.IN().}
\section{Objects from the Class}{
The object IN represents all elements contained in a CHILD input file.
}
\section{Slots}{
  \describe{
    \item{\code{ACCEL_REL_UPTIME}:}{Object of class \code{"numeric"} Uplift 
      option 9: fraction of total time that fault motion has been 
      accelerated. }
    \item{\code{ANTICLINEXCOORD}:}{Object of class \code{"numeric"} Uplift 
      option 5: x coordinate [m] of anticline crest. }
    \item{\code{ANTICLINEYCOORD}:}{Object of class \code{"numeric"} Uplift 
      option 5: y coordinate [m] of anticline crest. }
    \item{\code{BANK_ROUGH_COEFF}:}{Object of class \code{"numeric"} Coefficient 
      in bank roughness-discharge relation. }
    \item{\code{BANK_ROUGH_EXP}:}{Object of class \code{"numeric"} Exponent 
      in bank roughness-discharge relation. }
    \item{\code{BANK_ERO}:}{Object of class \code{"numeric"} Stream meander 
      module: stream-bank erodibility coefficient. }
    \item{\code{BANKFULLEVENT}:}{Object of class \code{"numeric"} Runoff rate
      associated with bankfull flood event. Used to  compute hydraulic geometry.}
    \item{\code{BEDROCKDEPTH}:}{Object of class \code{"numeric"} Starting 
      thickness [m] of bedrock layer. }
    \item{\code{BETA}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{BLDIVIDINGLINE}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{BLFALL_UPPER}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{BNKHTDEP}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{BRPROPORTION1}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 1 in bedrock.}
    \item{\code{BRPROPORTION2}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 2 in bedrock.}
    \item{\code{BRPROPORTION3}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 3 in bedrock.}
    \item{\code{BRPROPORTION4}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 4 in bedrock.}
    \item{\code{BRPROPORTION5}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 5 in bedrock.}
    \item{\code{BRPROPORTION6}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 6 in bedrock.}
    \item{\code{BRPROPORTION7}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 7 in bedrock.}
    \item{\code{BRPROPORTION8}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 8 in bedrock.}
    \item{\code{BRPROPORTION9}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 9 in bedrock.}
    \item{\code{BRPROPORTION10}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 10 in bedrock.}
    \item{\code{BRPROPORTION11}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 11 in bedrock.}
    \item{\code{BRPROPORTION12}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 12 in bedrock.}
    \item{\code{BRPROPORTION13}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 13 in bedrock.}
    \item{\code{BRPROPORTION14}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 14 in bedrock.}
    \item{\code{BRPROPORTION15}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 15 in bedrock.}
    \item{\code{BRPROPORTION16}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 16 in bedrock.}
    \item{\code{BRPROPORTION17}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 17 in bedrock.}
    \item{\code{BRPROPORTION18}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 18 in bedrock.}
    \item{\code{BRPROPORTION19}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 19 in bedrock.}
    \item{\code{BRPROPORTION20}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 20 in bedrock.}
    \item{\code{CHAN_GEOM_MODEL}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{CRITICAL_AREA}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{CRITICAL_SLOPE}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{DECAY_PARAM_UPLIFT}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{DEF_CHAN_DISCR}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{DETACHMENT_LAW}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{DIFFUSIONTHRESHOLD}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{FAULT_PIVOT_DISTANCE}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{FAULTPOS}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{FLATDEPTH}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{FLOWGEN}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{FLOWVELOCITY}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{FOLDLATRATE}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{FOLDPOSITION}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{FOLDPROPRATE}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{FOLDUPRATE}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{FOLDWAVELEN}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{FP_BANKFULLEVENT}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{FP_DRAREAMIN}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{FP_INLET_ELEVATION}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{FP_LAMBDA}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{FP_MU}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{FP_OPTCONTROLCHAN}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{FP_VALDROP}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{FRAC_WID_ADD}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{FRAC_WID_MOVE}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{FRONT_PROP_RATE}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{GRAINDIAM1}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 1 in bedrock.}
    \item{\code{GRAINDIAM2}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 2 in bedrock.}
    \item{\code{GRAINDIAM3}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 3 in bedrock.}
    \item{\code{GRAINDIAM4}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 4 in bedrock.}
    \item{\code{GRAINDIAM5}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 5 in bedrock.}
    \item{\code{GRAINDIAM6}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 6 in bedrock.}
    \item{\code{GRAINDIAM7}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 7 in bedrock.}
    \item{\code{GRAINDIAM8}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 8 in bedrock.}
    \item{\code{GRAINDIAM9}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 9 in bedrock.}
    \item{\code{GRAINDIAM10}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 10 in bedrock.}
    \item{\code{GRAINDIAM11}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 11 in bedrock.}
    \item{\code{GRAINDIAM12}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 12 in bedrock.}
    \item{\code{GRAINDIAM13}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 13 in bedrock.}
    \item{\code{GRAINDIAM14}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 14 in bedrock.}
    \item{\code{GRAINDIAM15}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 15 in bedrock.}
    \item{\code{GRAINDIAM16}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 16 in bedrock.}
    \item{\code{GRAINDIAM17}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 17 in bedrock.}
    \item{\code{GRAINDIAM18}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 18 in bedrock.}
    \item{\code{GRAINDIAM19}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 19 in bedrock.}
    \item{\code{GRAINDIAM20}:}{Object of class \code{"numeric"} Proportion 
      of sediments of grain size 20 in bedrock.}
    \item{\code{GRID_LENGTH}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{GRID_WIDTH}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{GRID_SPACING}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{GRIDDX}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{HIDINGEXP}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{HYDR_DEP_COEFF_DS}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{HYDR_DEP_EXP_DS}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{HYDR_DEP_EXP_STN}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{HYDR_ROUGH_COEFF_DS}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{HYDR_ROUGH_EXP_DS}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{HYDR_ROUGH_EXP_STN}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{HYDR_WID_COEFF_DS}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{HYDR_WID_EXP_DS}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{HYDR_WID_EXP_STN}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{HYDROSHAPEFAC}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{INDRAREA}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{INFILTRATION}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{INLET_OPTCALCSEDFEED}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{INLET_SLOPE}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{INLET_X}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{INLET_Y}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{INPUTDATAFILE}:}{Object of class \code{"character"} ~~ }
    \item{\code{INPUTTIME}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{INSEDLOAD1}:}{Object of class \code{"numeric"} Proportion 
      sediment load of grain size 1}
    \item{\code{INSEDLOAD2}:}{Object of class \code{"numeric"} Proportion 
      sediment load of grain size 2}
    \item{\code{INSEDLOAD3}:}{Object of class \code{"numeric"} Proportion 
      sediment load of grain size 3}
    \item{\code{INSEDLOAD4}:}{Object of class \code{"numeric"} Proportion 
      sediment load of grain size 4}
    \item{\code{INSEDLOAD5}:}{Object of class \code{"numeric"} Proportion 
      sediment load of grain size 5}
    \item{\code{INSEDLOAD6}:}{Object of class \code{"numeric"} Proportion 
      sediment load of grain size 6}
    \item{\code{INSEDLOAD7}:}{Object of class \code{"numeric"} Proportion 
      sediment load of grain size 7}
    \item{\code{INSEDLOAD8}:}{Object of class \code{"numeric"} Proportion 
      sediment load of grain size 8}
    \item{\code{INSEDLOAD9}:}{Object of class \code{"numeric"} Proportion 
      sediment load of grain size 9}
    \item{\code{INSEDLOAD10}:}{Object of class \code{"numeric"} Proportion 
      sediment load of grain size 10}
    \item{\code{INSEDLOAD11}:}{Object of class \code{"numeric"} Proportion 
      sediment load of grain size 11}
    \item{\code{INSEDLOAD12}:}{Object of class \code{"numeric"} Proportion 
      sediment load of grain size 12}
    \item{\code{INSEDLOAD13}:}{Object of class \code{"numeric"} Proportion 
      sediment load of grain size 13}
    \item{\code{INSEDLOAD14}:}{Object of class \code{"numeric"} Proportion 
      sediment load of grain size 14}
    \item{\code{INSEDLOAD15}:}{Object of class \code{"numeric"} Proportion 
      sediment load of grain size 15}
    \item{\code{INSEDLOAD16}:}{Object of class \code{"numeric"} Proportion 
      sediment load of grain size 16}
    \item{\code{INSEDLOAD17}:}{Object of class \code{"numeric"} Proportion 
      sediment load of grain size 17}
    \item{\code{INSEDLOAD18}:}{Object of class \code{"numeric"} Proportion 
      sediment load of grain size 18}
    \item{\code{INSEDLOAD19}:}{Object of class \code{"numeric"} Proportion 
      sediment load of grain size 19}
    \item{\code{INSEDLOAD20}:}{Object of class \code{"numeric"} Proportion 
      sediment load of grain size 20}
    \item{\code{KB}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{KD}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{KF}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{KINKDIP}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{KINWAVE_HQEXP}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{KR}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{KT}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{LAKEFILL}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{LOESS_DEP_RATE}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{MAXICMEAN}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{MAXREGDEPTH}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{MB}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{MEAN_ELEV}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{MEDIAN_DIAMETER}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{MESHADAPT_MAXNODEFLUX}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{MESHADAPTAREA_MINAREA}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{MESHADAPTAREA_MAXVAREA}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{MF}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{MINIMUM_UPRATE}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{NB}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{NF}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{NUM_PTS}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{NUMGRNSIZE}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{NUMUPLIFTMAPS}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{OPT_INCREASE_TO_FRONT}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{OPT_NONLINEAR_DIFFUSION}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{OPT_PT_PLACE}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{OPT_VAR_SIZE}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{OPINTRVL}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{OPTDETACHLIM}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{OPTDIFFDEP}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{OPTEXPOSURETIME}:}{Object of class \code{"numeric"} Track 
      surface-layer exposure ages.}
    \item{\code{OPTFLOODPLAIN}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{OPTFOLDDENS}:}{Object of class \code{"numeric"} Mesh 
      densification around a growing fold.}
    \item{\code{OPTINITMESHDENS}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{OPTINLET}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{OPTINTERPLAYER}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{OPTKINWAVE}:}{Object of class \code{"numeric"} Kinematic-wave 
      flow routing (steady, 2D).}
    \item{\code{OPTLAYEROUTPUT}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{OPTLOESSDEP}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{OPTMEANDER}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{OPTMESHADAPTAREA}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{OPTMESHADAPTDZ}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{OPTMNDR}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{OPTREADINPUT}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{OPTREADLAYER}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{OPTSINVARINFILT}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{OPTSTRATGRID}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{OPTTSOUTPUT}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{OPTVAR}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{OPTVEG}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{OUTFILENAME}:}{Object of class \code{"character"} ~~ }
    \item{\code{OUTLET_X_COORD}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{OUTLET_Y_COORD}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{PB}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{PERIOD_INFILT}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{PF}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{POINTFILENAME}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{RAMPDIP}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{RAND_ELEV}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{REGINIT}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{REGPROPORTION1}:}{Object of class \code{"numeric"} Inlet 
      of sediments of grain size 1 in regolith 1}
    \item{\code{REGPROPORTION2}:}{Object of class \code{"numeric"} Inlet 
      of sediments of grain size 1 in regolith 2}
    \item{\code{REGPROPORTION3}:}{Object of class \code{"numeric"} Inlet 
      of sediments of grain size 1 in regolith 3}
    \item{\code{REGPROPORTION4}:}{Object of class \code{"numeric"} Inlet 
      of sediments of grain size 1 in regolith 4}
    \item{\code{REGPROPORTION5}:}{Object of class \code{"numeric"} Inlet 
      of sediments of grain size 1 in regolith 5}
    \item{\code{REGPROPORTION6}:}{Object of class \code{"numeric"} Inlet 
      of sediments of grain size 1 in regolith 6}
    \item{\code{REGPROPORTION7}:}{Object of class \code{"numeric"} Inlet 
      of sediments of grain size 1 in regolith 7}
    \item{\code{REGPROPORTION8}:}{Object of class \code{"numeric"} Inlet 
      of sediments of grain size 1 in regolith 8}
    \item{\code{REGPROPORTION9}:}{Object of class \code{"numeric"} Inlet 
      of sediments of grain size 1 in regolith 9}
    \item{\code{REGPROPORTION10}:}{Object of class \code{"numeric"} Inlet 
      of sediments of grain size 1 in regolith 10}
    \item{\code{REGPROPORTION11}:}{Object of class \code{"numeric"} Inlet 
      of sediments of grain size 1 in regolith 11}
    \item{\code{REGPROPORTION12}:}{Object of class \code{"numeric"} Inlet 
      of sediments of grain size 1 in regolith 12}
    \item{\code{REGPROPORTION13}:}{Object of class \code{"numeric"} Inlet 
      of sediments of grain size 1 in regolith 13}
    \item{\code{REGPROPORTION14}:}{Object of class \code{"numeric"} Inlet 
      of sediments of grain size 1 in regolith 14}
    \item{\code{REGPROPORTION15}:}{Object of class \code{"numeric"} Inlet 
      of sediments of grain size 1 in regolith 15}
    \item{\code{REGPROPORTION16}:}{Object of class \code{"numeric"} Inlet 
      of sediments of grain size 1 in regolith 16}
    \item{\code{REGPROPORTION17}:}{Object of class \code{"numeric"} Inlet 
      of sediments of grain size 1 in regolith 17}
    \item{\code{REGPROPORTION18}:}{Object of class \code{"numeric"} Inlet 
      of sediments of grain size 1 in regolith 18}
    \item{\code{REGPROPORTION19}:}{Object of class \code{"numeric"} Inlet 
      of sediments of grain size 1 in regolith 19}
    \item{\code{REGPROPORTION20}:}{Object of class \code{"numeric"} Inlet 
      of sediments of grain size 1 in regolith 20}
    \item{\code{RUNTIME}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{SEED}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{SG_MAXREGDEPTH}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{SHEAR_RATIO}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{SLIPRATE}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{SLOPED_SURF}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{SOILSTORE}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{ST_ISTDUR}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{ST_PMEAN}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{ST_STDUR}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{STARTING_Y_COORD}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{SUBSRATE}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{SURFER}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{TAUCB}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{TAUCR}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{THETAC}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{TIGHTENINGRATE}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{TRANSMISSIVITY}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{TRANSPORT_LAW}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{TSOPINTRVL}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{TYPE_BOUND}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{UPDUR}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{UPLIFT_FRONT_GRADIENT}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{UPMAPFILENAME}:}{Object of class \code{"character"} ~~ }
    \item{\code{UPPER_BOUND_Z}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{UPPERKINKDIP}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{UPRATE}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{UPSUBRATIO}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{UPTIMEFILENAME}:}{Object of class \code{"character"} ~~ }
    \item{\code{UPTYPE}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{VERTICAL_THROW}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{X_GRID_SIZE}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{XCORNER}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{Y_GRID_SIZE}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{YCORNER}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{YFOLDINGSTART}:}{Object of class \code{"numeric"} ~~ }
  }
}
\section{Methods}{
No methods defined with class "IN" in the signature.
}
\references{
  CSDMS website. http://csdms.colorado.edu/wiki/Model:CHILD.\cr
  Tucker, GE. 2010. CHILD Users Guide for version R9.4.1. 
  http://csdms.colorado.edu/mediawiki/images/Child_users_guide.pdf \cr
  Tucker, GE., Lancaster, ST., Gasparini, NM., Bras, RL. 2001. The 
  Channel-Hillslope Integrated Landscape Development (CHILD) Model. 
  In Harmon, RS.,  Doe, W.W. III (eds). Landscape Erosion and Evolution
  Modeling. Kluwer Academic/Plenum Publishers, pp. 349-388.
}
\author{Michael Dietze}
\examples{
showClass("IN")
}
\keyword{classes}
