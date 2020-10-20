

#' Class \code{"CHILD"}
#' 
#' A CHILD object comprises all data sets from a model run output. CHILD saves
#' a series of ASCII-files in a common directory. A CHILD object can be created
#' from a model run output file collection with the function
#' \code{\link{read.CHILD}}.
#' 
#' 
#' @name CHILD-class
#' @aliases CHILD-class CHILD show,CHILD-method
#' @docType class
#' @param \dots %% ~~Describe \code{\dots} here~~
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{CHILD(...)}.
#' @author Michael Dietze
#' @references CSDMS website. http://csdms.colorado.edu/wiki/Model:CHILD.\cr
#' Tucker, GE. 2010. CHILD Users Guide for version R9.4.1.
#' http://csdms.colorado.edu/mediawiki/images/Child_users_guide.pdf \cr Tucker,
#' GE., Lancaster, ST., Gasparini, NM., Bras, RL. 2001. The Channel-Hillslope
#' Integrated Landscape Development (CHILD) Model.  In Harmon, RS., Doe, W.W.
#' III (eds). Landscape Erosion and Evolution Modeling. Kluwer Academic/Plenum
#' Publishers, pp. 349-388.
#' @keywords classes
#' @examples
#' 
#' showClass("CHILD")
#' 
NULL





#' Example digital elevation model data set
#' 
#' This data set contains a digital elevation model (DEM) data set of the
#' eastern Erzgebirge region with 500 m spatial resolution.  The data set is a
#' raster object and was imported from the ESRI ascii format, with 139 columns
#' and 143 rows. The DEM is projected with UTM zone 33N. It was created from an
#' ASTER GDEM data set, resampled to a pixel size of 250 m.
#' 
#' 
#' @name DEM500
#' @docType data
#' @format The format is:
#' 
#' ncols 139\cr nrows 143\cr xllcorner 361217.04418762\cr yllcorner
#' 5605428.9851945\cr cellsize 500\cr NODATA_value -9999\cr -9999 -9999 -9999
#' -9999 -9999 -9999 -9999 -9999 -9999...\cr -9999 -9999 -9999 -9999 -9999
#' -9999 -9999 -9999 -9999...\cr -9999 -9999 -9999 -9999 -9999 -9999 -9999
#' -9999 -9999...\cr -9999 -9999 -9999 -9999 -9999 -9999 -9999 -9999
#' -9999...\cr
#' @source ASTER GDEM. http://asterweb.jpl.nasa.gov/gdem.asp
#' @keywords datasets
#' @examples
#' 
#' data(DEM500)
#' 
NULL





#' CHILD model run example data set hillslope1
#' 
#' This data set contains the CHILD example data set created by hillslope1.in
#' 
#' 
#' @name hillslope1b
#' @docType data
#' @format The format is:\cr Formal class 'CHILD' [package ".GlobalEnv"] with
#' 21 slots\cr ..@ summary : chr [1:5] "Creation date: # Created by CHILD on
#' Wed Nov 14 10:32:13 2012." "Run time: 2e+06" "Output interval: 1e+05"
#' "Number of time steps: 21" ...\cr ..@ inputs : chr [1:181] "# Created by
#' CHILD on Wed Nov 14 10:32:13 2012." "OUTFILENAME" "hillslope1" "RUNTIME" ...
#' ..@ timesteps: int [1:22] 0 1 2 3 4 5 6 7 8 9 ...\cr ..@ pts : logi [1, 1]
#' NA\cr ..@ upliftmap:List of 1\cr .. ..$ : logi NA\cr ..@ layers :List of
#' 1\cr .. ..$ : logi NA\cr ..@ nodes :List of 21\cr .. ..$ : num [1:1681, 1:4]
#' 2.54 2.56 2.61 2.67 2.7 ...\cr .. ..$ : num [1:1681, 1:4] 2.54 2.56 2.61
#' 2.67 2.7 ...\cr .. [list output truncated]\cr ..@ edges :List of 21\cr ..
#' ..$ : num [1:9912, 1:3] 0 23 0 26 0 ...\cr .. ..$ : num [1:9912, 1:3] 0 23 0
#' 26 0 ...\cr .. [list output truncated]\cr ..@ tri :List of 21\cr .. ..$ :
#' num [1:3276, 1:9] 0 0 0 0 0 0 1 1 1 1 ...\cr .. ..$ : num [1:3276, 1:9] 0 0
#' 0 0 0 0 1 1 1 1 ...\cr .. [list output truncated]\cr ..@ random : num
#' [1:1239] 0.00 5.70e+01 7.68e+07 9.95e+08 3.09e+08 ...\cr ..@ varea :List of
#' 21\cr .. ..$ : num [1:1681] 21 23.5 25.9 22.2 19.9 ...\cr .. ..$ : num
#' [1:1681] 21 23.5 25.9 22.2 19.9 ...\cr .. [list output truncated]\cr ..@ slp
#' :List of 21\cr .. ..$ : num [1:1681] 0 0 0 0 0 0 0 0 0 0 ...\cr .. ..$ : num
#' [1:1681] 0 0 0 0 0 0 0 0 0 0 ...\cr .. [list output truncated]\cr ..@ area
#' :List of 21\cr .. ..$ : num [1:1521] 1861 218 6424 37104 16103 ...\cr .. ..$
#' : num [1:1521] 21 23.5 25.9 22.2 19.9 ...\cr .. [list output truncated]\cr
#' ..@ vols : num [1:4000] 1904 3789 5659 7515 9360 ...\cr ..@ dvols : num
#' [1:3999] 1885 1870 1857 1845 1834 ...\cr ..@ q :List of 21\cr .. ..$ : num
#' [1:1681] 0 0 0 0 0 0 0 0 0 0 ...\cr .. ..$ : num [1:1681] 0 0 0 0 0 0 0 0 0
#' 0 ...\cr .. [list output truncated]\cr ..@ net :List of 21\cr .. ..$ : num
#' [1:1521] 26 36 29 1603 24 ...\cr .. ..$ : num [1:1521] 23 25 29 1603 24
#' ...\cr .. [list output truncated]\cr ..@ storm : logi [1, 1] NA\cr ..@ tau
#' :List of 21\cr .. ..$ : num [1:1681] 0 0 0 0 0 0 0 0 0 0 ...\cr .. ..$ : num
#' [1:1681] 0 0 0 0 0 0 0 0 0 0 ...\cr .. [list output truncated]\cr ..@ tx :
#' num [1:42] 0 1681 100000 1681 200000 ...\cr ..@ z :List of 21\cr .. ..$ :
#' num [1:1681] 0 0 0 0 0 0 0 0 0 0 ...\cr .. ..$ : num [1:1681] 9.73 6.16 8.63
#' 2.03 7.74 ...\cr .. [list output truncated]
#' @source CMDMS website. http://csdms.colorado.edu/wiki/Model:CHILD.
#' 
#' Tucker, GE. 2010. CHILD Users Guide for version R9.4.1.
#' http://csdms.colorado.edu/mediawiki/images/Child_users_guide.pdf
#' @keywords datasets
#' @examples
#' 
#' data(hillslope1b) # read data set
#' hillslope1b@summary # show summary information
#' 
NULL





#' Input file from CHILD example data set hillslope1
#' 
#' This data set represents the import of the CHILD example file hillslope1.in
#' 
#' 
#' @name hillslope1.in
#' @docType data
#' @format The format is: \cr Formal class 'IN' [package ".GlobalEnv"] with 159
#' slots \cr ..@ ACCEL_REL_UPTIME : num NA \cr ..@ ANTICLINEXCOORD : num NA \cr
#' ..@ ANTICLINEYCOORD : num NA \cr ..@ ARCGRIDFILENAME : num NA \cr ..@
#' BANK_ROUGH_COEFF : num 1 \cr .. [list output truncated]
#' @source CMDMS website. http://csdms.colorado.edu/wiki/Model:CHILD.
#' 
#' Tucker, GE. 2010. CHILD Users Guide for version R9.4.1.
#' http://csdms.colorado.edu/mediawiki/images/Child_users_guide.pdf
#' @keywords datasets
#' @examples
#' 
#' data(hillslope1.in) # read input file
#' hillslope1.in@OPINTRVL # show output interval value
#' 
NULL





#' CHILD model run example data set hillslope1
#' 
#' This data set contains the CHILD example data set created by hillslope1.in
#' 
#' 
#' @name hillslope1
#' @docType data
#' @format The format is:\cr Formal class 'CHILD' [package ".GlobalEnv"] with
#' 21 slots\cr ..@ summary : chr [1:5] "Creation date: # Created by CHILD on
#' Wed Nov 14 10:32:13 2012." "Run time: 2e+06" "Output interval: 1e+05"
#' "Number of time steps: 21" ...\cr ..@ inputs : chr [1:181] "# Created by
#' CHILD on Wed Nov 14 10:32:13 2012." "OUTFILENAME" "hillslope1" "RUNTIME" ...
#' ..@ timesteps: int [1:22] 0 1 2 3 4 5 6 7 8 9 ...\cr ..@ pts : logi [1, 1]
#' NA\cr ..@ upliftmap:List of 1\cr .. ..$ : logi NA\cr ..@ layers :List of
#' 1\cr .. ..$ : logi NA\cr ..@ nodes :List of 21\cr .. ..$ : num [1:1681, 1:4]
#' 2.54 2.56 2.61 2.67 2.7 ...\cr .. ..$ : num [1:1681, 1:4] 2.54 2.56 2.61
#' 2.67 2.7 ...\cr .. [list output truncated]\cr ..@ edges :List of 21\cr ..
#' ..$ : num [1:9912, 1:3] 0 23 0 26 0 ...\cr .. ..$ : num [1:9912, 1:3] 0 23 0
#' 26 0 ...\cr .. [list output truncated]\cr ..@ tri :List of 21\cr .. ..$ :
#' num [1:3276, 1:9] 0 0 0 0 0 0 1 1 1 1 ...\cr .. ..$ : num [1:3276, 1:9] 0 0
#' 0 0 0 0 1 1 1 1 ...\cr .. [list output truncated]\cr ..@ random : num
#' [1:1239] 0.00 5.70e+01 7.68e+07 9.95e+08 3.09e+08 ...\cr ..@ varea :List of
#' 21\cr .. ..$ : num [1:1681] 21 23.5 25.9 22.2 19.9 ...\cr .. ..$ : num
#' [1:1681] 21 23.5 25.9 22.2 19.9 ...\cr .. [list output truncated]\cr ..@ slp
#' :List of 21\cr .. ..$ : num [1:1681] 0 0 0 0 0 0 0 0 0 0 ...\cr .. ..$ : num
#' [1:1681] 0 0 0 0 0 0 0 0 0 0 ...\cr .. [list output truncated]\cr ..@ area
#' :List of 21\cr .. ..$ : num [1:1521] 1861 218 6424 37104 16103 ...\cr .. ..$
#' : num [1:1521] 21 23.5 25.9 22.2 19.9 ...\cr .. [list output truncated]\cr
#' ..@ vols : num [1:4000] 1904 3789 5659 7515 9360 ...\cr ..@ dvols : num
#' [1:3999] 1885 1870 1857 1845 1834 ...\cr ..@ q :List of 21\cr .. ..$ : num
#' [1:1681] 0 0 0 0 0 0 0 0 0 0 ...\cr .. ..$ : num [1:1681] 0 0 0 0 0 0 0 0 0
#' 0 ...\cr .. [list output truncated]\cr ..@ net :List of 21\cr .. ..$ : num
#' [1:1521] 26 36 29 1603 24 ...\cr .. ..$ : num [1:1521] 23 25 29 1603 24
#' ...\cr .. [list output truncated]\cr ..@ storm : logi [1, 1] NA\cr ..@ tau
#' :List of 21\cr .. ..$ : num [1:1681] 0 0 0 0 0 0 0 0 0 0 ...\cr .. ..$ : num
#' [1:1681] 0 0 0 0 0 0 0 0 0 0 ...\cr .. [list output truncated]\cr ..@ tx :
#' num [1:42] 0 1681 100000 1681 200000 ...\cr ..@ z :List of 21\cr .. ..$ :
#' num [1:1681] 0 0 0 0 0 0 0 0 0 0 ...\cr .. ..$ : num [1:1681] 9.73 6.16 8.63
#' 2.03 7.74 ...\cr .. [list output truncated]
#' @source CMDMS website. http://csdms.colorado.edu/wiki/Model:CHILD.
#' 
#' Tucker, GE. 2010. CHILD Users Guide for version R9.4.1.
#' http://csdms.colorado.edu/mediawiki/images/Child_users_guide.pdf
#' @keywords datasets
#' @examples
#' 
#' data(hillslope1) # read data set
#' hillslope1@summary # show summary information
#' 
NULL





#' Class \code{"IN"}
#' 
#' An IN-object contains slots for all possible input parameters of the CHILD
#' model (cf. Tucker, 2010). Each slot for which an input-file cannot deliver a
#' parameter is assigned NA. IN-objects can be created from a CHILD input file
#' with the function read.IN().
#' 
#' 
#' @name IN-class
#' @aliases IN-class IN
#' @docType class
#' @section Objects from the Class: The object IN represents all elements
#' contained in a CHILD input file.
#' @author Michael Dietze
#' @references CSDMS website. http://csdms.colorado.edu/wiki/Model:CHILD.\cr
#' Tucker, GE. 2010. CHILD Users Guide for version R9.4.1.
#' http://csdms.colorado.edu/mediawiki/images/Child_users_guide.pdf \cr Tucker,
#' GE., Lancaster, ST., Gasparini, NM., Bras, RL. 2001. The Channel-Hillslope
#' Integrated Landscape Development (CHILD) Model.  In Harmon, RS., Doe, W.W.
#' III (eds). Landscape Erosion and Evolution Modeling. Kluwer Academic/Plenum
#' Publishers, pp. 349-388.
#' @keywords classes
#' @examples
#' 
#' showClass("IN")
#' 
NULL





#' CHILD model run example data set rainfall1
#' 
#' This data set contains the CHILD example data set created by rainfall1.in
#' 
#' 
#' @name rainfall1
#' @docType data
#' @format The format is:\cr Formal class 'CHILD' [package ".GlobalEnv"] with
#' 21 slots\cr ..@ summary : chr [1:5] "Creation date: # Created by CHILD on
#' Sun Nov 18 13:47:29 2012." "Run time: 1" "Output interval: 1" "Number of
#' time steps: 2" ...\cr ..@ inputs : chr [1:185] "# Created by CHILD on Sun
#' Nov 18 13:47:29 2012." "OUTFILENAME" "rainfall1" "RUNTIME" ...\cr ..@
#' timesteps: int [1:3] 0 1 2\cr ..@ pts : logi [1, 1] NA\cr ..@ upliftmap:List
#' of 1\cr .. ..$ : logi NA\cr ..@ layers :List of 1\cr .. ..$ : logi NA\cr ..@
#' nodes :List of 2\cr .. ..$ : num [1:121, 1:4] 64.2 74.5 91.6 97.9 99.7
#' ...\cr .. ..$ : num [1:121, 1:4] 64.2 74.5 91.6 97.9 99.7 ...\cr ..@ edges
#' :List of 2\cr .. ..$ : num [1:694, 1:3] 0 6 0 11 0 89 0 90 0 91 ...\cr ..
#' ..$ : num [1:694, 1:3] 0 6 0 11 0 89 0 90 0 91 ...\cr ..@ tri :List of 2\cr
#' .. ..$ : num [1:227, 1:9] 0 0 0 0 0 0 1 1 1 1 ...\cr .. ..$ : num [1:227,
#' 1:9] 0 0 0 0 0 0 1 1 1 1 ...\cr ..@ random : num [1:118] 0.00 5.70e+01
#' 7.68e+07 9.95e+08 3.09e+08 ...\cr ..@ varea :List of 2\cr .. ..$ : num
#' [1:121] 8343 8324 8517 11342 7812 ...\cr .. ..$ : num [1:121] 8343 8324 8517
#' 11342 7812 ...\cr ..@ slp :List of 2\cr .. ..$ : num [1:121] 0.00674 0.00862
#' 0.0024 0.00715 0.01366 ...\cr .. ..$ : num [1:121] 0.00674 0.00862 0.0024
#' 0.00715 0.01366 ...\cr ..@ area :List of 2\cr .. ..$ : num [1:81] 8343 28338
#' 88837 60636 7812 ...\cr .. ..$ : num [1:81] 8343 28338 88837 60636 7812
#' ...\cr ..@ vols : num [1:95] 4038694 4038694 4038694 4038694 4038694 ...\cr
#' ..@ dvols : num [1:94] -0.02125 -0.00426 -0.09042 -0.1193 -0.12075 ...\cr
#' ..@ q :List of 2\cr .. ..$ : num [1:121] 136833 464747 1456933 994428 128112
#' ...\cr .. ..$ : num [1:121] 20939 71117 222945 152171 19604 ...\cr ..@ net
#' :List of 2\cr .. ..$ : num [1:81] 6 8 5 7 112 12 1 2 3 8 ...\cr .. ..$ : num
#' [1:81] 6 8 5 7 112 12 1 2 3 8 ...\cr ..@ storm : num [1:95, 1:3] 0.00302
#' 0.00564 0.01221 0.00301 0.01526 ...\cr ..@ tau :List of 2\cr .. ..$ : num
#' [1:121] 0 0 0 0 0 0 0 0 0 0 ...\cr .. ..$ : num [1:121] 1.011 1.733 0.999
#' 1.91 1.625 ...\cr ..@ tx : num [1:4] 0 121 1.01 121\cr ..@ z :List of 2\cr
#' .. ..$ : num [1:121] 9.19 6.62 2.42 5.11 1.65 ...\cr .. ..$ : num [1:121]
#' 9.19 6.62 2.42 5.11 1.65 ...\cr
#' @source CMDMS website. http://csdms.colorado.edu/wiki/Model:CHILD.
#' 
#' Tucker, GE. 2010. CHILD Users Guide for version R9.4.1.
#' http://csdms.colorado.edu/mediawiki/images/Child_users_guide.pdf
#' @keywords datasets
#' @examples
#' 
#' data(rainfall1) # read data set
#' rainfall1@summary # show summary information
#' 
NULL





#' Functions for flexible use of the landscape evolution model CHILD
#' 
#' The CHILD (Channel-Hillslope Integrated Landscape Development) model is a
#' numerical landscape evolution model (LEM). Basic information about CHILD and
#' the capabilites are explained in Tucker et al. (2001). The model is operated
#' as a compiled program from the command line, in combination with an input
#' file (*.in). This input file comprises all modell parameters. CHILD produces
#' no graphical output but delivers a series of ASCII-files with numerical
#' output (for details see Tucker, 2010).\cr In order to work with this model
#' more flexible, to dynamically change input parameters and run the modified
#' model, to create visual output such as variable plots, surface maps or
#' surface animations, and to establish a link between CHILD and other software
#' for spatial data analysis, this package provides general functions. Many of
#' the functions are based on the Matlab-scripts available at the Community
#' Surface Dynamics Modeling System (CSDMS, 2012) and greatly benefited from
#' frequent support by founders and users of CHILD.\cr\cr References\cr CSDMS.
#' 2012. Community Surface Dynamics Modeling System.\cr Tucker, GE. 2010. CHILD
#' Users Guide for version R9.4.1.\cr Tucker, GE., Lancaster, ST., Gasparini,
#' NM., Bras, RL. 2001. The Channel-Hillslope Integrated Landscape Development
#' (CHILD) Model. In Harmon, RS., Doe, W.W. III (eds). Landscape Erosion and
#' Evolution Modeling. Kluwer Academic/Plenum Publishers, pp. 349-388.
#' 
#' \tabular{ll}{Package: \tab RCHILD\cr Type: \tab Package\cr Title: \tab
#' Functions for flexible use of the landscape evolution model CHILD\cr
#' Version: \tab 0.2.3\cr Date: \tab 2013-01-31\cr Author: \tab Michael
#' Dietze\cr Maintainer: \tab Michael Dietze
#' <micha.dietze@mailbox.tu-dresden.de>\cr License: \tab GPL-3\cr Depends: \tab
#' R (>= 2.15.1), akima, fields, raster, rgdal, rgl, animation\cr Imports: \tab
#' methods\cr}
#' 
#' @name RCHILD-package
#' @aliases RCHILD-package RCHILD
#' @docType package
#' @author Michael Dietze
#' @keywords package
NULL



