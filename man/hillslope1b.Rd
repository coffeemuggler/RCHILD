\name{hillslope1b}
\alias{hillslope1b}
\docType{data}
\title{CHILD model run example data set hillslope1}
\description{This data set contains the CHILD example data set created by hillslope1.in}
\usage{data(hillslope1b)}
\format{
  The format is:\cr
Formal class 'CHILD' [package ".GlobalEnv"] with 21 slots\cr
  ..@ summary  : chr [1:5] "Creation date:         # Created by CHILD on Wed Nov 14 10:32:13 2012." "Run time:              2e+06" "Output interval:       1e+05" "Number of time steps:  21" ...\cr
  ..@ inputs   : chr [1:181] "# Created by CHILD on Wed Nov 14 10:32:13 2012." "OUTFILENAME" "hillslope1" "RUNTIME" ...
  ..@ timesteps: int [1:22] 0 1 2 3 4 5 6 7 8 9 ...\cr
  ..@ pts      : logi [1, 1] NA\cr
  ..@ upliftmap:List of 1\cr
  .. ..$ : logi NA\cr
  ..@ layers   :List of 1\cr
  .. ..$ : logi NA\cr
  ..@ nodes    :List of 21\cr
  .. ..$ : num [1:1681, 1:4] 2.54 2.56 2.61 2.67 2.7 ...\cr
  .. ..$ : num [1:1681, 1:4] 2.54 2.56 2.61 2.67 2.7 ...\cr
  .. [list output truncated]\cr
  ..@ edges    :List of 21\cr
  .. ..$ : num [1:9912, 1:3] 0 23 0 26 0 ...\cr
  .. ..$ : num [1:9912, 1:3] 0 23 0 26 0 ...\cr
  .. [list output truncated]\cr
  ..@ tri      :List of 21\cr
  .. ..$ : num [1:3276, 1:9] 0 0 0 0 0 0 1 1 1 1 ...\cr
  .. ..$ : num [1:3276, 1:9] 0 0 0 0 0 0 1 1 1 1 ...\cr
  .. [list output truncated]\cr
  ..@ random   : num [1:1239] 0.00 5.70e+01 7.68e+07 9.95e+08 3.09e+08 ...\cr
  ..@ varea    :List of 21\cr
  .. ..$ : num [1:1681] 21 23.5 25.9 22.2 19.9 ...\cr
  .. ..$ : num [1:1681] 21 23.5 25.9 22.2 19.9 ...\cr
  .. [list output truncated]\cr
  ..@ slp      :List of 21\cr
  .. ..$ : num [1:1681] 0 0 0 0 0 0 0 0 0 0 ...\cr
  .. ..$ : num [1:1681] 0 0 0 0 0 0 0 0 0 0 ...\cr
  .. [list output truncated]\cr
  ..@ area     :List of 21\cr
  .. ..$ : num [1:1521] 1861 218 6424 37104 16103 ...\cr
  .. ..$ : num [1:1521] 21 23.5 25.9 22.2 19.9 ...\cr
  .. [list output truncated]\cr
  ..@ vols     : num [1:4000] 1904 3789 5659 7515 9360 ...\cr
  ..@ dvols    : num [1:3999] 1885 1870 1857 1845 1834 ...\cr
  ..@ q        :List of 21\cr
  .. ..$ : num [1:1681] 0 0 0 0 0 0 0 0 0 0 ...\cr
  .. ..$ : num [1:1681] 0 0 0 0 0 0 0 0 0 0 ...\cr
  .. [list output truncated]\cr
  ..@ net      :List of 21\cr
  .. ..$ : num [1:1521] 26 36 29 1603 24 ...\cr
  .. ..$ : num [1:1521] 23 25 29 1603 24 ...\cr
  .. [list output truncated]\cr
  ..@ storm    : logi [1, 1] NA\cr
  ..@ tau      :List of 21\cr
  .. ..$ : num [1:1681] 0 0 0 0 0 0 0 0 0 0 ...\cr
  .. ..$ : num [1:1681] 0 0 0 0 0 0 0 0 0 0 ...\cr
  .. [list output truncated]\cr
  ..@ tx       : num [1:42] 0 1681 100000 1681 200000 ...\cr
  ..@ z        :List of 21\cr
  .. ..$ : num [1:1681] 0 0 0 0 0 0 0 0 0 0 ...\cr
  .. ..$ : num [1:1681] 9.73 6.16 8.63 2.03 7.74 ...\cr
  .. [list output truncated]
}
\source{CMDMS website. http://csdms.colorado.edu/wiki/Model:CHILD.

Tucker, GE. 2010. CHILD Users Guide for version R9.4.1. http://csdms.colorado.edu/mediawiki/images/Child_users_guide.pdf
}
\examples{
data(hillslope1b) # read data set
hillslope1b@summary # show summary information
}
\keyword{datasets}
