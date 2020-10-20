\name{rainfall1}
\alias{rainfall1}
\docType{data}
\title{CHILD model run example data set rainfall1}
\description{This data set contains the CHILD example data set created by rainfall1.in}
\usage{data(rainfall1)}
\format{
  The format is:\cr
Formal class 'CHILD' [package ".GlobalEnv"] with 21 slots\cr
  ..@ summary  : chr [1:5] "Creation date:         # Created by CHILD on Sun Nov 18 13:47:29 2012." "Run time:              1" "Output interval:       1" "Number of time steps:  2" ...\cr
  ..@ inputs   : chr [1:185] "# Created by CHILD on Sun Nov 18 13:47:29 2012." "OUTFILENAME" "rainfall1" "RUNTIME" ...\cr
  ..@ timesteps: int [1:3] 0 1 2\cr
  ..@ pts      : logi [1, 1] NA\cr
  ..@ upliftmap:List of 1\cr
  .. ..$ : logi NA\cr
  ..@ layers   :List of 1\cr
  .. ..$ : logi NA\cr
  ..@ nodes    :List of 2\cr
  .. ..$ : num [1:121, 1:4] 64.2 74.5 91.6 97.9 99.7 ...\cr
  .. ..$ : num [1:121, 1:4] 64.2 74.5 91.6 97.9 99.7 ...\cr
  ..@ edges    :List of 2\cr
  .. ..$ : num [1:694, 1:3] 0 6 0 11 0 89 0 90 0 91 ...\cr
  .. ..$ : num [1:694, 1:3] 0 6 0 11 0 89 0 90 0 91 ...\cr
  ..@ tri      :List of 2\cr
  .. ..$ : num [1:227, 1:9] 0 0 0 0 0 0 1 1 1 1 ...\cr
  .. ..$ : num [1:227, 1:9] 0 0 0 0 0 0 1 1 1 1 ...\cr
  ..@ random   : num [1:118] 0.00 5.70e+01 7.68e+07 9.95e+08 3.09e+08 ...\cr
  ..@ varea    :List of 2\cr
  .. ..$ : num [1:121] 8343 8324 8517 11342 7812 ...\cr
  .. ..$ : num [1:121] 8343 8324 8517 11342 7812 ...\cr
  ..@ slp      :List of 2\cr
  .. ..$ : num [1:121] 0.00674 0.00862 0.0024 0.00715 0.01366 ...\cr
  .. ..$ : num [1:121] 0.00674 0.00862 0.0024 0.00715 0.01366 ...\cr
  ..@ area     :List of 2\cr
  .. ..$ : num [1:81] 8343 28338 88837 60636 7812 ...\cr
  .. ..$ : num [1:81] 8343 28338 88837 60636 7812 ...\cr
  ..@ vols     : num [1:95] 4038694 4038694 4038694 4038694 4038694 ...\cr
  ..@ dvols    : num [1:94] -0.02125 -0.00426 -0.09042 -0.1193 -0.12075 ...\cr
  ..@ q        :List of 2\cr
  .. ..$ : num [1:121] 136833 464747 1456933 994428 128112 ...\cr
  .. ..$ : num [1:121] 20939 71117 222945 152171 19604 ...\cr
  ..@ net      :List of 2\cr
  .. ..$ : num [1:81] 6 8 5 7 112 12 1 2 3 8 ...\cr
  .. ..$ : num [1:81] 6 8 5 7 112 12 1 2 3 8 ...\cr
  ..@ storm    : num [1:95, 1:3] 0.00302 0.00564 0.01221 0.00301 0.01526 ...\cr
  ..@ tau      :List of 2\cr
  .. ..$ : num [1:121] 0 0 0 0 0 0 0 0 0 0 ...\cr
  .. ..$ : num [1:121] 1.011 1.733 0.999 1.91 1.625 ...\cr
  ..@ tx       : num [1:4] 0 121 1.01 121\cr
  ..@ z        :List of 2\cr
  .. ..$ : num [1:121] 9.19 6.62 2.42 5.11 1.65 ...\cr
  .. ..$ : num [1:121] 9.19 6.62 2.42 5.11 1.65 ...\cr
}
\source{CMDMS website. http://csdms.colorado.edu/wiki/Model:CHILD.

Tucker, GE. 2010. CHILD Users Guide for version R9.4.1. http://csdms.colorado.edu/mediawiki/images/Child_users_guide.pdf
}
\examples{
data(rainfall1) # read data set
rainfall1@summary # show summary information
}
\keyword{datasets}
