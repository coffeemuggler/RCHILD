\name{hillslope1.in}
\alias{hillslope1.in}
\docType{data}
\title{Input file from CHILD example data set hillslope1}
\description{This data set represents the import of the CHILD example file hillslope1.in}
\usage{data(hillslope1.in)}
\format{
  The format is: \cr
Formal class 'IN' [package ".GlobalEnv"] with 159 slots \cr
  ..@ ACCEL_REL_UPTIME       : num NA \cr
  ..@ ANTICLINEXCOORD        : num NA \cr
  ..@ ANTICLINEYCOORD        : num NA \cr
  ..@ ARCGRIDFILENAME        : num NA \cr
  ..@ BANK_ROUGH_COEFF       : num 1  \cr
  .. [list output truncated]
}
\source{CMDMS website. http://csdms.colorado.edu/wiki/Model:CHILD.

Tucker, GE. 2010. CHILD Users Guide for version R9.4.1. http://csdms.colorado.edu/mediawiki/images/Child_users_guide.pdf}
\examples{
data(hillslope1.in) # read input file
hillslope1.in@OPINTRVL # show output interval value
}
\keyword{datasets}
