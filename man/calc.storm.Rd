\name{calc.storm}
\alias{calc.storm}
\title{Function to create a storm plot.}
\description{This function creates a time series of storm-derived rainfall intensity
[mm/hour] and returns a list object with the vectors time and intensity.
The parameter interval allows to create output for a subset of the entire
time series. Plot output is enabled by default.}
\usage{calc.storm(dataset, interval, plot = TRUE)}
\arguments{
  \item{dataset}{(S4-object) CHILD model run output data set.}
  \item{interval}{(numerical scalar) Optional time interval to evaluate. If specified, the 
vector must contain two elements: start and end time, values in hours.}
  \item{plot}{Optional plot output, default is TRUE.}
}

\value{A list object
\item{time}{Numeric vector of time [hours]}
\item{intensity}{Numeric vector of rainfall intensity}}
\references{CSDMS website. http://csdms.colorado.edu/wiki/Model:CHILD.\cr
Tucker, GE. 2010. CHILD Users Guide for version R9.4.1. 
http://csdms.colorado.edu/mediawiki/images/Child_users_guide.pdf \cr
Tucker, GE., Lancaster, ST., Gasparini, NM., Bras, RL. 2001. The 
Channel-Hillslope Integrated Landscape Development (CHILD) Model. 
In Harmon, RS.,  Doe, W.W. III (eds). Landscape Erosion and Evolution
Modeling. Kluwer Academic/Plenum Publishers, pp. 349-388.}
\author{Michael Dietze}



\seealso{\code{\link{display.SAP}}}
\examples{
# load example data set
data(rainfall1)

# create and plot the storm intensity time series for a given hour interval
STORM <- calc.storm(rainfall1, interval = c(1000, 2000))
STORM
}

\keyword{CHILD}
