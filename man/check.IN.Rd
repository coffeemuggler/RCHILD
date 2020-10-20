\name{check.IN}
\alias{check.IN}
\title{Function to check a CHILD input file for }
\description{This function checks an S4-object \code{IN} for consistency. It can work 
consistency. (detected issues noticed) or active (detected issues changed) 
mode. A list of in passive detected issues is returned. Not assgined (empty) 
and unused parameters are checked.}
\usage{check.IN(IN, mode)}
\arguments{
  \item{IN}{(character scalar) Name of the S4-object that will be checked.}
  \item{mode}{(logical) Mode of how to deal with detected issues: active (issues will 
be changed) as far as possible) or passive (issues will be reported only), 
default is passive.}
}
\details{This function is primarily designed for two reasons: i) to identify 
and potentially adjust common errors in the input-file prior to run
CHILD and ii) to remove uncessary parameters from the input-file.
Although CHILD reports some errors in the input-file, treating errors
directly in R allows for automatic error-handling rather than user-
interaction with printed error or warning messages.\cr
The function is of preliminary stage, yet. So far, only the presence of
mandatory parameters ist checked and potentially corrected, i.e. assigned
with dummy values from some of the example input-files from the CSDMS 
website. PLEASE HELP AND REPORT MISSING FUNCTIONALITY AND BUGS TO THE
MAINTAINER!}
\value{A list object
\item{IN}{S4-object with the (potentially modified) input-file}
\item{warnings}{Character matrix with warnings}
\item{notifications}{Character matrix with notes
}}
\references{CSDMS website. http://csdms.colorado.edu/wiki/Model:CHILD.\cr
Tucker, GE. 2010. CHILD Users Guide for version R9.4.1. 
http://csdms.colorado.edu/mediawiki/images/Child_users_guide.pdf \cr
Tucker, GE., Lancaster, ST., Gasparini, NM., Bras, RL. 2001. The 
Channel-Hillslope Integrated Landscape Development (CHILD) Model. 
In Harmon, RS.,  Doe, W.W. III (eds). Landscape Erosion and Evolution
Modeling. Kluwer Academic/Plenum Publishers, pp. 349-388.}
\author{Michael Dietze}



\seealso{\code{\link{write.IN}}, \code{\link{read.IN}}, \code{\link{create.IN}}}
\examples{
# load example data set
data(hillslope1.in)

# check inputfile in active mode
check <- check.IN(hillslope1.in, mode = "active")

# extract checked and corrected IN-object
hillslope1c.in <- check$IN

# show warnings report
check$warnings
}

\keyword{CHILD}
