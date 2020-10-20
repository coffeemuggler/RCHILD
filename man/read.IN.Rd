\name{read.IN}
\alias{read.IN}
\title{Function to read a CHILD input file.}
\description{This function reads the parameters of a CHILD input file (*.in) and
writes them to an S4-object. Non-specified parameters are assigned
NA.}
\usage{read.IN(IN)}
\arguments{
  \item{IN}{(character scalar) Name of the CHILD input file to be read, with 
extention.}
}
\details{There may occur a warning message (In initialize(value, ...) : NAs 
introduced by coercion). This is the result of non-conform characters 
in the input-file such as brackets and space-separated words. This 
warning message does not  change the content of the created S4-object.\cr
It is important to specify OPINTRVL above TSOPINTRVL in the input-file 
to read. This is due to the text pattern matching algorithm. All other 
parameters may be specified in any order desired.}
\value{An S4-object with all possible parameters as separate slots. Empty 
parameters are assgined NA.}
\references{CSDMS website. http://csdms.colorado.edu/wiki/Model:CHILD.\cr
Tucker, GE. 2010. CHILD Users Guide for version R9.4.1. 
http://csdms.colorado.edu/mediawiki/images/Child_users_guide.pdf \cr
Tucker, GE., Lancaster, ST., Gasparini, NM., Bras, RL. 2001. The 
Channel-Hillslope Integrated Landscape Development (CHILD) Model. 
In Harmon, RS.,  Doe, W.W. III (eds). Landscape Erosion and Evolution
Modeling. Kluwer Academic/Plenum Publishers, pp. 349-388.}
\author{Michael Dietze}



\seealso{\code{\link{write.IN}}, \code{\link{read.CHILD}}}
\examples{
# hillslope1.in <- read.IN("hillslope1")
}

\keyword{CHILD}
