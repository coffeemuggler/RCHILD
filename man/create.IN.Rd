\name{create.IN}
\alias{create.IN}
\title{Function to create an empty CHILD input file.}
\description{This function creates an S4-object IN from the example data set 
\code{empty.in()}. Primary use of this function is to create an 
object-skeleton to modify.}
\usage{create.IN()}

\value{An S4-object with all parameters as separate slots, parameters are set 
to \code{NA}.}
\references{CSDMS website. http://csdms.colorado.edu/wiki/Model:CHILD.\cr
Tucker, GE. 2010. CHILD Users Guide for version R9.4.1. 
http://csdms.colorado.edu/mediawiki/images/Child_users_guide.pdf \cr
Tucker, GE., Lancaster, ST., Gasparini, NM., Bras, RL. 2001. The 
Channel-Hillslope Integrated Landscape Development (CHILD) Model. 
In Harmon, RS.,  Doe, W.W. III (eds). Landscape Erosion and Evolution
Modeling. Kluwer Academic/Plenum Publishers, pp. 349-388.}
\author{Michael Dietze}



\seealso{\code{\link{write.IN}}, \code{\link{read.IN}}}
\examples{
empty.IN <- create.IN()
}

\keyword{CHILD}
