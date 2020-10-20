\name{write.IN}
\alias{write.IN}
\title{Function to write CHILD input files.}
\description{This function creates and writes a CHILD input file from a 
S4-object (IN).}
\usage{write.IN(IN, filename)}
\arguments{
  \item{IN}{(character scalar) Name of the S4-object that will be converted.}
  \item{filename}{(character scalar) File name of the CHILD input file that will  
be created, with extension.}
}
\details{The function only writes input parameters to the *.in-file from
slots that are not \code{NA}.}
\value{A CHILD input file (*.in).}
\references{CSDMS website. http://csdms.colorado.edu/wiki/Model:CHILD.\cr
Tucker, GE. 2010. CHILD Users Guide for version R9.4.1. 
http://csdms.colorado.edu/mediawiki/images/Child_users_guide.pdf \cr
Tucker, GE., Lancaster, ST., Gasparini, NM., Bras, RL. 2001. The 
Channel-Hillslope Integrated Landscape Development (CHILD) Model. 
In Harmon, RS.,  Doe, W.W. III (eds). Landscape Erosion and Evolution
Modeling. Kluwer Academic/Plenum Publishers, pp. 349-388.}
\author{Michael Dietze}



\seealso{\code{\link{read.IN}}}
\examples{
# load example data set
data(hillslope1.in)

# write example CHILD input file
write.IN(hillslope1.in, filename = "hillslope1")
}

\keyword{CHILD}
