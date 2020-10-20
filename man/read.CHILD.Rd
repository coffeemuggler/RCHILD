\name{read.CHILD}
\alias{read.CHILD}
\title{Function to read files to a CHILD-object.}
\description{This function reads all present output files of a CHILD model run and 
creates an S4-object (CHILD) with the respective data. }
\usage{read.CHILD(dataset)}
\arguments{
  \item{dataset}{(character scalar) Name of the CHILD run, i.e. common file names of the 
output files, without extension. All the files must be present in the 
same directory. Either the  workspace or a path to this CHILD run 
directory must be set appropriately.}
}

\value{An S4-object with all output data of a CHILD model run.}
\references{CSDMS website. http://csdms.colorado.edu/wiki/Model:CHILD.\cr
Tucker, GE. 2010. CHILD Users Guide for version R9.4.1. 
http://csdms.colorado.edu/mediawiki/images/Child_users_guide.pdf \cr
Tucker, GE., Lancaster, ST., Gasparini, NM., Bras, RL. 2001. The 
Channel-Hillslope Integrated Landscape Development (CHILD) Model. 
In Harmon, RS.,  Doe, W.W. III (eds). Landscape Erosion and Evolution
Modeling. Kluwer Academic/Plenum Publishers, pp. 349-388.}
\author{Michael Dietze}



\seealso{\code{\link{read.IN}}, \code{\link{write.IN}}, 
\code{\link{write.raster}}, \code{\link{display.surface}}}
\examples{
# Examples are not run, remove hashes to run them

# # Run CHILD to create a model output
  # run.CHILD("hillslope1", "hillslope1.in")

# # Import the model run output data set
  # hillslope1 <- read.CHILD("hillslope1/hillslope1")

# # Show the summary of the imported data set
  # hillslope1@summary  
}

\keyword{CHILD}
