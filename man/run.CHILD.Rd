\name{run.CHILD}
\alias{run.CHILD}
\title{Function to run CHILD from the R interface.}
\description{The function executes a CHILD model run from the R console and creates a
S4-object \code{\link{CHILD}} with all output files. The function  
requires a compiled version of CHILD and an appropriate input-file.}
\usage{run.CHILD(inputfile, outfiles = FALSE, path)}
\arguments{
  \item{inputfile}{(character scalar) Name of the input file, with extention.}
  \item{outfiles}{(logical scalar) Argument to keep (\code{\link{TRUE}}) or delete 
(\code{\link{FALSE}}) the original output files of the CHILD run, 
default is \code{\link{FALSE}}.}
  \item{path}{(character scalar) Optional path to the compiled CHILD executable. If 
not specified, it is assumed that the executable is present in the 
current working directory.}
}
\details{Currently, this function is of preliminary stage. It requires a compiled  
version of CHILD, present at a specified path or the current working 
directory. Compiled versions for MS Windows 32 Bit and some Mac OS may 
be downloaded from CSDMS (http://csdms.colorado.edu/wiki/Model:CHILD, 
scroll down to download, fill the form fields, choose a recent release 
version and you will find the compiled versions in 
ChildExercises/Executables_v10). Existing output files will be overwritten 
without prompting, so be careful and check before running.}
\value{A S4-object with all CHILD run output files.}
\references{CSDMS website. http://csdms.colorado.edu/wiki/Model:CHILD.\cr
Tucker, GE. 2010. CHILD Users Guide for version R9.4.1. 
http://csdms.colorado.edu/mediawiki/images/Child_users_guide.pdf \cr
Tucker, GE., Lancaster, ST., Gasparini, NM., Bras, RL. 2001. The 
Channel-Hillslope Integrated Landscape Development (CHILD) Model. 
In Harmon, RS.,  Doe, W.W. III (eds). Landscape Erosion and Evolution
Modeling. Kluwer Academic/Plenum Publishers, pp. 349-388.}
\author{Michael Dietze}



\seealso{\code{\link{read.CHILD}}, \code{\link{read.IN}}, \code{\link{write.IN}}}
\examples{
# To run example code uncomment it (delete hashes)

# You will need a compiled version of CHILD either in
# the working directory or under a known path.

# hillslope1 <- run.CHILD(dataset = "hillslope1", 
# inputfile = "hillslope1.in")

}

\keyword{CHILD}
