\name{display.SAP}
\alias{display.SAP}
\title{Function to plot slope vs. area.}
\description{This function creates a plot of slope va. area for all interiour nodes of
a CHILD model run, for each specified time step separately. The plot data
sets are returned as well.}
\usage{display.SAP(dataset, timesteps, plot = TRUE, ...)}
\arguments{
  \item{dataset}{(S4-object) CHILD model run output data set.}
  \item{timesteps}{(numeric vector) Time steps use for calculation. If missing, all modelled
time steps are used.}
  \item{plot}{(logical scalar) Option to show or hide plot output (dafault is TRUE).}
  \item{\dots}{Further plot parameters to pass to the plot object.}
}
\details{No details yet}
\value{A list object
\item{data}{A list with area and slope data per time step.}
\item{ranges}{A matrix with data ranges of area and slope.}}
\references{CSDMS website. http://csdms.colorado.edu/wiki/Model:CHILD.\cr
Tucker, GE. 2010. CHILD Users Guide for version R9.4.1. 
http://csdms.colorado.edu/mediawiki/images/Child_users_guide.pdf \cr
Tucker, GE., Lancaster, ST., Gasparini, NM., Bras, RL. 2001. The 
Channel-Hillslope Integrated Landscape Development (CHILD) Model. 
In Harmon, RS.,  Doe, W.W. III (eds). Landscape Erosion and Evolution
Modeling. Kluwer Academic/Plenum Publishers, pp. 349-388.}
\author{Michael Dietze}



\seealso{\code{\link{display.surface}},\code{\link{calc.storm}}}
\examples{
# load example data set
data(hillslope1)

# plot slope vs. area for time steps 2 to 5 in a log-log-plot
SAP <- display.SAP(hillslope1, 
                  timesteps = 2:5, 
                  main = "Slope-area-plot", 
                  xlab = "area [square metres]", 
                  ylab = "slope [degree]",
                  log = "xy",
                  cex = 0.4)

# show some numeric output for time step 2
SAP$data[[2]][1:5,]
SAP$ranges
}

\keyword{CHILD}
