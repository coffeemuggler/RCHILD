\name{RCHILD-package}
\alias{RCHILD-package}
\alias{RCHILD}
\docType{package}
\title{Functions for flexible use of the landscape evolution model CHILD}
\description{The CHILD (Channel-Hillslope Integrated Landscape Development) model is a numerical landscape evolution model (LEM). Basic
information about CHILD and the capabilites are explained in Tucker et al. (2001). The model is operated as a compiled
program from the command line, in combination with an input file (*.in). This input file comprises all
modell parameters. CHILD produces no graphical output but delivers a series of ASCII-files with numerical output (for
details see Tucker, 2010).\cr
In order to work with this model more flexible, to dynamically change input parameters and run the modified model, to create
visual output such as variable plots, surface maps or surface animations, and to establish a link between CHILD and other
software for spatial data analysis, this package provides general functions. Many of the functions are based on the
Matlab-scripts available at the Community Surface Dynamics Modeling System (CSDMS, 2012) and greatly benefited from frequent
support by founders and users of CHILD.\cr\cr
References\cr
CSDMS. 2012. Community Surface Dynamics Modeling System.\cr
Tucker, GE. 2010. CHILD Users Guide for version R9.4.1.\cr
Tucker, GE., Lancaster, ST., Gasparini, NM., Bras, RL. 2001. The Channel-Hillslope Integrated Landscape Development (CHILD)
Model. In Harmon, RS.,  Doe, W.W. III (eds). Landscape Erosion and Evolution Modeling. Kluwer Academic/Plenum Publishers,
pp. 349-388.}
\details{
\tabular{ll}{Package: \tab RCHILD\cr
Type: \tab Package\cr
Title: \tab Functions for flexible use of the landscape evolution model CHILD\cr
Version: \tab 0.2.3\cr
Date: \tab 2013-01-31\cr
Author: \tab Michael Dietze\cr
Maintainer: \tab Michael Dietze <micha.dietze@mailbox.tu-dresden.de>\cr
License: \tab GPL-3\cr
Depends: \tab R (>= 2.15.1), akima, fields, raster, rgdal, rgl, animation\cr
Imports: \tab methods\cr}
}
\author{Michael Dietze}

\keyword{ package }


