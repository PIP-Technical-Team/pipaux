% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pip_gdm_update.R
\name{pip_gdm_update}
\alias{pip_gdm_update}
\title{Update GDM}
\usage{
pip_gdm_update(
  force = FALSE,
  maindir = gls$PIP_DATA_DIR,
  pcndir = gls$PCN_MASTER
)
}
\arguments{
\item{force}{logical: If TRUE data will be overwritten.}

\item{maindir}{character: Main directory of project.}

\item{pcndir}{character: PovcalNet Masterfile directory.}
}
\description{
Update GDM data using the PovcalNet Masterfile.
}
\keyword{internal}
