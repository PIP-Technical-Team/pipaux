% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pip_regions.R
\name{pip_regions}
\alias{pip_regions}
\title{PIP Regions}
\usage{
pip_regions(action = "update", force = FALSE, maindir = gls$PIP_DATA_DIR)
}
\arguments{
\item{action}{character: Either "load" or "update". Default is "update". If
"update" data will be updated on the system. If "load" data is loaded in memory.}

\item{force}{logical: If TRUE data will be overwritten.}

\item{maindir}{character: Main directory of project.}
}
\description{
Update or load a dataset with regions.
}
