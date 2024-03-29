% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pip_maddison.R
\name{pip_maddison}
\alias{pip_maddison}
\title{Maddison data}
\usage{
pip_maddison(action = "update", force = FALSE, maindir = gls$PIP_DATA_DIR)
}
\arguments{
\item{action}{character: Either "load" or "update". Default is "update". If
"update" data will be updated on the system. If "load" data is loaded in memory.}

\item{force}{logical: If TRUE data will be overwritten.}

\item{maindir}{character: Main directory of project.}
}
\description{
Load or update data from the Maddison project.
}
