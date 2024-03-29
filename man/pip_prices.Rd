% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pip_prices.R
\name{pip_prices}
\alias{pip_prices}
\title{PIP Prices}
\usage{
pip_prices(
  measure = NULL,
  action = "update",
  maindir = gls$PIP_DATA_DIR,
  dlwdir = Sys.getenv("PIP_DLW_ROOT_DIR"),
  force = FALSE
)
}
\arguments{
\item{measure}{character: Measure to be used. e.g., "cpi" or "ppp".}

\item{action}{character: Either "load" or "update". Default is "update". If
"update" data will be updated on the system. If "load" data is loaded in memory.}

\item{maindir}{character: Main directory of project.}

\item{dlwdir}{character: Datalibweb directory.}

\item{force}{logical: If TRUE data will be overwritten.}
}
\description{
Works with either CPI, PPP or PFW.
}
