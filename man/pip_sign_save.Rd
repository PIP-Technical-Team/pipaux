% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pip_sign_save.R
\name{pip_sign_save}
\alias{pip_sign_save}
\title{Save PIP auxiliary data}
\usage{
pip_sign_save(x, measure, msrdir, force = FALSE, save_dta = TRUE)
}
\arguments{
\item{x}{data.frame Data frame to be signed and saved.}

\item{measure}{character: Measure to be used. e.g., "cpi" or "ppp".}

\item{msrdir}{character: Directory where the data and data signature will be saved.}

\item{force}{logical: If TRUE data will be overwritten.}

\item{save_dta}{logical: If TRUE a Stata (.dta) version of the dataset is also saved.}
}
\description{
Save PIP auxiliary data with data signature.
}
\keyword{internal}
