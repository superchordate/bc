% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean-vector.R
\name{clean.vector}
\alias{clean.vector}
\title{Clean a Vector}
\usage{
clean.vector(x, na.vals = bc::nastrings, verbose = TRUE)
}
\arguments{
\item{x}{Vector to clean.}

\item{na.vals}{Values to consider NA.}

\item{verbose}{Print informative output via cat.}
}
\value{
Cleaned vector.
}
\description{
Remove NA values and trims edge white-space.
}
\examples{
clean.vector( c( 'NA', 'Not NA') )
}
