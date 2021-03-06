% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spl.R
\name{spl}
\alias{spl}
\title{Sample data frame or vector.}
\usage{
spl(x, n = 10, return.index = FALSE)
}
\arguments{
\item{x}{Vector to be covnerted to numeric.}

\item{n}{Strings to consider NA.}

\item{return.index}{Return just the sample index, not the actual data.}
}
\value{
Sampled data.
}
\description{
Uniform random sample from the input.
}
\examples{
spl( cars )
spl( cars$speed )
}
