% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.any.R
\name{read.any}
\alias{read.any}
\title{Read Any}
\usage{
read.any(path, folder = NULL, sheet = 1, do.time = FALSE,
  min.acceptable.date = "1-1-1900", max.acceptable.date = "12-31-2100",
  nastrings = bc::nastrings)
}
\arguments{
\item{path}{Path to file within a folder, or full path.}

\item{folder}{Folder to look for path in, if applicable.}

\item{sheet}{If an excel file, the sheet to load.}

\item{do.time}{Allow processing to time values.}

\item{min.acceptable.date}{Set to NULL to ignore. Sometimes numbers are assumed to be excel-formatted. One way to prevent this is to set min/max acceptable dates to help the conversion know if something is a meaningful data or not.}

\item{max.acceptable.date}{Set to NULL to ignore. Sometimes numbers are assumed to be excel-formatted. One way to prevent this is to set min/max acceptable dates to help the conversion know if something is a meaningful data or not.}
}
\value{
Tibble.
}
\description{
Flexible read function which also handles conversion to data types, and common data cleaning tasks.
}
\examples{
# TODO
}
