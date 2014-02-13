\name{fieldToVar}
\alias{fieldToVar}
\title{Convert a REDCap Data Field to an R Vector}
\description{Converts a field exported from REDCap into a valid R vector}

\usage{fieldToVar(m, d, factors=TRUE, dates=TRUE)}


\arguments{
  \item{m}{A metadata file, as returned by \code{exportMetaData}}
  \item{d}{A data file, as returned by \code{exportRecords}}
  \item{factors}{Logical. Determines if categorical data from REDCap are returned with their numeric codes, 
    or as labelled factors.}
  \item{dates}{Logical. Determines if date variables from REDCap are converted to POSIXlt format.  The API 
               returns dates as character strings by default in YYYY-MM-DD format.}
}

\details{}

\author{Jeffrey Horner}

\examples{}

\keyword{ methods }

