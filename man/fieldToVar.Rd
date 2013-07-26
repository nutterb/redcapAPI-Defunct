\name{fieldToVar}
\alias{fieldToVar}
\title{Convert a REDCap Data Field to an R Vector}
\description{Converts a field exported from REDCap into a valid R vector}

\usage{fieldToVar(m, d, factors=TRUE)}


\arguments{
  \item{m}{A metadata file, as returned by \code{exportMetaData}}
  \item{d}{A data file, as returned by \code{exportRecords}}
  \item{factors}{Logical. Determines if categorical data from REDCap are returned with their numeric codes, 
    or as labelled factors.}
}

\details{}

\author{Jeffrey Horner}

\examples{}

\keyword{ methods }

