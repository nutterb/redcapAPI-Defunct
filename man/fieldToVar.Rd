\name{fieldToVar}
\alias{fieldToVar}
\title{Convert a REDCap Data Field to an R Vector}
\description{Converts a field exported from REDCap into a valid R vector}

\usage{fieldToVar(m, d, factors=TRUE, dates=TRUE, checkboxLabels=FALSE, vname)}


\arguments{
  \item{m}{A metadata file, as returned by \code{exportMetaData}}
  \item{d}{A data file, as returned by \code{exportRecords}}
  \item{factors}{Logical. Determines if categorical data from REDCap are returned with their numeric codes, 
    or as labelled factors.}
  \item{dates}{Logical. Determines if date variables from REDCap are converted to POSIXlt format.  The API 
               returns dates as character strings by default in YYYY-MM-DD format.}
  \item{checkboxLabels}{Logical. Determines the format of labels in checkbox variables.  If \code{FALSE} labels are
    applies as "Unchecked"/"Checked".  If \code{TRUE}, they are applied as ""/"[field_label]" where [field_label] is the
    label assigned to the level in the data dictionary.  This option only applies when \code{factors=TRUE} and only to 
    REDCap versions 6.0 and higher.}
  \item{vname}{The variable name being converted.  This is used only when \code{checkboxLabels=TRUE} in order to 
    extract the checkbox label.}
}

\details{
This function is called internally by \code{exportRecords} and \code{exportReports}. It is not available to the user.
}


\author{Jeffrey Horner}


\keyword{ methods }

