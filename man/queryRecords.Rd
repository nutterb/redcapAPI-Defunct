\name{queryRecords}
\alias{queryRecords}
\title{Query a REDCap Database}
\description{Allows for selection of fields, forms, records, and events from a REDCap database.}
\usage{queryRecords(rcon,fields=NULL,forms=NULL,records=NULL,events=NULL)}


\arguments{
  \item{rcon}{A connection object as created by \code{redcapConnection}}
  \item{fields}{A character vector of fields to be chosen from the database.}
  \item{forms}{A character vector naming the forms to be chosen from the database.}
  \item{records}{A vector of subject id's to be chosen from the database.  The default field name for the id in REDCap 
    is 'study_id', but it is possible for the user to change this.  Only exact matches are selected.}
  \item{events}{A character vector of events from a longitudinal database to be chosen from the database.}
}

\details{This function is called internally by \code{exportRecords.redcapDbConnection} and is not available to the user.}

\author{Jeffrey Horner}

\references{
This functionality were originally developed by Jeffrey Horner in the \code{redcap} package.
\url{https://github.com/vubiostat/redcap}
}

\keyword{ methods }

