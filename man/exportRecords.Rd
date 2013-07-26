\name{exportRecords}
\alias{exportRecords}
\alias{exportRecords.redcapDbConnection}
\alias{exportRecords.redcapApiConnection}
\title{Export Records from a REDCap Database}
\description{Exports records from a REDCap Database, allowing for subsets of subjects, fields, records, and events.}

\usage{
\method{.redcapDbConnection}{exportRecords}(rcon,factors=TRUE,fields=NULL,forms=NULL,records=NULL,events=NULL)
\method{.redcapApiConnection}{exportRecords}(rcon,factors=TRUE,fields=NULL,forms=NULL,records=NULL,events=NULL)
} 


\arguments{
  \item{rcon}{A REDCap connection object as created by \code{redcapConnection}.}
  \item{factors}{Logical.  Determines if categorical data from the database is returned as numeric codes or labelled 
    factors.}
  \item{fields}{A character vector of fields to be returned.  If \code{NULL}, all fields are returned.}
  \item{forms}{A character vector of forms to be returned.  If \code{NULL}, all forms are returned.}
  \item{records}{A vector of study id's to be returned.  If \code{NULL}, all subjects are returned.}
  \item{events}{A character vector of events to be returned from a longitudinal database.  If \code{NULL}, all
    events are returned.}
}

\details{A record of exports through the API is recorded in the Logging section of the project.}

\author{Jeffrey Horner}

\examples{}

\keyword{ methods }

