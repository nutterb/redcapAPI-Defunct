\name{importRecords}
\alias{importRecords}
\title{Import Records to a REDCap Database}
\description{Imports records from a \code{data.frame} to a REDCap Database.}

\usage{
importRecords(rcon, data, overwriteBehavior=c('normal', 'overwrite'),
              returnContent=c('count', 'ids', 'nothing'),
              returnFormat=c('xml', 'csv', 'json'))
}


\arguments{
  \item{rcon}{A REDCap connection object as created by \code{redcapConnection}.}
  \item{data}{A \code{data.frame} to be imported to the REDCap project.}
  \item{overwriteBehavior}{Character string.  'normal' prevents blank fields from overwriting populated fields.  'overwrite' causes blanks to overwrite data in the REDCap database.}
  \item{returnContent}{Character string.  'count' returns the number of records imported; 'ids' returns the record ids that are imported; 'nothing' returns no message.}
  \item{returnFormat}{A character vector of forms to be returned. If \code{NULL}, all forms are returned.}
  \item{records}{A vector of study id's to be returned. If \code{NULL}, all subjects are returned.}
  \item{events}{Specifies the format of returned content or error messages..}
}

\details{A record of imports through the API is recorded in the Logging section of the project.}

\author{Benjamin Nutter\cr
        with thanks to Josh O'Brien and etb (see references)}

\references{
http://stackoverflow.com/questions/12393004/parsing-back-to-messy-api-strcuture/12435389#12435389\cr
https://github.com/etb/my-R-code/blob/master/R-pull-and-push-from-and-to-REDCap.R\cr
See also the REDCap API documentation

\examples{}

\keyword{ methods }
