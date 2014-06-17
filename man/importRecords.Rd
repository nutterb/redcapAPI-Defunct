\name{importRecords}
\alias{importRecords}
\title{Import Records to a REDCap Database}
\description{Imports records from a \code{data.frame} to a REDCap Database.}

\usage{
importRecords(rcon, data, overwriteBehavior=c('normal', 'overwrite'),
              returnContent=c('count', 'ids', 'nothing'),
              returnFormat=c('xml', 'csv', 'json'),
              returnData=FALSE, logfile="")
}


\arguments{
  \item{rcon}{A REDCap connection object as created by \code{redcapConnection}.}
  \item{data}{A \code{data.frame} to be imported to the REDCap project.}
  \item{overwriteBehavior}{Character string.  'normal' prevents blank fields from overwriting populated fields.  'overwrite' causes blanks to overwrite data in the REDCap database.}
  \item{returnContent}{Character string.  'count' returns the number of records imported; 'ids' returns the record ids that are imported; 'nothing' returns no message.}
  \item{returnFormat}{A character vector of forms to be returned. If \code{NULL}, all forms are returned.}
  \item{returnData}{Logical.  Prevents the REDCap import and instead returns the data frame that would have been given
                    for import.  This is sometimes helpful if the API import fails with message "Bad Request."  The
                    data frame can be written to a csv and uploaded using the interactive tools to troubleshoot the
                    problem.  Please shoot me an e-mail if you find errors I havne't accounted for.}
  \item{logfile}{An optional filepath (preferably .txt) in which to print the log of errors and warnings about the data. 
                 If \code{""}, the log is printed to the console.}
}

\details{A record of imports through the API is recorded in the Logging section of the project.

  \code{importRecords} prevents the most common import errors by testing the data before attempting the import.  Namely
  \enumerate{
    \item Check that all variables in \code{data} exist in the REDCap data dictionary.
    \item Check that the study id variable exists
    \item Force the study id variable to the first position in the data frame
    \item Verify that REDCap date fields are represented in the data frame as either character, POSIXct, or Date class
          objects.
  }

}

\author{Benjamin Nutter\cr
        with thanks to Josh O'Brien and etb (see references)}

\references{
http://stackoverflow.com/questions/12393004/parsing-back-to-messy-api-strcuture/12435389#12435389\cr
https://github.com/etb/my-R-code/blob/master/R-pull-and-push-from-and-to-REDCap.R\cr
See also the REDCap API documentation}


\keyword{ methods }
