\name{importRecords}
\alias{importRecords}
\alias{importRecords.redcapDbConnection}
\alias{importRecords.redcapApiConnection}
\title{Import Records to a REDCap Database}
\description{Imports records from a \code{data.frame} to a REDCap Database.}

\usage{
\method{importRecords}{redcapDbConnection}(rcon, data, 
                          overwriteBehavior=c('normal', 'overwrite'),
                          returnContent=c('count', 'ids', 'nothing'),
                          returnData=FALSE, logfile="", ...)
\method{importRecords}{redcapApiConnection}(rcon, data, 
                            overwriteBehavior=c('normal', 'overwrite'),
              returnContent=c('count', 'ids', 'nothing'),
              returnData=FALSE, logfile="", ...,
              proj=NULL, batch.size=-1)
}


\arguments{
  \item{rcon}{A REDCap connection object as created by \code{redcapConnection}.}
  \item{data}{A \code{data.frame} to be imported to the REDCap project.}
  \item{proj}{A \code{redcapProject} object as created by \code{redcapProjectInfo}.}
  \item{overwriteBehavior}{Character string.  'normal' prevents blank fields from overwriting populated fields.  'overwrite' causes blanks to overwrite data in the REDCap database.}
  \item{returnContent}{Character string.  'count' returns the number of records imported; 'ids' returns the record ids that are imported; 'nothing' returns no message.}
  \item{returnData}{Logical.  Prevents the REDCap import and instead returns the data frame that would have been given
                    for import.  This is sometimes helpful if the API import fails without providing an 
                    informative message. The
                    data frame can be written to a csv and uploaded using the interactive tools to troubleshoot the
                    problem.  Please shoot me an e-mail if you find errors I havne't accounted for.}
  \item{logfile}{An optional filepath (preferably .txt) in which to print the log of errors and warnings about the data. 
                 If \code{""}, the log is printed to the console.}
  \item{batch.size}{Specifies size of batches.  A negative value indicates no batching.}
  \item{...}{Arguments to be passed to other methods.}

}

\details{A record of imports through the API is recorded in the Logging section of the project.

  \code{importRecords} prevents the most common import errors by testing the data before attempting the import.  Namely
  \enumerate{
    \item Check that all variables in \code{data} exist in the REDCap data dictionary.
    \item Check that the study id variable exists
    \item Force the study id variable to the first position in the data frame (with a warning)
    \item Remove calculated fields (with a warning)
    \item Verify that REDCap date fields are represented in the data frame as either character, POSIXct, or Date class
          objects.
    \item Determine if values are within their specified validation limits.
  }
  See the documentation for \code{\link{validateImport}} for detailed explanations of the validation.
}

\author{Benjamin Nutter\cr
        with thanks to Josh O'Brien and etb (see references)}

\references{
\url{http://stackoverflow.com/questions/12393004/parsing-back-to-messy-api-strcuture/12435389#12435389}\cr
\url{https://github.com/etb/my-R-code/blob/master/R-pull-and-push-from-and-to-REDCap.R}\cr
See also the REDCap API documentation

Please refer to your institution's API documentation.

Additional details on API parameters are found on the package wiki at
\url{https://github.com/nutterbAPI/redcap/wiki/REDCap-API-Parameters}
}

\seealso{
\code{\link{validateImport}}
}

\examples{
\dontrun{
> #*** Note: I cannot provide working examples without
> #*** compromising security.  Instead, I will try to 
> #*** offer up sample code with the matching results
> 
> 
> #*** Create the connection object
> rcon <- redcapConnection(url=[YOUR_REDCAP_URL], token=[API_TOKEN])
> 
> 
> #*** Import a record for a new patient
> NewScan <- data.frame(patient_id = 1022,
+                       redcap_event_name = "entry_arm_1",
+                       bmi = 24.689,
+                       patient_characteristics_complete = 1)
> 
> importRecords(rcon, NewScan)
REDCap Data Import Log: 2014-06-20 16:08:31
The following (if any) conditions were noted about the data.


1
> ## No conditions were noted, 1 record was uploaded
>
>
> 
> 
> 
> #*** Import a record for a new patient with an erroneous BMI value
> NewScan <- data.frame(patient_id = 1022,
+                       redcap_event_name = "entry_arm_1",
+                       bmi = 244.689,
+                       patient_characteristics_complete = 4)
> 
> importRecords(rcon, NewScan)
REDCap Data Import Log: 2014-06-20 16:08:33
The following (if any) conditions were noted about the data.


1022   entry_arm_1   244.689   Entry for 'bmi' is larger than 
       the acceptable maximum.  Please confirm.
1
> ## One condition was noted.  Notice that the BMI value was still
> ## uploaded to REDCap.  
}
}

\keyword{ methods }
