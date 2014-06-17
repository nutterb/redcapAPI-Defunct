\name{validateImport}
\alias{validateImport}
\title{Validate Data Frames for Import}
\description{Validates the variables in a data frame prior to attempting an import to REDCap.}

\usage{
validateImport(field, meta_data, records, ids,
               logfile="")
}


\arguments{
  \item{field}{Character(1) naming the variable to be validated.}
  \item{meta_data}{REDCap database meta data.}
  \item{records}{The data frame to be validated.}
  \item{ids}{Character vector giving the names of fields that uniquely identify a record.  Usually the study id and
             \code{redcap_event_name}.}
  \item{logfile}{A character string giving the filepath to which the results of the validation are printed.  If 
                 \code{""}, the results are printed in the console.}
}

\details{Each variable is validated by matching they type of variable with the type listed in the REDCap database.  

Although the log messages will indicate a preference for dates to be in mm/dd/yyyy format, the function will accept
mm/dd/yy, yyyy-mm-dd, yyyy/mm/dd, and yyyymmdd formats as well.  When possible, pass dates as Date objects or POSIXct 
objects to avoid confusion.  Dates are also compared to minimum and maximum values listed in the data dictionary.  
Records where a date is found out of range are allowed to import and a message is printed in the log.

For continuous variables, the values are checked against the minimum and maximum allowed in the data dictionary. 
Records where a date is found out of range are allowed to import and a message is printed in the log.

ZIP codes are tested to see if they fit either the 5 digit or 5 digit + 4 format.  When these conditions are not met, 
the data point is deleted and a message printed in the log.

YesNo fields permit any of the values 'yes', 'no', '0', '1' to be imported to REDCap with 0=No, and 1=Yes.  The values 
are converted to lower case for validation, so any combination of lower and upper case values will pass (ie, the data
frame is not case-sensitive).

TrueFalse fields will accept TRUE, FALSE, 0, 1, and logical values and are also not case-sensitive.

Radio and dropdown fields may have either the coding in the data dictionary or the labels in the data dictionary. 
The validation will use the meta data to convert any matching values to the appropriate coding before importing to 
REDCap.  Values that cannot be reconciled are deleted with a message printed in the log.  Currently, these variable
\emph{are} case-sensitive.

Checkbox fields require a value of "Checked", "Unchecked", "0", or "1".  These are currently case sensitive.  Values 
that do not match these are deleted with a warning printed in the log.

}

\author{Benjamin Nutter}

\keyword{ methods }
