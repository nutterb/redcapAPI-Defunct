\name{downloadRedcap}
\alias{downloadRedcap}

\title{Download a dataset from a REDCap database}
\description{Retrieves the data and metadata from a database in REDCap and creates appropriate factors and levels.}

\usage{
downloadRedcap(APIKEY, url=options('redcap_api_url'), 
               fields=NULL, forms=NULL, records=NULL, events=NULL, 
               raw=FALSE, label=TRUE)
}


\arguments{
 \item{APIKEY}{Character string giving the API token for the database.}
 \item{url}{Character string giving the API url.}
 \item{fields}{Optional character vector the specify which fields to pull from the database.}
 \item{forms}{Optional character vector to specify which forms to pull from the database.}
 \item{records}{Optional character vector to specify which records to pull from the database.}
 \item{events}{Optional character vector to specify which events to pull from the database.}
 \item{raw}{Raw format supplies data in numeric codes.  \code{raw = FALSE} returns the categorical data as factors.
 \item{label}{Toggles if variable labels are applied to the data}
}

\details{A record of this export is placed in the REDCap logging page, but the file that is exported is not stored in 
the database.  

This is a wrapper for using \code{exportMetaData} and \code{exportRecords} to generate a complete data set.  The 
meta data is parsed to create factor levels and labels, if requested.  For convenience, the API url may be set using 
the options, or may be hard coded into the function call.}

\author{Benjamin Nutter (Cleveland Clinic)\cr
        JoAnn Alvarez (Vanderbilt University)}

\examples{}

\keyword{ methods }
