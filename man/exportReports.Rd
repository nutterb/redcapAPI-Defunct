\name{exportReports}
\alias{exportReports}
\alias{exportReports.redcapDbConnection}
\alias{exportReports.redcapApiConnection}
\title{Export Reports from a REDCap Database}
\description{Exports reports from a REDCap Database and formats data if requested}

\usage{
\method{exportRecords}{redcapDbConnection}(rcon, report_id, factors=TRUE, labels=TRUE, dates=TRUE, ...)
\method{exportRecords}{redcapApiConnection}(rcon, report_id, factors=TRUE, labels=TRUE, dates=TRUE, ...,
    meta_data=getOption("redcap_project_info")$meta_data)
}


\arguments{
  \item{rcon}{A REDCap connection object as created by \code{redcapConnection}.}
  \item{factors}{Logical.  Determines if categorical data from the database is returned as numeric codes or labelled 
    factors.}
  \item{labels}{Logical.  Determines if the variable labels are applied to the data frame.}
  \item{dates}{Logical. Determines if date variables are converted to POSIXlt format during the download.}
  \item{meta_data}{A data frame of the project meta_data.  This is used to do parameter checking prior to making
    the call to the API.  It is strongly recommended that you run \code{redcapProjectInfo} prior to exporting
    records.  If necessary, an API call will be made to retrieve this table.}
  \item{...}{Additional arguments to be passed between methods.}
}

\details{A record of exports through the API is recorded in the Logging section of the project.

Reports are exported based on their id number, which can be looked up in the Reports page of a project.
}


\author{Benjamin Nutter}
