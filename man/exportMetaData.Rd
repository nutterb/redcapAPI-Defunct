\name{exportMetaData}
\alias{exportMetaData}
\alias{exportMetaData.redcapDbConnection}
\alias{exportMetData.ApiConnection}
\title{Export Meta Data from a REDCap Database}
\description{Retrieves the meta data for a REDcap database, including field names, labels, types, formulas, etc.  This
file can be used to parse levels of factors, apply labels, and other data management tasks once the data are 
retrieved.}

\usage{
\method{redcapDbConnection}{exportMetaData}(rcon)
\method{redcapApiConnection}{exportMetaData}(rcon)
}


\arguments{
 \item{rcon}{An API connection created by \code{redcapConnection}.}
}

\details{A record of this export is placed in the REDCap logging page, but the file that is exported is not stored in 
the database.}

\author{Jeffrey Horner}

\keyword{ methods }
