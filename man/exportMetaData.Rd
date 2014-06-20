\name{exportMetaData}
\alias{exportMetaData}
\alias{exportMetaData.redcapDbConnection}
\alias{exportMetaData.redcapApiConnection}
\title{Export Meta Data from a REDCap Database}
\description{Retrieves the meta data for a REDcap database, including field names, labels, types, formulas, etc.  This
file can be used to parse levels of factors, apply labels, and other data management tasks once the data are 
retrieved.}

\usage{
\method{exportMetaData}{redcapDbConnection}(rcon)
\method{exportMetaData}{redcapApiConnection}(rcon)
}


\arguments{
 \item{rcon}{An API connection created by \code{redcapConnection}.}
}

\details{A record of this export is placed in the REDCap logging page, but the file that is exported is not stored in 
the database.}

\author{Jeffrey Horner}

\examples{
\dontrun{
#*** Note: I cannot provide working examples without
#*** compromising security.  Instead, I will try to 
#*** offer up sample code with the matching results


#*** Create the connection object
rcon <- redcapConnection(url=[YOUR_REDCAP_URL], token=[API_TOKEN])

exportMetaData(rcon)
}
}

\keyword{ methods }
