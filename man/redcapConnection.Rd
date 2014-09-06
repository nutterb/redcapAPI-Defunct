\name{redcapConnection}
\alias{redcapConnection}
\title{Connect to a REDCap Database}
\description{Creates an object of class \code{redcapApiConnection} for using the REDCap API [or a direct 
connection through an SQL server]}
\usage{redcapConnection(url=getOption('redcap_api_url'), token, conn, project)}


\arguments{
 \item{url}{URL for a REDCap database API.  Check your institution's REDCap documentation for this address.  Either
            \code{url} or \code{conn} must be specified.}
 \item{token}{REDCap API token}
 \item{conn}{The database connection to be used. If used, \code{project} must also be used.}
 \item{project}{The project ID in the REDCap tables.}
}

\details{
For convenience, you may consider using \code{options(redcap_api_url=[your URL here])} in your RProfile.

To obtain an API token for a project, do the following:\cr
Enter the 'User Right' section of a project\cr
Select a user\cr
Check the box for 'API Data Export' or 'API Data Import,' as appropriate.


Tokens are specific to a project, and a token must be created for each project for which you wish to use the API.
}

\author{Jeffrey Horner}

\references{
  This functionality were originally developed by Jeffrey Horner in the \code{redcap} package.
  \url{https://github.com/vubiostat/redcap}
}

\examples{
\dontrun{
rcon <- redcapConnection(url=[YOUR_REDCAP_URL], token=[API_TOKEN])

options(redcap_api_url=[YOUR_REDCAP_URL])
rcon <- redcapConnection(token=[API_TOKEN])
}
}

\keyword{ methods }

