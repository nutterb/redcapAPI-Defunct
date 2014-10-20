\name{apiCall}
\alias{apiCall}
\title{Execute a Call to the REDCap API}
\description{\code{apiCall} is a wrapper for attempting to access the API via \code{httr::POST}, and then via 
  \code{RCurl::postForm} if a particular error occurs.  This prevents a particular kind of error for which I haven't
  found a proper solution, but at least allows the expected behavior of the package.}

\usage{
  apiCall(url, body, config)
}


\arguments{
  \item{url}{URL of the REDCap API}
  \item{body}{List of parameters to be passed to \code{httr::POST}'s \code{body} argument or \code{RCurl::postForm}'s
    \code{.param} argument.}
  \item{config}{A list of options to be passed to \code{httr::POST}'s \code{config} argument or \code{RCurl::postForm}'s
    \code{.opts} argument.}
}

\details{
Somewhere in the middle of an upgrade to RStudio, R 3.1.1, and various other system changes, I began seeing the error
'GnuTLS recv error (-9): A TLS packet with unexpected length was received.'  I still don't know what this error means, 
but it only occurs when using \code{httr} on Linux.  The \code{RCurl} equivalents appear to work just fine.

In order to prevent this error from occurring, and making the package rather useless, \code{apiCall} wraps 
\code{httr::POST} into a \code{tryCatch} call.  If the GnuTLS error is thrown, \code{apiCall} then resorts to using the
\code{RCurl} equivalent call.  
}

\author{Benjamin Nutter}

\references{
Please refer to your institution's API documentation.

Additional details on API parameters are found on the package wiki at
\url{https://github.com/nutterb/redcapAPI/wiki/REDCap-API-Parameters}
}

\keyword{ methods }
