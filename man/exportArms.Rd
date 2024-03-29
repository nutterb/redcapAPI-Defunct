% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exportArms.R
\name{exportArms}
\alias{exportArms}
\alias{exportArms.redcapDbConnection}
\alias{exportArms.redcapApiConnection}
\title{Export the Arms for a Project}
\usage{
exportArms(rcon, ...)

\method{exportArms}{redcapDbConnection}(rcon, ...)

\method{exportArms}{redcapApiConnection}(
  rcon,
  arms = NULL,
  ...,
  error_handling = getOption("redcap_error_handling")
)
}
\arguments{
\item{rcon}{A REDCap connection object as generated by \code{redcapConnection}.}

\item{...}{Arguments to be passed to other methods.}

\item{arms}{A numeric vector or arm numbers to retrieve. In REDCap 6.5.0, 
using this argument results in an empty data frame being returned.}

\item{error_handling}{An option for how to handle errors returned by the API.
see \code{\link{redcap_error}}}
}
\value{
Returns a data frame with two columns

\itemize{
  \item{\code{arm_num} }{The arm number}
  \item{\code{name} }{The arm's descriptive name}
}
}
\description{
This function allows you to export the Arms for a project
  Note: this only works for longitudinal projects
}
\details{
It is not sufficient to make the project a longitudinal project. The
project must satisfy one of two conditions: 1) have at least two arms and
one event defined in each arm; or 2) have one arm and at least two events defined. If 
neither of these conditions are satisfied, the API will return a message
such as \code{ERROR: You cannot export arms for classic projects}, an 
error message that isn't as descriptive of the nature of the problem as 
we might like.
}
\section{REDCap API Documentation}{

This function allows you to export the Arms for a project

NOTE: this only works for longitudinal projects.
}

\section{REDCap Version}{

5.8.2+
}

\section{Known REDCap Limitations}{

In versions earlier than 5.9.15, providing a value to the \code{arms} argument
had no effect and the entire data frame of arms is returned.

This was fixed in version 5.9.15.  Sometime before 6.5.0, using the \code{arms}
argument resulted in empty data frames being returned.

In most cases, the number or arms is fairly small, so there is no real performance 
benefit to only selecting a subset of the arms.  The safest course of action is 
to export all of the arms (the default behavior)
}

\references{
Please refer to your institution's API documentation.

Additional details on API parameters are found on the package wiki at
\url{https://github.com/vubiostat/redcapAPI/wiki/REDCap-API-Parameters}
}
\author{
Benjamin Nutter
}
