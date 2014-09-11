\name{redcapAPI-package}
\alias{redcapAPI-package}
\docType{package}
\title{
Access data, meta data, and files from REDCap using the API
}
\description{
REDCap is a database development tool built on MySQL.  Visit \url{project-redcap.org} for more information.  REDCap
provides an API through which data, the data dictionary, files, and project information can be accessed.  The 
\code{redcapAPI} package facilitates the use of these functions and simplifies the work needed to prepare data for 
analysis.

As much as possible, I've tried to adequately document \code{redcapAPI}.  Some topics did not seem well-suited to 
documenting in the typical R help files.  Additional tips and discussion are available at the package wiki at
\url{https://github.com/nutterb/redcapAPI/wiki}.  These topics include "Getting started with \code{redcapAPI}", 
"Setting Rights to Grant API Access", "Export data from REDCap" and a detailed description of the REDCap API 
parameters and how they are implemented in \code{R}.  I expect most documentation improvements to be placed on 
the wiki.

Please refer to your institution's REDCap API documentation as a primary resource of what is available.  
Different versions of REDCap support different features--your REDCap API documentation will address the 
features specific to your version of REDCap.

\code{redcapAPI} wouldn't be possible without the efforts of Jeffrey Horner, Will Gray, and Jeremy Stevens at 
Vanderbilt University.  Their work in developing the \code{redcap} package (\url{http://github.com/vubiostat/redcap}) 
was invaluable in helping me understand the API.  A few of their functions (\code{redcapConnection}, 
\code{fieldToVar}, \code{exportMetaData}, and \code{exportRecords}) are included in \code{redcapAPI} largely unaltered.

Many thanks also go to Will Beasley of University of Oklahoma for his development of the \code{REDCapR} package
\url{https://github.com/OuhscBbmc/REDCapR}.  Will introduced me to the \code{httr} package--which improved the use
of messages from the API--and to the idea of batching API calls to reduce the likelihood of servers timing out.  
}

\details{
\tabular{ll}{
Package: \tab redcap\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-09-15\cr
License: \tab gpl\cr
LazyLoad: \tab yes\cr
}

}
\author{
Benjamin Nutter

Maintainer: Benjamin Nutter <nutterb@ccf.org>
}

\references{
  Please refer to your institution's API documentation.

  Additional details on API parameters are found on the package wiki at
  \url{https://github.com/nutterb/redcapAPI/wiki/REDCap-API-Parameters}

  This functionality were originally developed by Jeffrey Horner in the \code{redcap} package.
  \url{https://github.com/vubiostat/redcap}

  See also \code{read_redcap_oneshot} in the \code{REDCapR} package by Will Beasley.
  \url{https://github.com/OuhscBbmc/REDCapR}
  
  To learn more about REDCap, visit \url{project-recap.org}.
}

\examples{
\dontrun{
> #*** Note: I cannot provide working examples
> #*** without compromising security.  Instead, I will try to 
> #*** offer up sample code with the matching results
> 
> 
> #*** Create the connection object
> rcon <- redcapConnection(url=[YOUR_REDCAP_URL], token=[API_TOKEN])
>
> #*** Import REDCap Project Info
> myProject <- redcapProjectInfo(rcon)
> 
> #*** Export the full data set
> BMD <- exportRecords(rcon, proj=myProject)
> head(BMD)
  patient_id redcap_event_name      bmi patient_characteristics_complete
1          1       entry_arm_1 38.18765                                2
2          1  dxa_scan_1_arm_1       NA                               NA
3          1  dxa_scan_2_arm_1       NA                               NA
4          1  dxa_scan_3_arm_1       NA                               NA
5          2       entry_arm_1 24.40972                                2
6          2  dxa_scan_1_arm_1       NA                               NA
  contact_date hip_left_bmd hip_left_tscore hip_right_bmd hip_right_tscore
1         <NA>           NA              NA            NA               NA
2   2013-06-12           NA              NA            NA               NA
3   2009-02-11           NA              NA            NA               NA
4   2011-02-26           NA              NA            NA               NA
5         <NA>           NA              NA            NA               NA
6   2010-11-06        0.697              -2            NA               NA
  neck_left_bmd neck_left_tscore neck_right_bmd neck_right_tscore spine_bmd
1            NA               NA             NA                NA        NA
2         0.664             -2.0             NA                NA        NA
3         0.675             -1.9             NA                NA        NA
4         0.734             -1.5             NA                NA        NA
5            NA               NA             NA                NA        NA
6         0.521             -3.0             NA                NA     0.899
  spine_tscore dxa_scan_summary_complete
1           NA                        NA
2           NA                         2
3           NA                         2
4           NA                         2
5           NA                        NA
6         -1.3                         2
> 
}
}

\keyword{ package }
