\name{redcap-package}
\alias{redcap-package}
\docType{package}
\title{
Access data, meta data, and files from REDCap using the API
}
\description{
REDCap API functions are made available to the user.  Data may be formatted consistent with the data dictionary for 
analysis-ready data.
}
\details{
\tabular{ll}{
Package: \tab redcap\cr
Type: \tab Package\cr
Version: \tab 0.9\cr
Date: \tab 2014-08-01\cr
License: \tab gpl\cr
LazyLoad: \tab yes\cr
}

}
\author{
Jeffery Horner, Jeremy Stevens, Will Gray, Benjamin Nutter

Maintainer: Benjamin Nutter <nutterb@ccf.org>
}

\references{
Additional documentation and resources can be found at \url{https://github.com/nutterb/redcap/wiki}\cr
Please also refer to your institution's API documentation.
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
