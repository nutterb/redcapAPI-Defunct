#' Access data, meta data, and files from REDCap using the API
#'
#' REDCap is a database development tool built on MySQL.  Visit 
#' \url{project-redcap.org} for more information.  REDCap
#' provides an API through which data, the data dictionary, files, and
#'  project information can be accessed.  The 
#' \code{redcapAPI} package facilitates the use of these functions and 
#' simplifies the work needed to prepare data for 
#' analysis.
#' 
#' As much as possible, I've tried to adequately document \code{redcapAPI}.  
#' Some topics did not seem well-suited to documenting in the typical R 
#' help files.  Additional tips and discussion are available at the 
#' package wiki at
#' \url{https://github.com/nutterb/redcapAPI/wiki}.  These topics include 
#' "Getting started with \code{redcapAPI}", 
#' "Setting Rights to Grant API Access", "Export data from REDCap" and a 
#' detailed description of the REDCap API 
#' parameters and how they are implemented in \code{R}.  I expect most 
#' documentation improvements to be placed on 
#' the wiki.
#' 
#' Please refer to your institution's REDCap API documentation as a 
#' primary resource of what is available.  
#' Different versions of REDCap support different features--your REDCap 
#' API documentation will address the 
#' features specific to your version of REDCap.
#' 
#' \code{redcapAPI} wouldn't be possible without the efforts of Jeffrey 
#' Horner, Will Gray, and Jeremy Stevens at 
#' Vanderbilt University.  Their work in developing the \code{redcap} 
#' package (\url{http://github.com/vubiostat/redcap}) 
#' was invaluable in helping me understand the API.  A few of their functions 
#' (\code{redcapConnection}, 
#' \code{fieldToVar}, \code{exportMetaData}, and \code{exportRecords}) are 
#' included in \code{redcapAPI} largely unaltered.
#' 
#' Many thanks also go to Will Beasley of University of Oklahoma for his 
#' development of the \code{REDCapR} package
#' \url{https://github.com/OuhscBbmc/REDCapR}.  Will introduced me to the 
#' \code{httr} package--which improved the use
#' of messages from the API--and to the idea of batching API calls to reduce 
#' the likelihood of servers timing out.   
#' 
#' @name redcapAPI
#' @docType package

NULL
