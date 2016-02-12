#' @name exportReports
#' @aliases exportReports.redcapApiConnection
#' @aliases exportReports.redcapDbConnection
#' @export exportReports
#' @importFrom httr POST
#' @importFrom chron times
#' @importFrom stringr str_split_fixed
#' @importFrom Hmisc label.default
#' @importFrom Hmisc label.data.frame
#' @importFrom Hmisc 'label<-.default'
#' @importFrom Hmisc 'label<-.data.frame'
#' @importFrom Hmisc '[.labelled'
#' @importFrom Hmisc print.labelled
#' 
#' @title Export Reports from a REDCap Database
#' @description Exports reports from a REDCap Database and formats data if requested
#' 
#' @param rcon A REDCap connection object as created by \code{redcapConnection}.
#' @param report_id Integer.  Gives the report id of the desired report. 
#' This is located on the Report Builder page of the user interface on REDCap.
#' @param factors Logical.  Determines if categorical data from the database 
#' is returned as numeric codes or labelled factors.
#' @param labels Logical.  Determines if the variable labels are applied to the data frame.
#' @param dates Logical. Determines if date variables are converted to POSIXlt format during the download.
#' @param checkboxLabels Logical. Determines the format of labels in checkbox 
#'   variables.  If \code{FALSE} labels are applies as "Unchecked"/"Checked".  
#'   If \code{TRUE}, they are applied as ""/"[field_labe]" where [field_label] 
#'   is the label assigned to the level in the data dictionary. This option 
#'   is only available after REDCap version 6.0.
#' @param proj A \code{redcapProject} object as created by \code{redcapProjectInfo}.
#' @param ... Additional arguments to be passed between methods.
#' 
#' @details
#' A record of exports through the API is recorded in the Logging section of 
#' the project.
#' 
#' Reports are exported based on their id number, which can be looked up in 
#' the Reports page of a project
#' 
#' @section REDCap API Documentation (6.5.0):
#' This function allows you to export the data set of a report created on a project's 
#' "Data Exports, Reports, and Stats" page.
#' 
#' Note about export rights (6.0.0+): Please be aware that Data Export user rights will be 
#' applied to this API request. For example, if you have "No Access" data export rights 
#' in the project, then the API report export will fail and return an error. And if you 
#' have "De-Identified" or "Remove all tagged Identifier fields" data export rights, 
#' then some data fields *might* be removed and filtered out of the data set returned 
#' from the API. To make sure that no data is unnecessarily filtered out of your API 
#' request, you should have "Full Data Set" export rights in the project.
#' 
#' @section REDCap Version:
#' 6.0.0+
#' 
#' @section Known REDCap Limitations:
#' None
#' 
#' @author Benjamin Nutter

exportReports <- function(rcon, report_id, factors=TRUE, labels=TRUE, 
                          dates=TRUE, checkboxLabels=FALSE, ...)
  UseMethod("exportReports")

#' @rdname exportReports
#' @export

exportReports.redcapDbConnection <- function(rcon, report_id, factors=TRUE, labels=TRUE, 
                                             dates=TRUE, checkboxLabels=FALSE, ...){
  message("Please accept my apologies.  The exportMappings method for redcapDbConnection objects\n",
          "has not yet been written.  Please consider using the API.")          
}

#' @rdname exportReports
#' @export

exportReports.redcapApiConnection <- function(rcon, report_id, factors = TRUE, labels = TRUE, 
                                              dates = TRUE, checkboxLabels = FALSE, ...,
                                              proj = getOption("redcap_bundle")){
  
  #* Secure the meta data.
  meta_data <- 
    if (is.null(proj$meta_data)) 
      exportMetaData(rcon) 
  else 
    proj$meta_data
  
  #* for purposes of the export, we don't need the descriptive fields. 
  #* Including them makes the process more error prone, so we'll ignore them.
  meta_data <- meta_data[!meta_data$field_type %in% "descriptive", ]  
  
  #* Secure the REDCap version
  version <- 
    if (is.null(proj$version))
      exportVersion(rcon)
  else
    proj$version
  
  body <- list(token = rcon$token, 
               content = 'report',
               format = 'csv', 
               returnFormat = 'csv',
               report_id = report_id)
  
  x <- httr::POST(url = rcon$url, 
                  body = body, 
                  config = rcon$config)
  
  if (x$status_code != 200) redcap_error(x, error_handling = "error")
  
  x <- utils::read.csv(textConnection(as.character(x)), 
                       stringsAsFactors = FALSE, 
                       na.strings = "")
  
  #* synchronize underscore codings between records and meta data
  #* Only affects calls in REDCap versions earlier than 5.5.21
  if (utils::compareVersion(version, "6.0.0") == -1) 
    meta_data <- syncUnderscoreCodings(x, meta_data)
  

  x <- fieldToVar(records = x, 
                  meta_data = meta_data, 
                  factors = factors, 
                  dates = dates, 
                  checkboxLabels = checkboxLabels)
  
  
  if (labels) 
  {
    field_names <- names(x)
    field_names <- unique(sub("___.+$", "", field_names))
    
    suffixed <- checkbox_suffixes(rcon = rcon, 
                                  fields = field_names,
                                  meta_data = meta_data, 
                                  version = version)
    
    Hmisc::label(x[, suffixed$name_suffix], self=FALSE) <- suffixed$label_suffix
  }
  
  x 
  
}