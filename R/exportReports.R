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

exportReports.redcapApiConnection <- function(rcon, report_id, factors=TRUE, labels=TRUE, 
              dates=TRUE, checkboxLabels=FALSE, ...,
              proj=NULL){
              
  #Hlabel <- require(Hmisc)
  #if (!Hlabel) stop("Please install the 'Hmisc' package.")
              
  .params <- list(token=rcon$token, content='report',
                    format='csv', returnFormat='csv',
                    report_id=report_id)
  
  #* descriptive fields aren't exported through the API, and 
  #* their meta_data can make other aspects of this function difficult,
  #* so we'll ignore them.
  if (is.null(proj$meta_data)) meta_data <- exportMetaData(rcon)
  meta_data <- subset(meta_data, !meta_data$field_type %in% "descriptive")
  
  x <- httr::POST(url=rcon$url, body=.params, config=rcon$config)
  if (x$status_code != "200") stop(as.character(x))
    
  x <- read.csv(textConnection(as.character(x)), stringsAsFactors=FALSE, na.strings="")
  
  #* synchronize underscore codings between records and meta data
  meta_data <- syncUnderscoreCodings(x, meta_data)
  
  #* Change field_names to match underscore codings
  if (!is.null(attributes(meta_data)$checkbox_field_name_map)){
    names(x)[names(x) %in%  attributes(meta_data)$checkbox_field_name_map[, 1]] <- 
          attributes(meta_data)$checkbox_field_name_map[, 2]
  }
  
  lapply(names(x),
      function(i) 
        {
          x[[i]] <<- fieldToVar(as.list(meta_data[meta_data$field_name==sub("___[a-z,A-Z,0-9,_]+", "", i),]), 
                                x[[i]],factors,dates,checkboxLabels,vname=i)
        }
  )
  if (labels){
    field_names <- gsub("___[a-z,A-Z,0-9,_]+", "", names(x))
    
    #* Extract label suffixes for checkbox fields
    #* This takes the choices of the checkboxes from the meta data and organizes
    #* To be conveniently pasted to 'field_label'
    #* In this process, a checkbox field label is replicated as many times as it has options
    checklabs <- function(x){
      if (meta_data$field_type[meta_data$field_name %in% x] == "checkbox"){
        opts <- unlist(strsplit(meta_data$select_choices_or_calculations[meta_data$field_name %in% x], "[|]"))
        opts <- sub("[[:space:]]+$", "", unlist(sapply(strsplit(opts, ","), '[', 2)))
        opts <- sub("[[:space:]]+", ": ", opts)
        return(opts)
      }
      return("")
    }
    field_labels_suffix <- unlist(sapply(unique(field_names), checklabs))
    
    #* Ensures field_labels is adjusted to the proper length to account for
    #* checkbox variables and creates the labels.
    field_labels <- rep(field_labels, sapply(field_names, length))
    field_labels <- paste0(field_labels, field_labels_suffix)
    
    Hmisc::label(x[, field_names], self=FALSE) <- field_labels
  }
  x
              
}
