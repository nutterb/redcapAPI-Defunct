#' @name exportRecords
#' 
#' @title Export Records from a REDCap Database
#' @description Exports records from a REDCap Database, allowing for 
#'   subsets of subjects, fields, records, and events.
#'   
#' @param rcon A REDCap connection object as created by \code{redcapConnection}.
#' @param datafile For the offline version, a character string giving the location
#'   of the dataset downloaded from REDCap.  Note that this should be the raw
#'   (unlabeled) data set.
#' @param meta_data A text string giving the location of the data dictionary 
#'   downloaded from REDCap.
#' @param factors Logical.  Determines if categorical data from the database is 
#'   returned as numeric codes or labelled factors. See 'Checkbox Variables'
#'   for more on how this interacts with the \code{checkboxLabels} argument.
#' @param labels Logical.  Determines if the variable labels are applied to 
#'   the data frame.
#' @param dates Logical. Determines if date variables are converted to POSIXct 
#'   format during the download.
#' @param fields A character vector of fields to be returned.  If \code{NULL}, 
#'   all fields are returned.
#' @param forms A character vector of forms to be returned.  If \code{NULL}, 
#'   all forms are returned.
#' @param records A vector of study id's to be returned.  If \code{NULL}, all 
#'   subjects are returned.
#' @param events A character vector of events to be returned from a 
#'   longitudinal database.  If \code{NULL}, all events are returned.
#' @param survey specifies whether or not to export the survey identifier field 
#'   (e.g., "redcap_survey_identifier") or survey timestamp fields 
#'   (e.g., form_name+"_timestamp") when surveys are utilized in the project. 
#'   If you do not pass in this flag, it will default to "false". If set to 
#'   "true", it will return the redcap_survey_identifier field and also the 
#'   survey timestamp field for a particular survey when at least 
#'   one field from that survey is being exported. NOTE: If the survey 
#'   identifier field or survey timestamp fields are imported via API data 
#'   import, they will simply be ignored since they are not real fields in 
#'   the project but rather are pseudo-fields.
#' @param dag specifies whether or not to export the "redcap_data_access_group" 
#'   field when data access groups are utilized in the project. If you do not 
#'   pass in this flag, it will default to "false". NOTE: This flag is only 
#'   viable if the user whose token is being used to make the API request is 
#'   *not* in a data access group. If the user is in a group, then this 
#'   flag will revert to its default value.
#' @param batch.size Integer.  Specifies the number of subjects to be included 
#'   in each batch of a batched export.  Non-positive numbers export the 
#'   entire project in a single batch. Batching the export may be beneficial 
#'   to prevent tying up smaller servers.  See details for more explanation.
#' @param checkboxLabels Logical. Determines the format of labels in checkbox 
#'   variables.  If \code{FALSE} labels are applies as "Unchecked"/"Checked".  
#'   If \code{TRUE}, they are applied as ""/"[field_labe]" where [field_label] 
#'   is the label assigned to the level in the data dictionary. 
#'   This option is only available after REDCap version 6.0.  See Checkbox Variables
#'   for more on how this interacts with the \code{factors} argument.
#' @param bundle A \code{redcapBundle} object as created by \code{exportBundle}.
#' @param colClasses A (named) vector of colum classes passed to 
#'   \code{\link[utils]{read.csv}} calls. 
#'   Useful to force the interpretation of a column in a specific type and 
#'   avoid an unexpected recast.
#' @param ... Additional arguments to be passed between methods.
#' @param error_handling An option for how to handle errors returned by the API.
#'   see \code{\link{redcap_error}}
#' 
#' @details
#' A record of exports through the API is recorded in the Logging section 
#' of the project.
#' 
#' The 'offline' version of the function operates on the raw (unlabeled) data 
#' file downloaded from REDCap along with the data dictionary.  
#' This is made available for instances where the API can not be accessed for 
#' some reason (such as waiting for API approval from the REDCap administrator).
#' 
#' It is unnecessary to include "redcap_event_name" in the fields argument.  
#' This field is automatically exported for any longitudinal database.  
#' If the user does include it in the fields argument, it is removed quietly 
#' in the parameter checks.
#' 
#' A 'batched' export is one where the export is performed over a series of 
#' API calls rather than one large call.  For large projects on small servers, 
#' this may prevent a single user from tying up the server and forcing others 
#' to wait on a larger job.  The batched export is performed by first 
#' calling the API to export the subject identifier field (the first field
#' in the meta data).  The unique ID's are then assigned a batch number with 
#' no more than \code{batch.size} ID's in any single batch.  The batches are 
#' exported from the API and stacked together.
#' 
#' In longitudinal projects, \code{batch.size} may not necessarily be the 
#' number of records exported in each batch.  If \code{batch.size} is 10 and 
#' there are four records per patient, each batch will consist of 40 records.  
#' Thus, if you are concerned about tying up the server with a large, 
#' longitudinal project, it would be prudent to use a smaller batch size.
#' 
#' @section Checkbox Variables:
#' 
#' There are four ways the data from checkbox variables may be 
#' represented depending on the values of \code{factors} and 
#' \code{checkboxLabels}. The most common are the first and third 
#' rows of the table below.  When \code{checkboxLabels = TRUE}, either 
#' the coded value or the labelled value is returned if the box is 
#' checked, or an empty string if it is not.
#' 
#' \tabular{lll}{
#' \code{factors} \tab \code{checkboxLabels} \tab Output \cr
#' \code{FALSE}   \tab \code{FALSE}          \tab 0 / 1 \cr
#' \code{FALSE}   \tab \code{TRUE}           \tab "" / value \cr
#' \code{TRUE}    \tab \code{FALSE}          \tab Unchecked / Checked \cr
#' \code{TRUE}    \tab \code{TRUE}           \tab "" / label 
#' }
#' 
#' @section REDCap API Documentation (6.5.0):
#' This function allows you to export a set of records for a project
#' 
#' Note about export rights (6.0.0+): Please be aware that Data Export user rights will be 
#' applied to this API request. For example, if you have "No Access" data export rights 
#' in the project, then the API data export will fail and return an error. And if you 
#' have "De-Identified" or "Remove all tagged Identifier fields" data export rights, 
#' then some data fields *might* be removed and filtered out of the data set returned 
#' from the API. To make sure that no data is unnecessarily filtered out of your API 
#' request, you should have "Full Data Set" export rights in the project.
#' 
#' @section REDCap Version:
#' 5.8.2 (Perhaps earlier) 
#' 
#' @section Known REDCap Limitations:
#' None
#' 
#' @section Deidentified Batched Calls:
#' Batched calls to the API are not a feature of the REDCap API, but may be imposed 
#' by making multiple calls to the API.  The process of batching the export requires
#' that an initial call be made to the API to retrieve only the record IDs.  The
#' list of IDs is then broken into chunks, each about the size of \code{batch.size}.
#' The batched calls then force the \code{records} argument in each call.
#' 
#' When a user's permissions require a de-identified data export, a batched call 
#' should be expected to fail.  This is because, upon export, REDCap will hash the 
#' identifiers.  When R attempts to pass the hashed identifiers back to REDCap, 
#' REDCap will try to match the hashed identifiers to the unhashed identifiers in the
#' database.  No matches will be found, and the export will fail.
#' 
#' Users who are exporting de-identified data will have to settle for using unbatched
#' calls to the API (ie, \code{batch.size = -1})
#' 
#' @author Jeffrey Horner
#' 
#' @references
#' Please refer to your institution's API documentation.
#' 
#' Additional details on API parameters are found on the package wiki at
#' \url{https://github.com/nutterb/redcapAPI/wiki/REDCap-API-Parameters}
#' 
#' This functionality were originally developed by Jeffrey Horner in the \code{redcap} package.
#' \url{https://github.com/vubiostat/redcap}
#' 
#' See also \code{read_redcap_oneshot} in the \code{REDCapR} package by Will Beasley.
#' \url{https://github.com/OuhscBbmc/REDCapR}
#' 
#' Borrowed code from http://stackoverflow.com/a/8099431/1017276 to 
#' create a list of arbitrary length.
#' 
#' @export

exportRecords <-
  function(rcon, factors = TRUE, fields = NULL, forms = NULL, records = NULL,
           events = NULL, labels = TRUE, dates = TRUE,
           survey = TRUE, dag = TRUE, checkboxLabels = FALSE, 
           colClasses = NA, ...)
    
    UseMethod("exportRecords")

#' @rdname exportRecords
#' @export
#' 
exportRecords.redcapDbConnection <- 
  function(rcon, factors = TRUE, fields = NULL, forms = NULL, records = NULL,
           events = NULL, labels = TRUE, dates = TRUE,
           survey = TRUE, dag = TRUE, checkboxLabels = FALSE, 
           colClasses = NA, ...)
  {
    message("Please accept my apologies.  The exportRecords method for redcapDbConnection objects\n",
            "has not yet been written.  Please consider using the API.")
  }

#' @rdname exportRecords
#' @export

exportRecords.redcapApiConnection <- 
  function(rcon, factors = TRUE, fields = NULL, forms = NULL,
           records = NULL, events = NULL, labels = TRUE, dates = TRUE,
           survey = TRUE, dag = TRUE, checkboxLabels = FALSE, 
           colClasses = NA, ..., 
           batch.size = -1,
           bundle = getOption("redcap_bundle"),
           error_handling = getOption("redcap_error_handling"))
{
  if (!is.na(match("proj", names(list(...)))))
  {
    message("The 'proj' argument is deprecated.  Please use 'bundle' instead")
    bundle <- list(...)[["proj"]]
  }
    
  if (is.numeric(records)) records <- as.character(records)
    
  #* Error Collection Object
  coll <- checkmate::makeAssertCollection()
    
  checkmate::assert_class(x = rcon,
                          classes = "redcapApiConnection",
                          add = coll)
  
  massert(~ factors + labels + dates + survey + dag + checkboxLabels,
          fun = checkmate::assert_logical,
          fixed = list(len = 1,
                       add = coll))
  
  massert(~ fields + forms + records + events,
          fun = checkmate::assert_character,
          fixed = list(null.ok = TRUE,
                       add = coll))
  
  checkmate::assert_integerish(x = batch.size,
                               len = 1,
                               add = coll)
  
  error_handling <- checkmate::matchArg(x = error_handling, 
                                        choices = c("null", "error"),
                                        add = coll)
  
  checkmate::reportAssertions(coll)
  
  #* Secure the meta data.
  meta_data <- 
    if (is.null(bundle$meta_data)) 
      exportMetaData(rcon) 
    else 
      bundle$meta_data
  
  #* for purposes of the export, we don't need the descriptive fields. 
  #* Including them makes the process more error prone, so we'll ignore them.
  meta_data <- meta_data[!meta_data$field_type %in% "descriptive", ]  
  
  #* Secure the events table
  events_list <- 
    if (is.null(bundle$events)) 
      exportEvents(rcon) 
    else 
      bundle$events
  
  #* Secure the REDCap version
  version <- 
    if (is.null(bundle$version))
      exportVersion(rcon)
    else
      bundle$version

  #* Check that all fields exist in the meta data
  if (!is.null(fields)) 
  {
    bad_fields <- fields[!fields %in% meta_data$field_name]
    if (length(bad_fields))
      coll$push(paste0("The following are not valid field names: ",
                       paste0(bad_fields, collapse = ", ")))
  }
  
  #* Check that all form names exist in the meta data
  if (!is.null(forms))
  {
    bad_forms <- forms[!forms %in% meta_data$form_name]
    if (length(bad_forms))
      coll$push(paste0("The following are not valid form names: ",
                       paste0(bad_forms, collapse = ", ")))
  }
  
  #* Check that all event names exist in the events list
  if (!is.null(events) && inherits(events_list, "data.frame"))
  {
    bad_events <- events[!events %in% events_list$unique_event_name]
    if (length(bad_events))
      coll$push(paste0("The following are not valid event names: ",
                       paste0(bad_events, collapse = ", ")))
  }
  
  checkmate::reportAssertions(coll)
  
  #* Create the vector of field names
  if (!is.null(fields)) #* fields were provided
  {
    # redcap_event_name is automatically included in longitudinal projects
    field_names <- fields[!fields %in% "redcap_event_name"] 
  }
  else if (!is.null(forms))
  {
    field_names <- meta_data$field_name[meta_data$form_name %in% forms]
  }
  else
    #* fields were not provided, default to all fields.
    field_names <- meta_data$field_name
  
  #* Expand 'field_names' to include fields from specified forms.    
  if (!is.null(forms)) 
    field_names <- 
    unique(c(field_names, 
             meta_data$field_name[meta_data$form_name %in% forms]))
  
  
  suffixed <- checkbox_suffixes(fields = field_names,
                                meta_data = meta_data, 
                                version = version)
  
  body <- list(token = rcon$token, 
               content = 'record',
               format = 'csv', 
               type = 'flat',
               exportSurveyFields = tolower(survey),
               exportDataAccessGroups = tolower(dag),
               returnFormat = 'csv')
  
  body[['fields']] <- paste0(field_names, collapse=",")
  if (!is.null(forms)) body[['forms']] <- paste0(forms, collapse=",")
  if (!is.null(events)) body[['events']] <- paste0(events, collapse=",") 
  if (!is.null(records)) body[['records']] <- paste0(records, collapse=",")
  
  if (batch.size < 1){
    x <- unbatched(rcon, body, colClasses = colClasses, error_handling)
  }
  else 
  {
    x <- batched(rcon, body, batch.size, meta_data$field_name[1],
                 colClasses = colClasses,
                 error_handling = error_handling)
  }

  #* synchronize underscore codings between records and meta data
  #* Only affects calls in REDCap versions earlier than 5.5.21
  if (utils::compareVersion(version, "6.0.0") == -1) 
    meta_data <- syncUnderscoreCodings(x, meta_data)

  x <- fieldToVar(records = x, 
                  meta_data = meta_data, 
                  factors = factors, 
                  dates = dates, 
                  checkboxLabels = checkboxLabels)
  
  if (labels){
    x[suffixed$name_suffix] <-
      mapply(nm = suffixed$name_suffix,
             lab = suffixed$label_suffix,
             FUN = function(nm, lab){
               labelVector::set_label(x[[nm]], lab)
             },
             SIMPLIFY = FALSE)
  }
  
  x  
}



#*** UNBATCHED EXPORT
unbatched <- function(rcon, body, colClasses, error_handling)
{
  x <- httr::POST(url = rcon$url, 
                  body = body, 
                  config = rcon$config)
  
  if (x$status_code != 200) redcap_error(x, error_handling = error_handling)
  
  x <- as.character(x)
  x <- iconv(x, "utf8", "ASCII", sub = "")
  utils::read.csv(text = x, 
                  stringsAsFactors = FALSE, 
                  na.strings = "",
                  colClasses = colClasses)
}


#*** BATCHED EXPORT
batched <- function(rcon, body, batch.size, id, colClasses, error_handling)
{
  #* 1. Get the IDs column
  #* 2. Restrict to unique IDs
  #* 3. Determine if the IDs look hashed (de-identified)
  #* 4. Give warning about potential problems joining hashed IDs
  #* 5. Read batches
  #* 6. Combine tables
  #* 7. Return full data frame
  
  
  #* 1. Get the IDs column
  id_body <- body
  id_body[['fields']] <- id
  IDs <- httr::POST(url = rcon$url,
                    body = id_body,
                    config = rcon$config)
  
  if (IDs$status_code != 200) redcap_error(IDs, error_handling)
  
  IDs <- as.character(IDs)
  IDs <- iconv(IDs, "utf8", "ASCII", sub = "")
  IDs <- utils::read.csv(text = IDs,
                         stringsAsFactors = FALSE,
                         na.strings = "")
  
  #* 2. Restrict to unique IDs
  unique_id <- unique(IDs[[id]])
  
  #* 3. Determine if the IDs look hashed (de-identified)
  #* 4. Give warning about potential problems joining hashed IDs
  if (all(nchar(unique_id) == 32L))
  {
    warning("The record IDs in this project appear to be de-identified. ",
            "Subject data may not match across batches. ",
            "See 'Deidentified Batched Calls' in '?exportRecords'")
  }
  
  #* Determine batch numbers for the IDs.
  batch.number <- rep(1:ceiling(length(unique_id) / batch.size),
                      each = batch.size,
                      length.out = length(unique_id))
  
  #* Make a list to hold each of the batched calls
  #* Borrowed from http://stackoverflow.com/a/8099431/1017276
  batch_list <- vector("list", max(batch.number))

  #* 5. Read batches
  for (i in unique(batch.number))
  {
    body[['records']] <- paste0(unique_id[batch.number == i], collapse = ",")
    x <- httr::POST(url = rcon$url, 
                    body = body, 
                    config = rcon$config)
    
    if (x$status_code != 200) redcap_error(x, error_handling = "error")
    
    x <- as.character(x)
    x <- iconv(x, "utf8", "ASCII", sub = "")
    batch_list[[i]] <- utils::read.csv(text = x,
                                       stringsAsFactors = FALSE,
                                       na.strings = "",
                                       colClasses = colClasses)
    Sys.sleep(1)
  }
  
  #* 6. Combine tables and return
  do.call("rbind", batch_list)
}
