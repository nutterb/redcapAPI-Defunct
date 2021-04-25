#' @name exportRecords
#' @title Export Records from a REDCap Database
#' @description Exports records from a REDCap Database, allowing for 
#'   subsets of subjects, fields, records, and events.
#'   
#' @param rcon A \code{redcapApiConnection} object
#' @param factors \code{logical(1)}.  Determines if categorical data from the 
#'   database is 
#'   returned as numeric codes or labelled factors. See 'Checkbox Variables'
#'   for more on how this interacts with the \code{checkboxLabels} argument.
#' @param labels \code{logical(1)}.  Determines if the variable labels are applied to 
#'   the data frame.
#' @param dates \code{logical(1)} Determines if date variables are converted to POSIXct 
#'   format during the download.
#' @param fields \code{character} naming fields to be returned.  If \code{character(0)}, 
#'   all fields are returned.
#' @param forms \code{character} naming forms to be returned.  If \code{character(0)}, 
#'   all forms are returned.
#' @param records \code{character} or \code{numeric}. A vector of study id's to 
#'   be returned.  If \code{character(0)}, all subjects are returned.
#' @param events \code{character} naming events to be returned from a 
#'   longitudinal database.  If \code{character(0)}, all events are returned.
#' @param survey \code{logical(1)} specifies whether or not to export the survey identifier field 
#'   (e.g., "redcap_survey_identifier") or survey timestamp fields 
#'   (e.g., form_name+"_timestamp") when surveys are utilized in the project. 
#'   If you do not pass in this flag, it will default to "false". If set to 
#'   "true", it will return the redcap_survey_identifier field and also the 
#'   survey timestamp field for a particular survey when at least 
#'   one field from that survey is being exported.
#' @param dag \code{logical(1)} specifies whether or not to export the "redcap_data_access_group" 
#'   field when data access groups are utilized in the project. If you do not 
#'   pass in this flag, it will default to "false". NOTE: This flag is only 
#'   viable if the user whose token is being used to make the API request is 
#'   *not* in a data access group. If the user is in a group, then this 
#'   flag will revert to its default value.
#' @param batch.size \code{integerish(1)}.  Specifies the number of subjects to be included 
#'   in each batch of a batched export.  Non-positive numbers export the 
#'   entire project in a single batch. Batching the export may be beneficial 
#'   to prevent tying up smaller servers.  See details for more explanation.
#' @param checkboxLabels \code{logical(1)} Determines the format of labels in checkbox 
#'   variables.  If \code{FALSE} labels are applies as "Unchecked"/"Checked".  
#'   If \code{TRUE}, they are applied as ""/"[field_label]" where [field_label] 
#'   is the label assigned to the level in the data dictionary. 
#'   This option is only available after REDCap version 6.0.  See Checkbox Variables
#'   for more on how this interacts with the \code{factors} argument.
#' @param bundle A \code{redcapBundle} object as created by \code{exportBundle}.
#' @param colClasses A (named) vector of colum classes passed to 
#'   \code{\link[utils]{read.csv}} calls. 
#'   Useful to force the interpretation of a column in a specific type and 
#'   avoid an unexpected recast.
#' @param error_handling An option for how to handle errors returned by the API.
#'   see \code{\link{redcapError}}
#' @param form_complete_auto \code{logical(1)}. When \code{TRUE} 
#'   (default), the \code{[form]_complete} fields for any form 
#'   from which at least one variable is requested will automatically
#'   be retrieved.  When \code{FALSE}, these fields must be 
#'   explicitly requested.
#' @param config \code{list} Additional configuration parameters to pass to 
#'   \code{\link[httr]{POST}}. These will be appended to anything in the
#'   \code{config} attribute of \code{rcon}.
#' @param api_param \code{list} Additional API parameters to pass into the
#'   body of the API call. This provides users to execute calls with options
#'   that may not otherwise be supported by \code{redcapAPI}.
#'   
#' @details
#' A record of exports through the API is recorded in the Logging section 
#' of the project.
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
#' 5.8.2+ (Perhaps earlier) 
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

exportRecords <- function(rcon, 
                          factors = TRUE, 
                          fields = character(0), 
                          forms = character(0), 
                          records = character(0), 
                          events = character(0), 
                          labels = TRUE, 
                          dates = TRUE, 
                          survey = TRUE, 
                          dag = TRUE, 
                          checkboxLabels = FALSE, 
                          colClasses = NA, 
                          batch.size = -1, 
                          bundle = getOption("redcap_bundle"), 
                          error_handling = getOption("redcap_error_handling", "null"), 
                          form_complete_auto = TRUE, 
                          config = list(), 
                          api_param = list()){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_logical(x = factors, 
                            len = 1, 
                            add = coll)
  
  checkmate::assert_character(x = fields, 
                              add = coll)
  
  checkmate::assert_character(x = forms, 
                              add = coll)
  
  if (!checkmate::test_character(x = records) &&
      !checkmate::test_integerish(x = records)){
    coll$push("`records` must be either a character or integerish vector.")
  }
  
  checkmate::assert_character(x = records, 
                              add = coll)
  
  checkmate::assert_character(x = events, 
                              add = coll)
  
  checkmate::assert_logical(x = labels, 
                            len = 1, 
                            add = coll)
  
  checkmate::assert_logical(x = dates, 
                            len = 1, 
                            add = coll)
  
  checkmate::assert_logical(x = survey, 
                            len = 1, 
                            add = coll)
  
  checkmate::assert_logical(x = dag, 
                            len = 1, 
                            add = coll)
  
  checkmate::assert_logical(x = checkboxLabels, 
                            len = 1, 
                            add = coll)
  
  checkmate::assert_character(x = colClasses, 
                              add = coll)
  
  checkmate::assert_integerish(x = batch.size, 
                               len = 1, 
                               add = coll)
  
  error_handling <- checkmate::matchArg(x = error_handling, 
                                        choices = c("null", "error"), 
                                        add = coll)
  
  checkmate::assert_logical(x = form_complete_auto, 
                            len = 1, 
                            add = coll)
  
  checkmate::assert_list(x = config, 
                         add = coll)
  
  checkmate::assert_list(x = api_param, 
                         add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Argument Validation - Round Two ---------------------------------
  
  MetaData <- .exportRecords_getMetaData(bundle, rcon)
  
  .exportRecords_validateFieldNames(fields = fields, 
                                    MetaData = MetaData, 
                                    coll = coll)
  
  .exportRecords_validateFormNames(forms = forms, 
                                   MetaData = MetaData, 
                                   coll = coll)

  .exportRecords_validateEventNames(bundle = bundle, 
                                    rcon = rcon,
                                    events = events, 
                                    MetaData = MetaData, 
                                    coll = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functional Code -------------------------------------------------

  version <- .exportRecords_getVersion(bundle = bundle, 
                                       rcon = rcon)

  field_names <- .exportRecords_finalizeFieldNames(fields = fields, 
                                                   forms = forms, 
                                                   MetaData = MetaData, 
                                                   form_complete_auto = form_complete_auto)

  body <- .exportRecords_makeApiBodyObject(survey = survey, 
                                           dag = dag, 
                                           field_names = field_names, 
                                           forms = forms, 
                                           events = events, 
                                           records = records, 
                                           api_param = api_param)

  RedcapData <- .exportRecords_fetchData(rcon = rcon, 
                                         body = body, 
                                         MetaData = MetaData, 
                                         colClasses = colClasses, 
                                         error_handling = error_handling, 
                                         config = config, 
                                         batch.size = batch.size)
  
  #* synchronize underscore codings between records and meta data
  #* Only affects calls in REDCap versions earlier than 5.5.21
  if (utils::compareVersion(version, "6.0.0") == -1){
    MetaData <- syncUnderscoreCodings(records = RedcapData, 
                                      meta_data = MetaData)
  }
  
  RedcapData <- fieldToVar(records = RedcapData,
                           meta_data = MetaData,
                           factors = factors,
                           dates = dates,
                           checkboxLabels = checkboxLabels)
  
  RedcapData <- .exportRecords_addLabelToData(RedcapData = RedcapData, 
                                              MetaData = MetaData, 
                                              field_names = field_names, 
                                              labels = labels, 
                                              version = version)
  
  RedcapData
  
}

# UNEXPORTED --------------------------------------------------------

.exportRecords_getMetaData <- function(bundle, rcon){
  MetaData <- 
    if (is.null(bundle$meta_data)){
      exportMetaData(rcon)
    } else {
      bundle$meta_data
    }
  
  #* for purposes of the export, we don't need the descriptive fields.
  #* Including them makes the process more error prone, so we'll ignore them.
  MetaData[!MetaData$field_type %in% "descriptive", ]
}

.exportRecords_validateFieldNames <- function(fields, MetaData, coll){
  if (length(fields) > 0){
    # generate the [form]_complete field names for comparison
    form_complete_fields <- sprintf("%s_complete", 
                                    unique(MetaData$form_name))
    
    form_complete_fields <- form_complete_fields[!is.na(form_complete_fields)]
    
    # find the bad fields
    bad_fields <- fields[!fields %in% c(MetaData$field_name, 
                                        form_complete_fields)]
    
    # Throw the error
    if (length(bad_fields)){
      coll$push(sprintf("These requested fields are not valid field names: `%s`", 
                        paste0(bad_fields, 
                               collapse = "`, `")))
    }
  }
}

.exportRecords_validateFormNames <- function(forms, MetaData, coll){
  if (length(forms) > 0){
    bad_forms <- forms[!forms %in% MetaData$form_name]
    if (length(bad_forms))
      coll$push(sprintf("These requested forms are not valid form names: `%s`",
                        paste0(bad_forms, collapse = "`, `")))
  }
}

.exportRecords_validateEventNames <- function(bundle, rcon, events, 
                                              MetaData, coll){
  if (length(events) > 0)
  {
    # Get the EventsList
    EventsList <- NULL
    
    if (length(events) > 0){
      EventsList <- 
        if (is.null(bundle$events)){
          exportEvents(rcon)
        } else {
          bundle$events
        }
    }
    
    if (inherits(EventsList, "data.frame")){
      bad_events <- events[!events %in% EventsList$unique_event_name]
      if (length(bad_events))
        coll$push(paste0("These requested events are not valid event names: ",
                         paste0(bad_events, collapse = ", ")))
    }
  }
}

.exportRecords_getVersion <- function(bundle, rcon){
  if (is.null(bundle$version)){
    exportVersion(rcon)
  }
  else{
    bundle$version
  }
}

.exportRecords_finalizeFieldNames <- function(fields, forms, MetaData, 
                                              form_complete_auto){
  field_names <- 
    if (length(fields) > 0){
      # redcap_event_name is automatically included in longitudinal projects
      fields[!fields %in% "redcap_event_name"]
    } else if (length(forms) > 0){
      MetaData$field_name[MetaData$form_name %in% forms]
    } else {
      #* fields were not provided, default to all fields.
      MetaData$field_name
    }
  
  #* Expand 'field_names' to include fields from specified forms.
  if (length(forms) > 0){
    field_names <-
      unique(c(field_names,
               MetaData$field_name[MetaData$form_name %in% forms]))
  }
  
  # Identify the forms from which the chosen fields are found
  included_form <-
    unique(
      MetaData$form_name[MetaData$field_name %in% field_names]
    )
  
  # Add the form_name_complete column to the export
  if (form_complete_auto){
    field_names <- c(field_names,
                     sprintf("%s_complete", included_form))
  }
  
  field_names
}

.exportRecords_makeApiBodyObject <- function(survey, dag, field_names, forms, 
                                             events, records, api_param){
  body <- list(content = 'record',
               format = 'csv',
               type = 'flat',
               exportSurveyFields = tolower(survey),
               exportDataAccessGroups = tolower(dag),
               returnFormat = 'csv')
  
  if (length(field_names) > 0){
    body <- c(body, 
              vectorToApiBodyList(field_names, "fields"))
  }
  
  if (length(forms) > 0){
    body <- c(body, 
              vectorToApiBodyList(forms, "forms"))
  }
  
  if (length(events) > 0){
    body <- c(body, 
              vectorToApiBodyList(events, "events"))
  }
  
  if (length(records) > 0){
    if (is.numeric(records)){
      records <- as.character(records)
    }
    
    body <- c(body, 
              vectorToApiBodyList(records, "records"))
  }
  
  if (length(api_param) > 0){
    body <- c(body, api_param)
  }
  
  body
}

.exportRecords_fetchData <- function(rcon, body, MetaData, colClasses, 
                                     error_handling, config, batch.size){
  if (batch.size < 1){
    .exportRecords_fetchDataUnbatched(rcon, 
                                      body, 
                                      MetaData, 
                                      colClasses, 
                                      error_handling, 
                                      config)
  } else {
    .exportRecords_fetchDataBatched(rcon, 
                                    body, 
                                    MetaData, 
                                    colClasses, 
                                    error_handling, 
                                    config, 
                                    batch.size)
  }
}

.exportRecords_fetchDataUnbatched <- function(rcon, body, MetaData, colClasses, 
                                              error_handling, config){
  id <- MetaData$field_name[1]
  
  colClasses[[id]] <- "character"
  colClasses <- colClasses[!vapply(colClasses,
                                   is.na,
                                   logical(1))]

  response <- makeApiCall(rcon = rcon, 
                          body = body,
                          config = config)
  
  redcapError(response, error_handling)

  response <- as.character(response)
  
  utils::read.csv(text = response, 
                  stringsAsFactors = FALSE, 
                  na.strings = "",
                  colClasses = colClasses)
}

.exportRecords_fetchDataBatched <- function(rcon, body, MetaData, colClasses, 
                                            error_handling, config, batch.size){
  id <- MetaData$field_name[1]
  
  colClasses[[id]] <- "character"
  colClasses <- colClasses[!vapply(colClasses,
                                   is.na,
                                   logical(1))]
  
  #* 1. Get the IDs column
  #* 2. Restrict to unique IDs
  #* 3. Determine if the IDs look hashed (de-identified)
  #* 4. Give warning about potential problems joining hashed IDs
  #* 5. Read batches
  #* 6. Combine tables
  #* 7. Return full data frame

  body <- body[!grepl("records", names(body))]

  #* 1. Get the IDs column
  id_body <- body
  id_body <- id_body[!grepl("fields", names(id_body))]
  id_body <- c(id_body, 
               vectorToApiBodyList(id, "fields"))

  id_response <- makeApiCall(rcon = rcon, 
                             body = body, 
                             config = config)
  
  redcapError(id_response, error_handling)
  
  IDs <- as.character(id_response)
  IDs <- utils::read.csv(text = IDs,
                         stringsAsFactors = FALSE,
                         na.strings = "",
                         colClasses = colClasses[id])
  
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
  batch.number <- rep(seq_len(ceiling(length(unique_id) / batch.size)),
                      each = batch.size,
                      length.out = length(unique_id))
  
  #* Make a list to hold each of the batched calls
  #* Borrowed from http://stackoverflow.com/a/8099431/1017276
  batch_list <- vector("list", max(batch.number))
  
  #* 5. Read batches
  for (i in unique(batch.number))
  {
    this_body <- c(body, 
                   vectorToApiBodyList(unique_id[batch.number == i], 
                                       "records"))
    
    this_response <- makeApiCall(rcon = rcon, 
                                 body = this_body, 
                                 config = config)

    redcapError(this_response, error_handling = "error")
    
    this_data <- as.character(this_response)
    # probably not necessary for data.  Useful for meta data though. (See Issue #99)
    # x <- iconv(x, "utf8", "ASCII", sub = "")
    batch_list[[i]] <- utils::read.csv(text = this_data,
                                       stringsAsFactors = FALSE,
                                       na.strings = "",
                                       colClasses = colClasses)
    Sys.sleep(1)
  }
  
  #* 6. Combine tables and return
  do.call("rbind", batch_list)
}

.exportRecords_addLabelToData <- function(RedcapData, MetaData, field_names, labels, version){
  if (labels){
    suffixed <-
      checkboxSuffixes(
        # The subset prevents `[form]_complete` fields from
        # being included here.
        fields = field_names[field_names %in% MetaData$field_name],
        meta_data = MetaData,
        version = version)
    
    RedcapData[,suffixed$name_suffix] <-
      mapply(nm = suffixed$name_suffix,
             lab = suffixed$label_suffix,
             FUN = function(nm, lab){
               labelVector::set_label(RedcapData[[nm]], lab)
             },
             SIMPLIFY = FALSE)
  }
  
  RedcapData
}

# ALIASES -----------------------------------------------------------

#' @rdname exportRecords
#' @export

export_records <- exportRecords