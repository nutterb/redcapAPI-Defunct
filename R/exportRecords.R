#' @name exportRecords
#' @aliases exportRecords.redcapApiConnection
#' @aliases exportRecords.redcapDbConnection
#' @aliases exportRecords_offline
#' @aliases queryRecords
#' @export exportRecords
#' @export exportRecords_offline
#' @importFrom DBI dbGetQuery
#' @importFrom chron times
#' @importFrom stringr str_split_fixed
#' @importFrom Hmisc label.default
#' @importFrom Hmisc label.data.frame
#' @importFrom Hmisc 'label<-.default'
#' @importFrom Hmisc 'label<-.data.frame'
#' @importFrom Hmisc '[.labelled'
#' @importFrom Hmisc print.labelled
#' @useDynLib redcapAPI, .registration = TRUE
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
#'   returned as numeric codes or labelled factors.
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
#'   This option is only available after REDCap version 6.0.
#' @param proj A \code{redcapProject} object as created by \code{redcapProjectInfo}.
#' @param ... Additional arguments to be passed between methods.
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
#' @examples
#' \dontrun{
#' > #*** Note: I cannot provide working examples
#' > #*** without compromising security.  Instead, I will try to 
#' > #*** offer up sample code with the matching results
#' > 
#' > 
#' > #*** Create the connection object
#' > rcon <- redcapConnection(url=[YOUR_REDCAP_URL], token=[API_TOKEN])
#' > 
#' > 
#' > #*** Export the full data set
#' > BMD <- exportRecords(rcon)
#' > head(BMD)
#' patient_id redcap_event_name      bmi patient_characteristics_complete
#' 1          1       entry_arm_1 38.18765                                2
#' 2          1  dxa_scan_1_arm_1       NA                               NA
#' 3          1  dxa_scan_2_arm_1       NA                               NA
#' 4          1  dxa_scan_3_arm_1       NA                               NA
#' 5          2       entry_arm_1 24.40972                                2
#' 6          2  dxa_scan_1_arm_1       NA                               NA
#' contact_date hip_left_bmd hip_left_tscore hip_right_bmd hip_right_tscore
#' 1         <NA>           NA              NA            NA               NA
#' 2   2013-06-12           NA              NA            NA               NA
#' 3   2009-02-11           NA              NA            NA               NA
#' 4   2011-02-26           NA              NA            NA               NA
#' 5         <NA>           NA              NA            NA               NA
#' 6   2010-11-06        0.697              -2            NA               NA
#' neck_left_bmd neck_left_tscore neck_right_bmd neck_right_tscore spine_bmd
#' 1            NA               NA             NA                NA        NA
#' 2         0.664             -2.0             NA                NA        NA
#' 3         0.675             -1.9             NA                NA        NA
#' 4         0.734             -1.5             NA                NA        NA
#' 5            NA               NA             NA                NA        NA
#' 6         0.521             -3.0             NA                NA     0.899
#' spine_tscore dxa_scan_summary_complete
#' 1           NA                        NA
#' 2           NA                         2
#' 3           NA                         2
#' 4           NA                         2
#' 5           NA                        NA
#' 6         -1.3                         2
#' > 
#' > 
#' > 
#' > #*** Export only the patient_characteristics form
#' > BMD <- exportRecords(rcon, forms="patient_characteristics")
#' > head(BMD)
#' patient_id redcap_event_name      bmi patient_characteristics_complete
#' 1          1       entry_arm_1 38.18765                                2
#' 2          1  dxa_scan_1_arm_1       NA                               NA
#' 3          1  dxa_scan_2_arm_1       NA                               NA
#' 4          1  dxa_scan_3_arm_1       NA                               NA
#' 5          2       entry_arm_1 24.40972                                2
#' 6          2  dxa_scan_1_arm_1       NA                               NA
#' > 
#' > 
#' > #*** Export only the second scan 
#' > BMD <- exportRecords(rcon, events="dxa_scan_2_arm_1", forms="dxa_scan_summary")
#' > head(BMD)
#' patient_id redcap_event_name contact_date hip_left_bmd hip_left_tscore
#' 1          1  dxa_scan_2_arm_1   2009-02-11           NA              NA
#' 2          2  dxa_scan_2_arm_1   2012-10-30        0.684            -2.1
#' 3          3  dxa_scan_2_arm_1   2013-02-06        1.007             0.0
#' 4          4  dxa_scan_2_arm_1   2007-09-20           NA              NA
#' 5          5  dxa_scan_2_arm_1   2006-07-07           NA              NA
#' 6          6  dxa_scan_2_arm_1   2006-10-25           NA              NA
#' hip_right_bmd hip_right_tscore neck_left_bmd neck_left_tscore neck_right_bmd
#' 1            NA               NA         0.675             -1.9             NA
#' 2            NA               NA         0.524             -2.9             NA
#' 3            NA               NA         0.897             -1.0             NA
#' 4            NA               NA         0.632             -2.0             NA
#' 5            NA               NA         0.835             -0.1             NA
#' 6            NA               NA            NA               NA           0.54
#' neck_right_tscore spine_bmd spine_tscore dxa_scan_summary_complete
#' 1                NA        NA           NA                         2
#' 2                NA     0.915         -1.2                         2
#' 3                NA     1.109         -0.6                         2
#' 4                NA     0.864         -1.7                         2
#' 5                NA     0.869         -1.6                         2
#' 6              -2.8     0.830         -2.0                         2
#' > 
#' > 
#' > #*** Retrieve the first scan for patients 38 and 103
#' > BMD <- exportRecords(rcon, records=c(38, 103), 
#'                        forms="dxa_scan_summary", events="dxa_scan_1_arm_1")
#' > BMD
#' patient_id redcap_event_name contact_date hip_left_bmd hip_left_tscore
#' 1         38  dxa_scan_1_arm_1   2008-05-07           NA              NA
#' 2        103  dxa_scan_1_arm_1   2010-04-21        0.856            -1.2
#' hip_right_bmd hip_right_tscore neck_left_bmd neck_left_tscore neck_right_bmd
#' 1            NA               NA         0.595             -2.3             NA
#' 2            NA               NA         0.789             -1.8             NA
#' neck_right_tscore spine_bmd spine_tscore dxa_scan_summary_complete
#' 1                NA     0.770         -2.5                         2
#' 2                NA     1.023         -1.3                         2
#' }




queryRecords <-
function(rcon,fields=NULL,forms=NULL,records=NULL,events=NULL)
{

.SQL_PROJECT_DATA <- "
select
   *
from 
   %s
where
   project_id = %d %s %s %s
   AND field_name NOT LIKE '%s'
order by abs(record), record, event_id
"

   if (!is.null(fields))
   {
      fields <- unique(c('study_id',fields))
      fieldsSQL <- sprintf("AND field_name in (%s)", 
                           paste("'",fields,"'",collapse=",",sep=''))
   }
   else
   {
      fieldsSQL <- ''
   }

   if (!is.null(forms))
   {
      formsSQL <- ''
   }
   else
   {
      formsSQL <- ''
   }

   if (!is.null(records))
   {
      recordsSQL <- ''
   }
   else
   {
      recordsSQL <- ''
   }

   if (!is.null(events))
   {
      eventsSQL <- ''
   }
   else
   {
      eventsSQL <- ''
   }

   sql <- sprintf(.SQL_PROJECT_DATA, getOption('redcap.dataTable'),
                  rcon$project,recordsSQL,eventsSQL,fieldsSQL,'\\_\\_%')
   DBI::dbGetQuery(rcon$conn, sql)
}

#' @rdname exportRecords

exportRecords <-
function(rcon,factors=TRUE,fields=NULL,forms=NULL,records=NULL,events=NULL,labels=TRUE,dates=TRUE,
         survey=TRUE, dag=TRUE, checkboxLabels=FALSE, ...)
   UseMethod("exportRecords")

#' @rdname exportRecords
#' @export
#' 
exportRecords.redcapDbConnection <- 
function(rcon,factors=TRUE,fields=NULL,forms=NULL,records=NULL,events=NULL,labels=TRUE,dates=TRUE,
         survey=TRUE, dag=TRUE, checkboxLabels=FALSE, ...)
{
   meta_data <- exportMetaData(rcon)
   if (!is.null(fields))
   {
      if (is.character(fields) && 
          length(which(meta_data$field_name %in% fields)) == length(fields))
         field_names <- unique(c('study_id',fields))
      else
         stop("Non-existent fields")
   }
   else
      field_names <- meta_data$field_name

   field_lookup <- new.env(hash=TRUE,size=length(field_names))

   raw_data <- queryRecords(rcon,fields,forms,records,events)
   if (nrow(raw_data)>0)
      num_rows <- length(unique(sort(raw_data$record)))
   else
      num_rows <- 0

   x <- data.frame(
      lapply(1:length(field_names),
          function(i) 
          {
            d <- fieldToVar(as.list(meta_data[meta_data$field_name==field_names[i],]), 
                         character(num_rows),factors,dates)
            if (!is.null(attr(d,'redcapLevels')) && factors){
               # REDCap allows any integer as a level for their factors, so
               # we need to transform for R.
               redcapLevels <- attr(d,'redcapLevels')

               # C is zero based, and we normalize redcapLevels to start at index position 2,
               # since element 0 and element 1 are occupied.
               offset <- 2-min(redcapLevels)
               xform <- integer(2+length(redcapLevels))
               xform[1] <-  i
               xform[2] <- offset

               # Add 1 since R has one based indexing.
               xform[1+offset+redcapLevels] <- seq(1,length(redcapLevels))
               xform <- as.integer(xform)
            } else {
               xform <- as.integer(i)
            }
               
            assign(field_names[i],xform,envir=field_lookup)
            d
          }
   ),stringsAsFactors=FALSE)
   names(x) <- field_names

   if (nrow(x)>0){
      suppressWarnings(
      .Call(records_to_dataframe,raw_data$record,raw_data$field_name,
            raw_data$value,field_lookup,x,factors)
      )
   }

   x
}

#' @rdname exportRecords
#' @export

exportRecords.redcapApiConnection <- 
  function(rcon,factors=TRUE,fields=NULL,forms=NULL,records=NULL,events=NULL,labels=TRUE,dates=TRUE,
           survey=TRUE, dag=TRUE, checkboxLabels=FALSE, ..., 
           batch.size=-1,
           proj=NULL)
  {
    #Hlabel <- require(Hmisc)
    #if (!Hlabel) stop("Please install the 'Hmisc' package.")
    
    #* Check that any events listed exist in the events table.
    events_list <- if (is.null(proj$events)) exportEvents(rcon) else proj$events
    if (class(events_list) == "data.frame" & !is.null(events)){
      if (any(!events %in% events_list$unique_event_name)){
        stop(paste0("'", paste(events[!events %in% events_list$unique_event_name], collapse="', '"),
                   " are not valid event names"))
      }
    }
    
    .params <- list(token=rcon$token, content='record',
                    format='csv', type='flat',
                    exportSurveyFields=tolower(survey),
                    exportDataAccessGroups=tolower(dag),
                    returnFormat='csv')
    
    #* for purposes of the export, we don't need the descriptive fields. 
    #* Including them makes the process more error prone, so we'll ignore them.
    meta_data <- if (is.null(proj$meta_data)) exportMetaData(rcon) else proj$meta_data
    meta_data <- subset(meta_data, !meta_data$field_type %in% "descriptive")
    
    #* Check that stated forms exist
    if (any(!forms %in% unique(meta_data$form_name))){
        stop(paste0("'", paste(forms[!forms %in% unique(meta_data$form_name)], collapse="', '"),
                   " are not valid form names"))
      }
    
    #* Create list of field names
    if (!is.null(fields)) #* fields were provided
    {
      # redcap_event_name is automatically included in longitudinal projects
      fields <- fields[!fields %in% "redcap_event_name"] 
      
      # verify all field names exist (could also be 'all(meta_data$field_name %in% fields)'
      if (is.character(fields) && 
            length(which(meta_data$field_name %in% fields)) == length(fields)){
        field_names <- unique(c(fields))
        .params[['fields']] = paste(fields,collapse=',')
      }
      else #* found non-existent fields
        stop(paste("Non-existent fields:", paste(fields[!fields %in% meta_data$field_name], collapse=", "), sep=" "))
    }
    else if (!is.null(forms)){
      field_names <- meta_data$field_name[meta_data$form_name %in% forms]
      
    }
    else
      #* fields were not provided, default to all fields.
      field_names <- meta_data$field_name
   
   #* Expand 'field_names' to include fields from specified forms.    
   if (!is.null(forms)) 
     field_names <- unique(c(field_names, meta_data$field_name[meta_data$form_name %in% forms]))
    
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
    field_labels_suffix <- unlist(sapply(field_names, checklabs))
    
    #* convert field_names to a list for convenience of using sapply to get field_labels
    field_names <- lapply(field_names, identity) 
    field_labels <- sapply(field_names, function(x) meta_data$field_label[meta_data$field_name %in% x])
    
    #* This function grabs the variable codings of the checkbox variables.
    #* These need to be appended to the field names
    #* In this process, a checkbox field name is replicated as many times as it has options
    checkvars <- function(x){
      if (meta_data$field_type[meta_data$field_name %in% x] == "checkbox"){
        opts <- unlist(strsplit(meta_data$select_choices_or_calculations[meta_data$field_name %in% x], "[|]"))
        opts <- tryCatch(as.numeric(unlist(sapply(strsplit(opts, ","), '[', 1))),
                         warning = function(cond){ nm <- as.character(unlist(sapply(strsplit(opts, ","), '[', 1)))
                                                   nm <- gsub('^\\s*','',nm,perl=TRUE)
                                                   nm <- gsub('\\s*$','',nm,perl=TRUE)
                                                   return(nm)})
        x <- paste(x, opts, sep="___")
      }
      return(x)
    }
    field_names <- sapply(field_names, checkvars)
    
    #* Ensures field_labels is adjusted to the proper length to account for
    #* checkbox variables and creates the labels.
    field_labels <- rep(field_labels, sapply(field_names, length))
    field_labels <- paste(field_labels, field_labels_suffix, sep="")
    
    #* return field_names to a vector
    field_names <- unlist(field_names)
    
    if (!is.null(forms)) .params[['forms']] = paste(forms, collapse=",")
    if (!is.null(events)) .params[['events']] = paste(events, collapse=",") # untested...not sure it will work (nutterb)
    if (!is.null(records)) .params[['records']] = paste(records, collapse=",")
    
    #* read in one API call
    if (batch.size < 1){
      x <- apiCall(url=rcon$url, body=.params, config=rcon$config)
      if (x$status_code != "200") stop(as.character(x))
    
      x <- read.csv(textConnection(as.character(x)), stringsAsFactors=FALSE, na.strings="")
    }
    else {
     #* Batch calls. First call requests only the record ids.
     #* if a specific list of records was requested, only those records
     #* are considered forbatching.
      batch.params <- list(token=rcon$token, content='record',
                           format='csv', type='flat',
                           fields=meta_data$field_name[1])
      if (!is.null(records)) batch.params[['records']] = paste(records, collapse=",")
      
      #* Export IDs, then limit to unique IDs (for longitudinal projects). 
      #* There isn't really a way to extract a fixed number of records in each batch
      #* The best we can do is a fixed number of ID's.
      ID <- apiCall(url=rcon$url, body=batch.params, config=rcon$config)
      if (ID$status_code != "200") stop(paste0(ID$status_code, ": ", as.character(ID)))
      ID <- read.csv(textConnection(as.character(ID)), stringsAsFactors=FALSE, na.strings="")
      ID <- unique(ID[, 1, drop=FALSE])
      
      #* Determine the number of batches. Create an index vector of the batch number.
      n.batch <- ceiling(nrow(ID) / batch.size)
      ID$batch.number <- rep(1:n.batch, rep(batch.size, n.batch))[1:nrow(ID)]
      
      #* generate a list of the batched IDs
      batch.records <- lapply(unique(ID$batch.number), function(x) ID[ID$batch.number == x, 1])
      
      #* remove 'records' parameter from the API parameters. This will be replaced by an element of
      #* the list 'batch.records'
      if (!is.null(.params$records)) .params$records <- NULL
      
      #* API calls
      x <- lapply(batch.records, 
                  function(r) apiCall(url=rcon$url,
                                       body=c(.params, list(records=paste(r, collapse=","))),
                                       config=rcon$config))
      if (x[[1]]$status_code != "200") stop(paste(x[[1]]$status_code, ": ", as.character(x[[1]])))
      
      #* Convert results to data.frames, then collapse into a single data.frame
      x <- lapply(x, function(r) read.csv(textConnection(as.character(r)), stringsAsFactors=FALSE, na.strings=""))
      x <- do.call("rbind", x)
    }
    
    #* synchronize underscore codings between records and meta data
    #* Only affects calls in REDCap versions earlier than 5.5.21
    if (compareRedcapVersion(proj$version, "5.5.21") == -1) meta_data <- syncUnderscoreCodings(x, meta_data)

    #* Change field_names to match underscore codings
    if (!is.null(attributes(meta_data)$checkbox_field_name_map)){
      field_names[field_names %in%  attributes(meta_data)$checkbox_field_name_map[, 1]] <- 
            attributes(meta_data)$checkbox_field_name_map[, 2]
    }

    lapply(field_names,
           function(i) 
           {
             x[[i]] <<- fieldToVar(as.list(meta_data[meta_data$field_name==sub("___[a-z,A-Z,0-9,_]+", "", i),]), 
                                   x[[i]],factors,dates, checkboxLabels, vname=i)
           }
    )
    if (labels) Hmisc::label(x[, field_names], self=FALSE) <- field_labels
    x
  }

