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

exportRecords <-
function(rcon,factors=TRUE,fields=NULL,forms=NULL,records=NULL,events=NULL,labels=TRUE,dates=TRUE,
         survey=TRUE, dag=TRUE, checkboxLabels=FALSE, ...)
   UseMethod("exportRecords")

exportRecords.redcapDbConnection <- 
function(rcon,factors=TRUE,fields=NULL,forms=NULL,records=NULL,events=NULL,labels=TRUE,dates=TRUE,
         survey=TRUE, dag=TRUE, checkboxLabels=FALSE, ...)
{
  require(DBI)
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
    else #* fields were not provided, default to all fields.
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

