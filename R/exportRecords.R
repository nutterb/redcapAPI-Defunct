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
   dbGetQuery(rcon$conn, sql)
}

exportRecords <-
function(rcon,factors=TRUE,fields=NULL,forms=NULL,records=NULL,events=NULL,labels=TRUE,dates=TRUE,
         survey=TRUE, dag=TRUE, ...)
   UseMethod("exportRecords")

exportRecords.redcapDbConnection <- 
function(rcon,factors=TRUE,fields=NULL,forms=NULL,records=NULL,events=NULL,labels=TRUE,dates=TRUE,
         survey=TRUE, dag=TRUE, ...)
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
           survey=TRUE, dag=TRUE, batch.size=-1,
           meta_data=getOption('redcap_project_info')$meta_data, 
           events_list=getOption('redcap_project_info')$events, ...)
  {
    Hlabel <- require(Hmisc)
    if (!Hlabel) stop("Please install the 'Hmisc' package.")
    
    if (is.null(events_list)) events_list <- exportEvents(rcon)
    if (class(events_list) == "data.frame"){
      if (any(!events %in% events_list$unique_event_name)){
        stop(paste("'", paste(events[!events %in% events_list$unique_event_name], collapse="', '"),
                   " are not valid event names", sep=""))
      }
    }
    
    .params <- list(token=rcon$token, content='record',
                    format='csv', type='flat',
                    exportSurveyFields=tolower(survey),
                    exportDataAccessGroups=tolower(dag))
    
    if (is.null(meta_data)) meta_data <- exportMetaData(rcon)
    meta_data <- subset(meta_data, !meta_data$field_type %in% "descriptive")
    
    #* Check that stated forms exist
    if (any(!forms %in% unique(meta_data$form_name))){
        stop(paste("'", paste(forms[!forms %in% unique(meta_data$form_name)], collapse="', '"),
                   " are not valid form names"), sep="")
      }
    
    if (!is.null(fields))
    {
      fields <- fields[!fields %in% "redcap_event_name"]
      if (is.character(fields) && 
            length(which(meta_data$field_name %in% fields)) == length(fields))
      {
        field_names <- unique(c(fields))
        .params[['fields']] = paste(fields,collapse=',')
      }
      else
        stop(paste("Non-existent fields:", paste(fields[!fields %in% meta_data$field_name], collapse=", "), sep=" "))
    }
    else
      field_names <- meta_data$field_name
      
   if (!is.null(forms)) 
     field_names <- field_names[field_names %in% c(fields, 
                                                   meta_data$field_name[meta_data$form_name %in% forms])]
    
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
    
    field_names <- lapply(field_names, identity)
    field_labels <- sapply(field_names, function(x) meta_data$field_label[meta_data$field_name %in% x])
    
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
    
    field_labels <- rep(field_labels, sapply(field_names, length))
    field_labels <- paste(field_labels, field_labels_suffix, sep="")
    
    field_names <- unlist(field_names)
    
    if (!is.null(forms)) .params[['forms']] = paste(forms, collapse=",")
    if (!is.null(events)) .params[['events']] = paste(events, collapse=",") # untested...not sure it will work (nutterb)
    if (!is.null(records)) .params[['records']] = paste(records, collapse=",")
    
    if (batch.size < 1){
      x <- postForm(uri=rcon$url,.params=.params,
                    .opts=curlOptions(ssl.verifyhost=FALSE))
    
      x <- read.csv(textConnection(x), stringsAsFactors=FALSE, na.strings="")
    }
    else {
      batch.params <- list(token=rcon$token, content='record',
                           format='csv', type='flat',
                           fields=meta_data$field_name[1])
      if (!is.null(records)) batch.params[['records']] = paste(records, collapse=",")
      ID <- postForm(uri=rcon$url, .params=batch.params,
                     .opts=curlOptions(ssl.verifyhost=FALSE))
      ID <- read.csv(textConnection(ID), stringsAsFactors=FALSE, na.strings="")
      ID <- unique(ID[, 1, drop=FALSE])
      n.batch <- ceiling(nrow(ID) / batch.size)
      ID$batch.number <- rep(1:n.batch, rep(batch.size, n.batch))[1:nrow(ID)]
      batch.records <- lapply(unique(ID$batch.number), function(x) ID[ID$batch.number == x, 1])
      
      if (!is.null(.params$records)) .params$records <- NULL
      x <- lapply(batch.records, 
                  function(r) postForm(uri=rcon$url,
                                       .params=c(.params, list(records=paste(r, collapse=","))),
                                       .opts=curlOptions(ssl.verifyhost=FALSE)))
      x <- lapply(x, function(r) read.csv(textConnection(r), stringsAsFactors=FALSE, na.strings=""))
      x <- do.call("rbind", x)
    }
    
    #* synchronize underscore codings between records and meta data
    meta_data <- syncUnderscoreCodings(x, meta_data)
    
    #* Change field_names to match underscore codings
    if (!is.null(attributes(meta_data)$checkbox_field_name_map)){
      field_names[field_names %in%  attributes(meta_data)$checkbox_field_name_map[, 1]] <- 
            attributes(meta_data)$checkbox_field_name_map[, 2]
    }
    
    lapply(field_names,
           function(i) 
           {
             x[[i]] <<- fieldToVar(as.list(meta_data[meta_data$field_name==sub("___[a-z,A-Z,0-9,_]+", "", i),]), 
                                   x[[i]],factors,dates)
           }
    )
    if (labels) label(x[, field_names], self=FALSE) <- field_labels
    x
  }

