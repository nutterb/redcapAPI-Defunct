redcapConnection <-
function(url,token,conn,project)
{
   if (missing(url) && missing(conn))
      stop("Need one of url or conn")
   if (!missing(url))
   {
      if (missing(token))
         stop("Need an API token")
      return(
         structure(
            list(url=url,token=token),
            class='redcapApiConnection'
         )
      )
   }
   else
   {
      if (missing(project))
         stop("Need a project_id specified in project variable")
      return(
         structure(
            list(conn=conn,project=project),
            class='redcapDbConnection'
         )
      )
   }
}


exportMetaData <- function(x) UseMethod("exportMetaData")

exportMetaData.redcapDbConnection <- 
function(rcon)
{

.SQL_PROJECT_META_DATA <- "
SELECT 
   field_name, 
   form_name, 
   element_preceding_header as section_header, 
   if(element_type='textarea','notes',if(element_type='select','dropdown',element_type)) as field_type, 
   element_label as field_label, 
   element_enum as select_choices_or_calculations, 
   element_note as field_note,
   if(element_validation_type='int','integer',if(element_validation_type='float','number',element_validation_type)) as text_validation_type_or_show_slider_number, 
   element_validation_min as text_validation_min, 
   element_validation_max as text_validation_max, 
   if(field_phi='1','Y','') as identifier, branching_logic, 
   if(field_req='0','','Y') as required_field, 
   custom_alignment, 
   question_num as question_number
FROM 
   %s
WHERE 
   project_id = %d AND 
   field_name != concat(form_name,'_complete')
ORDER BY field_order"

   suppressWarnings(
   x <- dbGetQuery(rcon$con,
           sprintf(.SQL_PROJECT_META_DATA,
                   getOption('redcap.metaDataTable'),
                   rcon$project))
   )
   suppressWarnings(
   x$required_field <- as.integer(x$required_field)
   )

   # MySQL likes to escape stuff
   x$select_choices_or_calculations <- gsub('\\\\n','\n',x$select_choices_or_calculations)

   x
}

exportMetaData.redcapApiConnection <-
function(rcon)
{
   x <- postForm(
         uri=rcon$url,
         token=rcon$token,
         content='metadata',
         format='csv',.opts=curlOptions(ssl.verifyhost=FALSE))
   x <- read.csv(textConnection(x), stringsAsFactors=FALSE, na.strings="")
   x$required_field <- as.integer(x$required_field)
   x
}


fieldToVar <- 
function(m,d,factors=TRUE)
{

   # Continuous variables
   if ((!is.na(m$text_validation_type_or_show_slider_number) && 
       m$text_validation_type_or_show_slider_number %in% c('float','int') ) || 
       (m$field_type %in% c('calc')) )
   {

      suppressWarnings(d <- as.numeric(d))

   # Ordinal or Categorical variable with a label
   } 
   else if (m$field_type %in% c('select','radio','dropdown'))
   {
      # parses the string "0, Birth \\n 1, Death \\n 2, Unknown" into a
      # character vector for creating a factor
      w <- unlist(strsplit(m$select_choices_or_calculations,"[\n|]"))
      if (length(w)>0) 
      {
         if (length(w) == length(grep(',',w)))
         {
            w <- unlist(lapply(strsplit(w,',',perl=TRUE),
                               function(x)
                                 c(x[1],paste(x[2:length(x)],collapse=','))
                 ))
            w <- gsub('^\\s*','',w,perl=TRUE)
            w <- gsub('\\s*$','',w,perl=TRUE)
            # Create factor
            # Ignore warnings of NAs produced by coercion as this
            # is typically user entry error
            if (factors==TRUE)
            {
               suppressWarnings(
               d <- factor(as.integer(d),
                           levels=as.integer(w[seq(1,length(w),2)]),
                           labels=w[seq(2,length(w),2)])
               )
               attr(d,'redcapLevels') <- as.integer(w[seq(1,length(w),2)])
            }
            else
            {
               suppressWarnings(d <- as.integer(d))
               attr(d,'redcapLabels') <- w[seq(2,length(w),2)]
               attr(d,'redcapLevels') <- as.integer(w[seq(1,length(w),2)])
            }
         } 
         else if (length(w) == length(grep('^[0-9.]+$',w,perl=TRUE))) 
         {
            # Create integer since the meta data about choices are bungled.
            suppressWarnings(d <- as.integer(d))
         } 
      }
   }

   d
}


.queryRecords <-
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
function(rcon,factors=TRUE,fields=NULL,forms=NULL,records=NULL,events=NULL,raw=FALSE)
   UseMethod("exportRecords")

exportRecords.redcapDbConnection <- 
function(rcon,factors=TRUE,fields=NULL,forms=NULL,records=NULL,events=NULL,raw=FALSE)
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

   raw_data <- .queryRecords(rcon,fields,forms,records,events)
   if (nrow(raw_data)>0)
      num_rows <- length(unique(sort(raw_data$record)))
   else
      num_rows <- 0

   x <- data.frame(
      lapply(1:length(field_names),
          function(i) 
          {
            d <- fieldToVar(as.list(meta_data[meta_data$field_name==field_names[i],]), 
                         character(num_rows),factors)
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
function(rcon,factors=TRUE,fields=NULL,forms=NULL,records=NULL,events=NULL,raw=FALSE)
{
   .params <- list(token=rcon$token, content='record',
                   format='csv', type='flat')

   meta_data <- exportMetaData(rcon)

   if (!is.null(fields))
   {
      if (is.character(fields) && 
          length(which(meta_data$field_name %in% fields)) == length(fields))
      {
         field_names <- unique(c('study_id',fields))
         .params[['fields']] = paste(fields,collapse=',')
      }
      else
         stop("Non-existent fields")
   }
   else
      field_names <- meta_data$field_name


   x <- postForm(uri=rcon$url,.params=.params,
                 .opts=curlOptions(ssl.verifyhost=FALSE))
   if (raw) {
      return(x)
   }

   x <- read.csv(textConnection(x), stringsAsFactors=FALSE, na.strings="")

   lapply(field_names,
          function(i) 
          {
            x[[i]] <<- fieldToVar(as.list(meta_data[meta_data$field_name==i,]), 
                            x[[i]],factors)
          }
   )

   x
}

.onLoad <- function(libname,pkgname)
{
   require(RCurl)
   options(redcap.dataTable='redcap_data')
   options(redcap.metaDataTable='redcap_metadata')
}

.onUnload <- function(libPath)
{
   options(redcap.dataTable=NULL)
   options(redcap.metaDataTable=NULL)
}
