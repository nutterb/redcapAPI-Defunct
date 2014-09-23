exportMetaData <- function(rcon, ...) UseMethod("exportMetaData")

exportMetaData.redcapDbConnection <- 
function(rcon, ...)
{
  require(DBI)
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
   x <- DBI::dbGetQuery(rcon$con,
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
function(rcon, ...)
{
   x <- apiCall(url=rcon$url, 
                body=list(token=rcon$token, content='metadata',
                          format='csv', returnFormat='csv'),
                config=rcon$config)          
if (x$status_code == 200){
     x <- read.csv(textConnection(as.character(x)), stringsAsFactors=FALSE, na.strings="")
     return(x)
   }
   else stop(paste(x$status_code, ": ", as.character(x), sep=""))
}
