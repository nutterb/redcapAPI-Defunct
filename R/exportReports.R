exportReports <- function(rcon, report_id, factors=TRUE, labels=TRUE, 
              dates=TRUE, checkboxLabels=FALSE, ...)
    UseMethod("exportReports")
    
exportReports.redcapDbConnection <- function(rcon, report_id, factors=TRUE, labels=TRUE, 
              dates=TRUE, checkboxLabels=FALSE, ...){
    message("Please accept my apologies.  The exportMappings method for redcapDbConnection objects\n",
          "has not yet been written.  Please consider using the API.")          
}

exportReports.redcapApiConnection <- function(rcon, report_id, factors=TRUE, labels=TRUE, 
              dates=TRUE, checkboxLabels=FALSE, ...,
              meta_data=getOption('redcap_project_info')$meta_data){
              
  Hlabel <- require(Hmisc)
  if (!Hlabel) stop("Please install the 'Hmisc' package.")
              
  .params <- list(token=rcon$token, content='report',
                    format='csv', returnFormat='csv')
                    
  if (is.null(meta_data)) meta_data <- exportMetaData(rcon)
  meta_data <- subset(meta_data, !meta_data$field_type %in% "descriptive")
  
  x <- httr::POST(url=rcon$url, body=.params)
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
                                x[[i]],factors,dates,checkboxLabels)
        }
  )
  if (labels) label(x[, field_names], self=FALSE) <- field_labels
  x
              
}
