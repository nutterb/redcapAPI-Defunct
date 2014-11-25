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
