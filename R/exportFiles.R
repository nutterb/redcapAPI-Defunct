exportFiles <- function(rcon, id, field, event, dir=getwd(), filePrefix=TRUE, records, meta_data...)
  UseMethod("exportFiles")

exportFiles.redcapDbConnection <- function(rcon, id, field, event=NULL, dir=getwd(), filePrefix=TRUE, records, meta_data){
  message("Please accept my apologies.  The exportFiles method for redcapDbConnection objects\n",
          "has not yet been written.  Please consider using the API.")
}

exportFiles.redcapApiConnection <- function(rcon, id, field, event=NULL, dir=getwd(), filePrefix=TRUE, records, meta_data){
  if (missing(meta_data)) meta_data <- exportMetaData(rcon)
  file_fields <- meta_data$field_name[meta_data$field_type == "file"]
  if (any(!field %in% file_fields)){
    stop(paste("The following fields from the project cannot hold files:",
               paste(field[!field %in% file_fields], collapse=", "), sep=""))
  }
  
  if (missing(records)) records <- exportRecords(rcon, fields=field)
  
  longitudinal <- tryCatch(exportEvents(rcon), error = function(cond) FALSE)
  longitudinal <- !is.logical(longitudinal)
  ids <- if (longitudinal) c(meta_data$field_name[1], "redcap_event_name") else meta_data$field_name[1]
  
  download <- melt(records[, c(ids, field)], ids)
  download <- subset(download, !is.na(value))
  
  getRedcapFile <- function(i){
    .params <- list(token=rcon$token, content='file',
                    action='export', record=download[i, 1],
                    field=download$variable[i])
    if ("redcap_event_name" %in% names(download)) .params[['event']] <- download$redcap_event_name[i]
    file <- postForm(uri=rcon$url, .params=.params,
                   .opts=curlOptions(ssl.verifyhost=FALSE))
    filename <- gsub("\"", "", attributes(file)$'Content-Type'['name'])
    if (filePrefix) filename <- paste(download[i, 1], "-", 
                                      if (longitudinal) paste(download$redcap_event_name[i], "-", sep="") else "", 
                                      filename, sep="")
    writeBin(as.vector(file), file.path(dir, filename), useBytes=TRUE)
    return(filename)
  }
  
  download$filename <- sapply(1:nrow(download), getRedcapFile)
  return(download)
}

