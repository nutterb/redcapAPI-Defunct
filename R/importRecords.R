importRecords <- function(rcon, data, 
                          overwriteBehavior=c('normal', 'overwrite'),
                          returnContent=c('count', 'ids', 'nothing'),
                          returnData=FALSE, logfile="", ...) UseMethod("importRecords")

importRecords.redcapDbConnection <- function(rcon, data, 
                          overwriteBehavior=c('normal', 'overwrite'),
                          returnContent=c('count', 'ids', 'nothing'),
                          returnData=FALSE, logfile="", ...){
  message("Please accept my apologies.  The importRecords method for redcapDbConnection objects\n",
          "has not yet been written.  Please consider using the API.")
}

importRecords.redcapApiConnection <- function(rcon, data, 
                          overwriteBehavior=c('normal', 'overwrite'),
                          returnContent=c('count', 'ids', 'nothing'),
                          returnData=FALSE, logfile="", ...,
                          meta_data=getOption('redcap_project_info')$meta_data){
  
  warn.flag <- 0
  warn.msg <- NULL
  
  error.flag <- 0
  error.msg <- NULL
  
  overwriteBehavior <- match.arg(overwriteBehavior, c('normal', 'overwrite'))
  returnContent <- match.arg(returnContent, c('count', 'ids', 'nothing'))
  
  if (is.null(meta_data)) meta_data <- exportMetaData(rcon)
  meta_data <- syncUnderscoreCodings(data, meta_data, export=FALSE)
  form_names <- unique(meta_data$form_name)
  names(data)[names(data) %in% attributes(meta_data)$checkbox_field_name_map[, 2]] <- attributes(meta_data)$checkbox_field_name_map[, 1]
  meta_data <- subset(meta_data, meta_data$field_name %in% sub("___[a-z,A-Z,0-9,_]+", "", names(data)))
  
  #** Check that all of the variable names in 'data' exist in REDCap Database
  .checkbox <- subset(meta_data, meta_data$field_type == "checkbox")
  .opts <- sapply(.checkbox$select_choices_or_calculations, function(x) strsplit(x, " [|] "))
  .opts <- lapply(.opts, function(x) gsub(",[[:print:]]+", "", x))
  check_var <- paste(rep(.checkbox$field_name, sapply(.opts, length)), unlist(.opts), sep="___")
  with_complete_fields <- c(unique(meta_data$field_name), paste(form_names, "_complete", sep=""), check_var)
  
  if (!all(names(data) %in% c(with_complete_fields, "redcap_event_name",
                              "redcap_survey_identifier", 
                              paste(unique(meta_data$form_name), "_timestamp", sep=""),
                              "redcap_data_access_group"))){
    error.flag <- error.flag + 1
    error.msg <- c(error.msg, 
                   paste(error.flag, ": The variables ", paste(names(data)[!names(data) %in% with_complete_fields], collapse=", "),
                         " do not exist in the REDCap Data Dictionary", sep=""))
  }
  
  #** Check that the study id exists in data
  if (!meta_data$field_name[1] %in% names(data)){
    error.flag <- error.flag + 1
    error.msg <- c(error.msg,
                   paste(error.flag, ": The variable '", meta_data$field_name[1], "' cannot be found in 'data'. ",
                         "Please include this variable and place it in the first column.", sep=""))
  }
  
  #** If the study id is not in the the first column, move it and print a warning
  if (meta_data$field_name[1] %in% names(data) && meta_data$field_name[1] != names(data)[1]){
    warn.flag <- warn.flag + 1
    warn.msg <- c(warn.msg,
                  paste(warn.flag, ": The variable'", meta_data$field_name[1], "' was not in the first column. ",
                        "It has been moved to the first column."))
    w <- which(names(data) == meta_data$field_name[1])
    data <- data[, c(w, (1:length(data))[-w])]
  }

  #** Confirm that date fields are either character, Date class, or POSIXct
  date_vars <- meta_data$field_name[grepl("date_", meta_data$text_validation_type_or_show_slider_number)]
  bad_date_fmt <- !sapply(data[date_vars], function(x) is.character(x) | "Date" %in% class(x) | "POSIXct" %in% class(x))
  if (any(bad_date_fmt)){
    error.flag <- error.flag + 1
    error.msg <- c(error.msg,
                   paste(error.flag, ": The variables '", paste(date_vars[bad_date_fmt], collapse="', '"), 
                         "' must have class Date, POSIXct, or character.", sep=""))
  }
  
  if (error.flag) stop(paste(error.msg, collapse="\n"))
  
  
  idvars <- if ("redcap_event_name" %in% names(data)) c(meta_data$field_name[1], "redcap_event_name") else meta_data$field_name[1]
  
  msg <- paste("REDCap Data Import Log: ", Sys.time(), 
               "\nThe following (if any) conditions were noted about the data.\n\n", sep="")
  if (is.null(logfile)) cat(msg) else write(msg, logfile)

  data[, names(data)] <- lapply(names(data), validateImport, meta_data, data, idvars, logfile)
  if (returnData) return(data)  
  
  #** Format the data for REDCap import
  #** Thanks go to:
  #**   https://github.com/etb/my-R-code/blob/master/R-pull-and-push-from-and-to-REDCap.R
  #**   http://stackoverflow.com/questions/12393004/parsing-back-to-messy-api-strcuture/12435389#12435389
  data <- lapply(data, function(x){
    if(any(is.na(x))) {x[is.na(x)] <- ""; x} else {x}
  })
  
  l1 <- paste(names(data), collapse=",")
  l2 <- capture.output(write.table(data, sep=",", col.names=FALSE, row.names=FALSE))
  out <- paste0(c(l1, l2, ""), collapse="\n")
  
  ## Reattach attributes
  att <- list("Content-Type" = structure(c("text/html", "utf-8"),
                                         .Names = c("", "charset")))
  attributes(out) <- att
  
  x <- httr::POST(url=rcon$url,
               body=list(token = rcon$token, content='record', format='csv',
                         type='flat', overwriteBehavior = overwriteBehavior,
                         returnFormat='csv', data=out))
  if (x$status_code == "200") as.character(x) else stop(paste(x$status_code, ": ", as.character(x), sep=""))
}
