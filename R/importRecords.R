#' @name importRecords
#' @aliases importRecords.redcapApiConnection
#' @aliases importRecords.redcapDbConnection
#' @export importRecords
#' @importFrom httr POST
#'
#' @title Import Records to a REDCap Database
#' @description Imports records from a \code{data.frame} to a REDCap Database
#'
#' @param rcon A REDCap connection object as created by \code{redcapConnection}.
#' @param data A \code{data.frame} to be imported to the REDCap project.
#' @param proj A \code{redcapProject} object as created by
#'   \code{redcapProjectInfo}.
#' @param overwriteBehavior Character string.  'normal' prevents blank
#'   fields from overwriting populated fields.  'overwrite' causes blanks to
#'   overwrite data in the REDCap database.
#' @param returnContent Character string.  'count' returns the number of
#'   records imported; 'ids' returns the record ids that are imported;
#'   'nothing' returns no message.
#' @param returnData Logical.  Prevents the REDCap import and instead
#'   returns the data frame that would have been given
#'   for import.  This is sometimes helpful if the API import fails without
#'   providing an informative message. The data frame can be written to a csv
#'   and uploaded using the interactive tools to troubleshoot the
#'   problem.  Please shoot me an e-mail if you find errors I havne't
#'   accounted for.
#' @param logfile An optional filepath (preferably .txt) in which to print the
#'   log of errors and warnings about the data.
#'   If \code{""}, the log is printed to the console.
#' @param batch.size Specifies size of batches.  A negative value
#'   indicates no batching.
#' @param ... Arguments to be passed to other methods.
#'
#' @details
#' A record of imports through the API is recorded in the Logging section
#' of the project.
#'
#' \code{importRecords} prevents the most common import errors by testing the
#' data before attempting the import.  Namely
#' \enumerate{
#'   \item Check that all variables in \code{data} exist in the REDCap data dictionary.
#'   \item Check that the study id variable exists
#'   \item Force the study id variable to the first position in the data frame (with a warning)
#'   \item Remove calculated fields (with a warning)
#'   \item Verify that REDCap date fields are represented in the data frame as
#'     either character, POSIXct, or Date class objects.
#'   \item Determine if values are within their specified validation limits.
#' }
#'
#' See the documentation for \code{\link{validateImport}} for detailed
#' explanations of the validation.
#'
#' @author Benjamin Nutter\cr
#' with thanks to Josh O'Brien and etb (see references)
#'
#' @references
#' \url{http://stackoverflow.com/questions/12393004/parsing-back-to-messy-api-strcuture/12435389#12435389}\cr
#' \url{https://github.com/etb/my-R-code/blob/master/R-pull-and-push-from-and-to-REDCap.R}\cr
#' See also the REDCap API documentation
#' Please refer to your institution's API documentation.
#'
#' Additional details on API parameters are found on the package wiki at
#' \url{https://github.com/nutterb/redcapAPI/wiki/REDCap-API-Parameters}
#'
#' @seealso \code{\link{validateImport}}
#'
#' @examples
#' \dontrun{
#' > #*** Note: I cannot provide working examples without
#' > #*** compromising security.  Instead, I will try to
#' > #*** offer up sample code with the matching results
#' >
#' >
#' > #*** Create the connection object
#' > rcon <- redcapConnection(url=[YOUR_REDCAP_URL], token=[API_TOKEN])
#' >
#' >
#' > #*** Import a record for a new patient
#' > NewScan <- data.frame(patient_id = 1022,
#' +                       redcap_event_name = "entry_arm_1",
#' +                       bmi = 24.689,
#' +                       patient_characteristics_complete = 1)
#' >
#' > importRecords(rcon, NewScan)
#' REDCap Data Import Log: 2014-06-20 16:08:31
#' The following (if any) conditions were noted about the data.
#'
#'
#' 1
#' > ## No conditions were noted, 1 record was uploaded
#' >
#' >
#' >
#' >
#' >
#' > #*** Import a record for a new patient with an erroneous BMI value
#' > NewScan <- data.frame(patient_id = 1022,
#' +                       redcap_event_name = "entry_arm_1",
#' +                       bmi = 244.689,
#' +                       patient_characteristics_complete = 4)
#' >
#' > importRecords(rcon, NewScan)
#' REDCap Data Import Log: 2014-06-20 16:08:33
#' The following (if any) conditions were noted about the data.
#'
#'
#' 1022   entry_arm_1   244.689   Entry for 'bmi' is larger than
#' the acceptable maximum.  Please confirm.
#' 1
#' > ## One condition was noted.  Notice that the BMI value was still
#' > ## uploaded to REDCap.
#' }




importRecords <- function(rcon, data,
                          overwriteBehavior=c('normal', 'overwrite'),
                          returnContent=c('count', 'ids', 'nothing'),
                          returnData=FALSE, logfile="", ...) UseMethod("importRecords")

#' @rdname importRecords
#' @export

importRecords.redcapDbConnection <- function(rcon, data,
                          overwriteBehavior=c('normal', 'overwrite'),
                          returnContent=c('count', 'ids', 'nothing'),
                          returnData=FALSE, logfile="", ...){
  message("Please accept my apologies.  The importRecords method for redcapDbConnection objects\n",
          "has not yet been written.  Please consider using the API.")
}

#' @rdname importRecords
#' @export

importRecords.redcapApiConnection <- function(rcon, data,
                          overwriteBehavior=c('normal', 'overwrite'),
                          returnContent=c('count', 'ids', 'nothing'),
                          returnData=FALSE, logfile="", ...,
                          proj = NULL, batch.size=-1){

  warn.flag <- 0
  warn.msg <- NULL

  error.flag <- 0
  error.msg <- NULL

  overwriteBehavior <- match.arg(overwriteBehavior, c('normal', 'overwrite'))
  returnContent <- match.arg(returnContent, c('count', 'ids', 'nothing'))

  if (is.null(proj$meta_data)) meta_data <- exportMetaData(rcon) else meta_data <- proj$meta_data
  if (compareRedcapVersion(proj$version, "5.5.21") == -1 )
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

  #** Remove survey identifiers and data access group fields from data
  w.remove <- which(names(data) %in% c("redcap_survey_identifier",
                              paste0(unique(meta_data$form_name), "_timestamp"),
                              "redcap_data_access_group"))
  if (length(w.remove) > 0) data <- data[, -w.remove]

  if (!all(names(data) %in% c(with_complete_fields, "redcap_event_name"))){
    error.flag <- error.flag + 1
    error.msg <- c(error.msg,
                   paste0(error.flag, ": The variables ", paste(names(data)[!names(data) %in% with_complete_fields], collapse=", "),
                         " do not exist in the REDCap Data Dictionary"))
  }

  #** Check that the study id exists in data
  if (!meta_data$field_name[1] %in% names(data)){
    error.flag <- error.flag + 1
    error.msg <- c(error.msg,
                   paste0(error.flag, ": The variable '", meta_data$field_name[1], "' cannot be found in 'data'. ",
                         "Please include this variable and place it in the first column."))
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
                   paste0(error.flag, ": The variables '", paste(date_vars[bad_date_fmt], collapse="', '"),
                         "' must have class Date, POSIXct, or character."))
  }

  #*** Remove calculated fields
  calc_field <- meta_data$field_name[meta_data$field_type == "calc"]
  if (length(calc_field) > 0){
    warn.flag <- warn.flag + 1
    warn.msg <- c(warn.msg,
                  paste0(warn.flag, ": The variable(s) '", paste(calc_field, collapse="', '"),
                        "' are calculated fields and cannot be imported. ",
                        "They have been removed from the imported data frame."))
    data <- data[, !names(data) %in% calc_field, drop=FALSE]
  }

  if (warn.flag) warning(paste(warn.msg, collapse="\n"))
  if (error.flag) stop(paste(error.msg, collapse="\n"))


  idvars <- if ("redcap_event_name" %in% names(data)) c(meta_data$field_name[1], "redcap_event_name") else meta_data$field_name[1]

  msg <- paste0("REDCap Data Import Log: ", Sys.time(),
               "\nThe following (if any) conditions were noted about the data.\n\n")
  if (is.null(logfile)) cat(msg) else write(msg, logfile)

  data[, names(data)] <- lapply(names(data), validateImport, meta_data, data, idvars, logfile)
  if (returnData) return(data)

  #** Format the data for REDCap import
  #** Thanks go to:
  #**   https://github.com/etb/my-R-code/blob/master/R-pull-and-push-from-and-to-REDCap.R
  #**   http://stackoverflow.com/questions/12393004/parsing-back-to-messy-api-strcuture/12435389#12435389

  if (batch.size > 0){
    n.batch <- ceiling(nrow(data) / batch.size)
    ID <- data.frame(row = 1:nrow(data))
    ID$batch.number <- rep(1:n.batch, rep(batch.size, n.batch))[1:nrow(ID)]

    data <- lapply(unique(ID$batch.number), function(x) data[ID$row[ID$batch.number == x], ])
    data <- lapply(data, function(d) lapply(d, function(x){
                                                  if(any(is.na(x))) {x[is.na(x)] <- ""; x} else {x}}))
    out <- lapply(data, function(d){ l1 <- paste(names(d), collapse=",")
                                      l2 <- capture.output(write.table(d, sep=",", col.names=FALSE, row.names=FALSE))
                                      out <- paste0(c(l1, l2, ""), collapse="\n")})
    att <- list("Content-Type" = structure(c("text/html", "utf-8"),
                                           .Names = c("", "charset")))
    out <- lapply(out, function(d) {attributes(d) <- att; return(d)})

    x <- lapply(out,
                function(o){
                  httr::POST(url=rcon$url,
                             body=list(token = rcon$token, content='record', format='csv',
                                       type='flat', overwriteBehavior = overwriteBehavior,
                                       returnFormat='csv', data=o),
                             config=rcon$config)})
    if (all(unlist(sapply(x, '[', "status_code")) == "200")) sapply(x, as.character)
    else {
      status.code <- unlist(sapply(x, '[', "status_code"))
      msg <- sapply(x, as.character)

      stop(paste(paste0(status.code[status.code != "200"], ": ", msg[status.code != "200"]), collapse="\n"))
    }

  }
  else{
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
    if (x$status_code == "200") as.character(x) else stop(paste0(x$status_code, ": ", as.character(x)))
  }
}
