#' @name importRecords
#' @title Import Records to a REDCap Database
#' 
#' @description Imports records from a \code{data.frame} to a REDCap Database
#'
#' @param rcon A REDCap connection object as created by \code{redcapConnection}.
#' @param data A \code{data.frame} to be imported to the REDCap project.
#' @param bundle A \code{redcapBundle} object as created by
#'   \code{exportBundle}.
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
#' @export

importRecords <- function(rcon, data,
                          overwriteBehavior=c('normal', 'overwrite'),
                          returnContent=c('count', 'ids', 'nothing'),
                          returnData=FALSE, logfile="", ...) 
{
  UseMethod("importRecords")
}

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
                                              overwriteBehavior = c('normal', 'overwrite'),
                                              returnContent = c('count', 'ids', 'nothing'),
                                              returnData = FALSE, logfile = "", 
                                              ...,
                                              bundle = NULL, batch.size=-1)
{
  if (!is.na(match("proj", names(list(...)))))
  {
    message("The 'proj' argument is deprecated.  Please use 'bundle' instead")
    bundle <- list(...)[["proj"]]
  }
  
  coll <- checkmate::makeAssertCollection()
  
  massert(~ rcon + bundle + data,
          fun = checkmate::assert_class,
          classes = list(rcon = "redcapApiConnection",
                         bundle = "redcapBundle",
                         data = "data.frame"),
          null.ok = list(bundle = TRUE),
          fixed = list(add = coll))
  
  overwriteBehavior <- 
    checkmate::matchArg(x = overwriteBehavior, 
                        choices = c('normal', 'overwrite'),
                        add = coll)
  
  returnContent <- 
    checkmate::matchArg(x = returnContent, 
                        choices = c('count', 'ids', 'nothing'),
                        add = coll)
  
  checkmate::assert_logical(x = returnData,
                            len = 1,
                            add = coll)
  
  checkmate::assert_character(x = logfile,
                              len = 1,
                              add = TRUE)
  
  checkmate::assert_integerish(x = batch.size,
                               len = 1,
                               add = coll)
  
  checkmate::reportAssertions(coll)
  
  meta_data <- 
    if (is.null(bundle$meta_data)) 
      exportMetaData(rcon) 
    else 
      bundle$meta_data
  
  version <- 
    if (is.null(bundle$version))
      exportVersion(rcon)
    else 
      bundle$version

  if (utils::compareVersion(version, "5.5.21") == -1 )
    meta_data <- syncUnderscoreCodings(data, 
                                       meta_data, 
                                       export = FALSE)
  
  suffixed <- checkbox_suffixes(fields = meta_data$field_name,
                                meta_data = meta_data, 
                                version = version)
  
  form_names <- unique(meta_data$form_name)
  
  meta_data <- 
    meta_data[meta_data$field_name %in% 
                sub(pattern = "___[a-z,A-Z,0-9,_]+", 
                    replacement = "", 
                    x = names(data)), ]
  
  #** Check that all of the variable names in 'data' exist in REDCap Database
  .checkbox <- meta_data[meta_data$field_type == "checkbox", ]
  
  .opts <- lapply(X = .checkbox$select_choices_or_calculations, 
                  FUN = function(x) strsplit(x, 
                                             split = " [|] "))
  .opts <- lapply(X = .opts, 
                  FUN = function(x) gsub(pattern = ",[[:print:]]+", 
                                         replacement = "", 
                                         x = x))
  
  check_var <- paste(rep(.checkbox$field_name, 
                         vapply(.opts, 
                                FUN = length,
                                FUN.VALUE = numeric(1))), 
                     unlist(.opts), 
                     sep="___")
  
  with_complete_fields <- 
    c(unique(meta_data$field_name), 
      paste(form_names, "_complete", sep=""), 
      check_var)
  
  #** Remove survey identifiers and data access group fields from data
  w.remove <- 
    which(names(data) %in% 
            c("redcap_survey_identifier",
              paste0(unique(meta_data$form_name), "_timestamp"),
              "redcap_data_access_group"))
  if (length(w.remove)) data <- data[-w.remove]
  
  if (!all(names(data) %in% c(with_complete_fields, "redcap_event_name")))
  {
    coll$push(paste0("The variables ", 
                     paste(names(data)[!names(data) %in% with_complete_fields], collapse=", "),
                     " do not exist in the REDCap Data Dictionary"))
  }
  
  #** Check that the study id exists in data
  if (!meta_data$field_name[1] %in% names(data))
  {
    coll$push(paste0("The variable '", 
                     meta_data$field_name[1], 
                     "' cannot be found in 'data'. ",
                     "Please include this variable and place it in the first column."))
  }
  
  #** If the study id is not in the the first column, move it and print a warning
  if (meta_data$field_name[1] %in% names(data) && 
      meta_data$field_name[1] != names(data)[1])
  {
    message("The variable'", meta_data$field_name[1], 
            "' was not in the first column. ",
            "It has been moved to the first column.")
    w <- which(names(data) == meta_data$field_name[1])
    data <- data[c(w, (1:length(data))[-w])]
  }
  
  #** Confirm that date fields are either character, Date class, or POSIXct
  date_vars <- meta_data$field_name[grepl("date_", meta_data$text_validation_type_or_show_slider_number)]
  
  bad_date_fmt <- 
    !vapply(X = data[date_vars], 
            FUN = function(x) is.character(x) | "Date" %in% class(x) | "POSIXct" %in% class(x),
            FUN.VALUE = logical(1))
  
  if (any(bad_date_fmt))
  {
    coll$push(paste0("The variables '", 
                     paste(date_vars[bad_date_fmt], collapse="', '"),
                     "' must have class Date, POSIXct, or character."))
  }
  
  #*** Remove calculated fields
  calc_field <- meta_data$field_name[meta_data$field_type == "calc"]
  
  if (length(calc_field) > 0)
  {
    message("The variable(s) '", 
            paste(calc_field, collapse="', '"),
            "' are calculated fields and cannot be imported. ",
            "They have been removed from the imported data frame.")
    data <- data[!names(data) %in% calc_field]
  }
  
  checkmate::reportAssertions(coll)
  
  
  idvars <- 
    if ("redcap_event_name" %in% names(data)) 
      c(meta_data$field_name[1], "redcap_event_name") 
  else 
    meta_data$field_name[1]
  
  msg <- paste0("REDCap Data Import Log: ", Sys.time(),
                "\nThe following (if any) conditions were noted about the data.\n\n")
  
  if (is.null(logfile)) 
    message(msg) 
  else 
    write(msg, logfile)
  
  data <- validateImport(data = data,
                         meta_data = meta_data,
                         logfile = logfile)
  
  if (returnData) return(data)
  
  #** Format the data for REDCap import
  #** Thanks go to:
  #**   https://github.com/etb/my-R-code/blob/master/R-pull-and-push-from-and-to-REDCap.R
  #**   http://stackoverflow.com/questions/12393004/parsing-back-to-messy-api-strcuture/12435389#12435389
  
  if (batch.size > 0)
  {
    import_records_batched(rcon = rcon, 
                           data = data,
                           batch.size = batch.size,
                           overwriteBehavior = overwriteBehavior)
  }
  else
  {
    import_records_unbatched(rcon = rcon,
                             data = data,
                             overwriteBehavior = overwriteBehavior)
  }
}

#####################################################################
## UNEXPORTED FUNCTIONS
#####################################################################

import_records_batched <- function(rcon, data, batch.size, overwriteBehavior)
{
  n.batch <- nrow(data) %/% batch.size + 1
  
  ID <- data.frame(row = 1:nrow(data))
  
  ID$batch.number <- rep(1:n.batch, 
                         each = batch.size, 
                         length.out = nrow(data))
  
  data[is.na(data)] <- ""
  
  data <- split(data, 
                f = ID$batch.number)
  
  out <- lapply(X = data, 
                FUN = data_frame_to_string)
  
  att <- list("Content-Type" = 
                structure(c("text/html", "utf-8"),
                          .Names = c("", "charset")))
  out <- lapply(X = out, 
                FUN = function(d){
                  attributes(d) <- att; 
                  return(d)
                })
  
  x <- vector("list", length = length(out))
  
  for (i in seq_along(out))
  {
    httr::POST(url=rcon$url,
               body=list(token = rcon$token, 
                         content = 'record', 
                         format = 'csv',
                         type = 'flat', 
                         overwriteBehavior = overwriteBehavior,
                         returnFormat = 'csv', 
                         data = out[[i]]),
               config = rcon$config)
  }
  
  if (all(unlist(sapply(X = x, 
                        FUN = function(y) y["status_code"])) == "200"))
  {
    vapply(x, as.character, character(1))
  }
  else 
  {
    status.code <- unlist(sapply(X = x, 
                                 FUN = function(y) y["status_code"]))
    msg <- sapply(x, as.character)
    
    stop(paste(paste0(status.code[status.code != "200"], 
                      ": ", 
                      msg[status.code != "200"]), 
               collapse="\n"))
  }
}


import_records_unbatched <- function(rcon, data, overwriteBehavior)
{
  data[is.na(data)] <- ""

  out <- data_frame_to_string(data)
  
  ## Reattach attributes
  attributes(out) <- 
    list("Content-Type" = structure(c("text/html", "utf-8"),
                                    .Names = c("", "charset")))
  
  x <- httr::POST(url=rcon$url,
                  body=list(token = rcon$token, 
                            content = 'record', 
                            format = 'csv',
                            type = 'flat', 
                            overwriteBehavior = overwriteBehavior,
                            returnFormat = 'csv', 
                            dateFormat = "YMD",
                            data = out))
  
  if (x$status_code == "200") 
    as.character(x) 
  else 
    redcap_error(x, error_handling = "error")
}



data_frame_to_string <- function(data)
{
  paste0(
    utils::capture.output(
      utils::write.table(data, 
                         sep = ",",
                         col.names = TRUE,
                         row.names = FALSE)
    ),
    collapse = "\n"
  )
}