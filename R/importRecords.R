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

importRecords <- function(rcon, 
                          data,
                          overwriteBehavior = c("normal", "overwrite"),
                          returnContent = c("count", "ids", "nothing"),
                          returnData = FALSE, 
                          logfile = "", 
                          bundle = NULL, 
                          batch.size = -1, 
                          forceAutoNumber = FALSE, 
                          error_handling = getOption("redcap_error_handling", "error"), 
                          config = list(), 
                          api_param = list()){
  
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon,
                          classes = "redcapApiConnection",
                          add = coll)
  
  overwriteBehavior <- 
    checkmate::matchArg(x = overwriteBehavior, 
                        choices = c("normal", "overwrite"), 
                        add = coll)
  
  returnContent <- 
    checkmate::matchArg(x = returnContent, 
                        choices = c("count", "ids", "nothing"), 
                        add = coll)
  
  checkmate::assert_logical(x = returnData, 
                            len = 1, 
                            add = coll)
  
  checkmate::assert_character(x = logfile, 
                              len = 1, 
                              add = coll)
  
  checkmate::assert_integerish(x = batch.size, 
                               len = 1, 
                               add = coll)
  
  checkmate::assert_logical(x = forceAutoNumber, 
                            len = 1, 
                            add = coll)
  
  error_handling <- checkmate::matchArg(x = error_handling, 
                                        choices = c("null", "error"), 
                                        add = coll)
  
  checkmate::assert_list(x = config, 
                         add = coll)
  
  checkmate::assert_list(x = api_param, 
                         add = coll)
  

  checkmate::reportAssertions(coll)

  version <- .exportRecords_getVersion(bundle = bundle, 
                                       rcon = rcon)
  
  MetaData <- .exportRecords_getMetaData(bundle, rcon)
  
  if (utils::compareVersion(version, "5.5.21") == -1 ){
    MetaData <- syncUnderscoreCodings(data, 
                                      MetaData, 
                                      export = FALSE)
  }
  
  MetaData <- 
    MetaData[MetaData$field_name %in% 
                sub(pattern = "___[a-z,A-Z,0-9,_]+", 
                    replacement = "", 
                    x = names(data)), ]
    
  # Field name validation -------------------------------------------
  #** Check that all of the variable names in 'data' exist in REDCap Database
  
  .exportRecords_validateFieldNames(fields = names(data), 
                                    MetaData = MetaData, 
                                    coll = coll)
  
  #** Check that the study id exists in data

  .importRecords_validateIdFieldName(data, MetaData, coll)
  
  #** Confirm that date fields are either character, Date class, or POSIXct
  
  .importRecords_validateDateFields(data, MetaData, coll)
  
  #** Return validations
  
  checkmate::reportAssertions(coll)
  
  
  
  #** Remove survey identifiers and data access group fields from data
  
  data <- .importRecords_RemoveSurveyIdentifiers(data, MetaData)
  
  #** If the study id is not in the the first column, move it and print a warning
 
  data <- .importRecords_makeFieldNameFirst(data, MetaData)

  #*** Remove calculated fields
  
  data <- .importRecords_removeCalculatedFields(data, MetaData)
  
  #** Start the Log
  
  idvars <- 
    if ("redcap_event_name" %in% names(data)) 
      c(meta_data$field_name[1], "redcap_event_name") 
  else 
    meta_data$field_name[1]
  
  msg <- paste0("REDCap Data Import Log: ", Sys.time(),
                "\nThe following (if any) conditions were noted about the data.\n\n")
  
  if (is.null(logfile)){ 
    message(msg) 
  } else { 
    write(msg, logfile)
  }
  
  data <- validateImport(data = data,
                         meta_data = meta_data,
                         logfile = logfile)
  
  if (returnData){
    return(data)
  } 
  
  #** Format the data for REDCap import
  #** Thanks go to:
  #**   https://github.com/etb/my-R-code/blob/master/R-pull-and-push-from-and-to-REDCap.R
  #**   http://stackoverflow.com/questions/12393004/parsing-back-to-messy-api-strcuture/12435389#12435389
  
  
  #** Import Records

}

# UNEXPORTED --------------------------------------------------------

.importRecords_validateIdFieldName <- function(data, MetaData, coll){
  id_field_name <- MetaData$field_name[1]
  
  if (!id_field_name %in% names(data))
  {
    coll$push(paste0("The variable '", 
                     id_field_name, 
                     "' cannot be found in 'data'. ",
                     "Please include this variable and place it in the first column."))
  }  
}

.importRecords_validateDateFields <- function(data, MetaData, coll){
  date_vars <- MetaData$field_name[grepl("date_", MetaData$text_validation_type_or_show_slider_number)]
  
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
}

.importRecords_makeFieldNameFirst <- function(data, MetaData){
  id_field_name <- MetaData$field_name[1]
  
  if (if_field_name %in% names(data) && 
      id_field_name != names(data)[1]){
    message("The variable'", id_field_name, 
            "' was not in the first column. ",
            "It has been moved to the first column.")
    
    w <- which(names(data) == if_field_name)
    
    data <- data[c(w, seq_along(data)[-w])]
  }
  
  data
}

.importRecords_RemoveSurveyIdentifiers <- function(data, MetaData){
  cols_to_remove <- c("redcap_survey_identifier", 
                      paste0(unique(MetaData$form_name), "_timestamp"))
  
  which_to_remove <- which(names(data) %in% cols_to_remove)
  
  if (length(which_to_remove) > 0){
    data <- data[-which_to_remove]
  }
  
  data
}

.importRecords_removeCalculatedFields <- function(data, MetaData){
  calc_field <- meta_data$field_name[MetaData$field_type == "calc"]
  
  if (length(calc_field) > 0)
  {
    message("The variable(s) '", 
            paste(calc_field, collapse="', '"),
            "' are calculated fields and cannot be imported. ",
            "They have been removed from the imported data frame.")
    
    data <- data[!names(data) %in% calc_field]
  }
  
  data
}

# ALIASES

#' @rdname importRecords
#' @export

import_records <- importRecords