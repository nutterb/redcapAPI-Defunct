#' @name validateImport
#' @importFrom chron times
#' 
#' @title Validate Data Frames for Import
#' @description Validates the variables in a data frame prior to attempting 
#'   an import to REDCap
#'   
#' @param data Data frame being prepared for import to REDCap.
#' @param meta_data REDCap database meta data.
#' @param logfile A character string giving the filepath to which the 
#'   results of the validation are printed.  If \code{""}, the results 
#'   are printed in the console.
#'   
#' @details
#' \code{validateImport} is called internally by \code{importRecords} and is 
#' not available to the user.
#' 
#' Each variable is validated by matching they type of variable with the type 
#' listed in the REDCap database.  
#' 
#' Although the log messages will indicate a preference for dates to be in 
#' mm/dd/yyyy format, the function will accept mm/dd/yy, yyyy-mm-dd, 
#' yyyy/mm/dd, and yyyymmdd formats as well.  When possible, pass dates as 
#' Date objects or POSIXct objects to avoid confusion.  Dates are also compared 
#' to minimum and maximum values listed in the data dictionary.  Records where 
#' a date is found out of range are allowed to import and a message 
#' is printed in the log.
#' 
#' For continuous/numeric variables, the values are checked against the 
#' minimum and maximum allowed in the data dictionary. Records where a value 
#' is found out of range are allowed to import and a message is printed 
#' in the log.
#' 
#' ZIP codes are tested to see if they fit either the 5 digit or 
#' 5 digit + 4 format.  When these conditions are not met, the data point is 
#' deleted and a message printed in the log.
#' 
#' YesNo fields permit any of the values 'yes', 'no', '0', '1' to be imported 
#' to REDCap with 0=No, and 1=Yes.  The values are converted to lower case 
#' for validation, so any combination of lower and upper case values 
#' will pass (ie, the data frame is not case-sensitive).
#' 
#' TrueFalse fields will accept 'TRUE', 'FALSE', 0, 1, and logical values 
#' and are also not case-sensitive.
#' 
#' Radio and dropdown fields may have either the coding in the data 
#' dictionary or the labels in the data dictionary. The validation will use 
#' the meta data to convert any matching values to the appropriate coding 
#' before importing to REDCap.  Values that cannot be reconciled are 
#' deleted with a message printed in the log.  These variables
#' are not case-sensitive.
#' 
#' Checkbox fields require a value of "Checked", "Unchecked", "0", or "1".  
#' These are currently case sensitive.  Values that do not match these are 
#' deleted with a warning printed in the log.
#' 
#' Phone numbers are required to be 10 digit numbers.  The phone number is 
#' broken into three parts: 1) a 3 digit area code, 2) a 3 digit exchange code, 
#' and 3) a 4 digit station code.  The exchange code must start with a number 
#' from 2-9, followed by 0-8, and then any third digit.  
#' The exchange code starts with a number from 2-9, followed by any two 
#' digits. The station code is 4 digits with no restrictions.
#' 
#' E-mail addresses are considered valid when they have three parts. The first 
#' part comes before the @@ symbol, and may be number of characters from 
#' a-z, A-Z, a period, underscore, percent, plus, or minus.  The second part
#' comes after the @@, but before the period, and may consist of any 
#' number of letters, numbers, periods, or dashes.  Finally, the string ends 
#' with a period then anywhere from 2 to 6 letters.
#' 
#' @author Benjamin Nutter
#' 
#' @references
#' See the REDCap Help and FAQ page's section on 'Text Validation Types'
#' 
#' Validating e-mail addresses
#' \url{http://www.regular-expressions.info/email.html}
#' 

validateImport <- function(data, meta_data, logfile = "")
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_data_frame(x = data,
                               add = coll)
  
  checkmate::assert_data_frame(x = meta_data,
                               add = coll)
  
  checkmate::assert_character(x = logfile,
                              len = 1,
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  for (i in seq_along(data))
  {
    field_name <- names(data)[i]
    field_meta <- sub(pattern = "___[a-z,A-Z,0-9,_]+", 
                      replacement = "", 
                      x = field_name)
    meta_index <- which(meta_data$field_name == field_meta)
    field_type <- meta_data[meta_index, "field_type"]
    
    if (length(field_type)){
      if (field_type == "text"){
        field_type <- meta_data[meta_index, 
                                "text_validation_type_or_show_slider_number"]
      }
    }
    
    field_min <- meta_data[meta_index,
                           "text_validation_min"]
    field_max <- meta_data[meta_index,
                           "text_validation_max"]
    
    field_choice <- meta_data[meta_index,
                              "select_choices_or_calculations"]
    
    if (!length(field_type))
      field_type <- "form_complete"
    if (field_type %in% c("float", "integer", "number", "number_1dp"))
      field_type <- "numeric"
    
    data[[field_name]] <- 
      switch(
        EXPR = field_type,
        "form_complete" = 
          validate_import_form_complete(x = data[[field_name]],
                                        field_name = field_name,
                                        logfile = logfile),
        "date_dmy" = 
          validate_import_date(x = data[[field_name]],
                               field_name = field_name,
                               field_min = field_min,
                               field_max = field_max,
                               logfile = logfile),
        "date_mdy" = 
          validate_import_date(x = data[[field_name]],
                               field_name = field_name,
                               field_min = field_min,
                               field_max = field_max,
                               logfile = logfile),
        "date_ymd" = 
          validate_import_date(x = data[[field_name]],
                               field_name = field_name,
                               field_min = field_min,
                               field_max = field_max,
                               logfile = logfile),
        "datetime_dmy" = 
          validate_import_datetime(x = data[[field_name]],
                                   field_name = field_name,
                                   field_min = field_min,
                                   field_max = field_max,
                                   logfile = logfile),
        "datetime_mdy" = 
          validate_import_datetime(x = data[[field_name]],
                                   field_name = field_name,
                                   field_min = field_min,
                                   field_max = field_max,
                                   logfile = logfile),
        "datetime_ymd" = 
          validate_import_datetime(x = data[[field_name]],
                                   field_name = field_name,
                                   field_min = field_min,
                                   field_max = field_max,
                                   logfile = logfile),
        "datetime_seconds_dmy" = 
          validate_import_datetime_seconds(x = data[[field_name]],
                                           field_name = field_name,
                                           field_min = field_min,
                                           field_max = field_max,
                                           logfile = logfile),
        "datetime_seconds_mdy" = 
          validate_import_datetime_seconds(x = data[[field_name]],
                                           field_name = field_name,
                                           field_min = field_min,
                                           field_max = field_max,
                                           logfile = logfile),
        "datetime_seconds_ymd" = 
          validate_import_datetime_seconds(x = data[[field_name]],
                                           field_name = field_name,
                                           field_min = field_min,
                                           field_max = field_max,
                                           logfile = logfile),
        "time" = 
          validate_import_time(x = data[[field_name]],
                               field_name = field_name,
                               field_min = field_min,
                               field_max = field_max,
                               logfile = logfile),
        "time_mm_ss" = 
          validate_import_time_mm_ss(x = data[[field_name]],
                                     field_name = field_name,
                                     field_min = field_min,
                                     field_max = field_max,
                                     logfile = logfile),
        "numeric" = 
          validate_import_numeric(x = data[[field_name]],
                                  field_name = field_name,
                                  field_min = field_min,
                                  field_max = field_max,
                                  logfile = logfile),
        "zipcode" = 
          validate_import_zipcode(x = data[[field_name]],
                                  field_name = field_name,
                                  logfile = logfile),
        "yesno" = 
          validate_import_yesno(x = data[[field_name]],
                                field_name = field_name,
                                logfile = logfile),
        "truefalse" = 
          validate_import_truefalse(x = data[[field_name]],
                                    field_name = field_name,
                                    logfile = logfile),
        "select" = 
          validate_import_select_dropdown_radio(x = data[[field_name]],
                                                field_name = field_name,
                                                field_choice = field_choice,
                                                logfile = logfile),
        "dropdown" = 
          validate_import_select_dropdown_radio(x = data[[field_name]],
                                                field_name = field_name,
                                                field_choice = field_choice,
                                                logfile = logfile),
        "radio" = 
          validate_import_select_dropdown_radio(x = data[[field_name]],
                                                field_name = field_name,
                                                field_choice = field_choice,
                                                logfile = logfile),
        "checkbox" = 
          validate_import_checkbox(x = data[[field_name]],
                                   field_name = field_name,
                                   field_choice = field_choice,
                                   logfile),
        "email" = 
          validate_import_email(x = data[[field_name]],
                                field_name = field_name,
                                logfile = logfile),
        "phone" = 
          validate_import_phone(x = data[[field_name]],
                                field_name = field_name,
                                logfile = logfile),
        data[[field_name]]
      )
  }
  
  invisible(data)
}
