# validate_import_form_complete -------------------------------------

validate_import_form_complete <- function(x, field_name, logfile)
{
  x <- as.character(x)
  x <- gsub(pattern = "Incomplete", 
            replacement = "0", 
            x = x)
  x <- gsub(pattern = "Unverified", 
            replacement = "1", 
            x = x)
  x <- gsub(pattern = "Complete", 
            replacement = "2", 
            x = x)
  
  w <- which(!grepl("[0-2]", x = x))
  x[w] <- NA
  
  print_validation_message(
    field_name, 
    indices = w,
    message = paste0("Values(s) must be one of: 0, 1, 2, ",
                     "Incomplete, Unverified, or Complete.\n", 
                     "Value not imported"),
    logfile = logfile)
  
  x
}

# validate_import_date ----------------------------------------------

validate_import_date <- function(x, field_name, field_min, field_max, logfile)
{
  if (!inherits(x, "Date") && !inherits(x, "POSIXct"))
  {
    suppressWarnings(
      x <- lubridate::parse_date_time(x = x,
                                      orders = c("ymd", "ymd HMS",
                                                 "mdy", "mdy HMS",
                                                 "dmy", "dmy HMS"))
    )
  }
  
  w_low <- which(x < as.POSIXct(field_min))
  print_validation_message(
    field_name,
    indices = w_low,
    message = paste0("Value(s) are before the stated minimum date: ",
                     format(field_min, format = "%Y-%m-%d")),
    logfile = logfile
  )
  
  w_high <- which(x > as.POSIXct(field_max))
  print_validation_message(
    field_name,
    indices = w_high,
    message = paste0("Values(s) are after the stated maximum date: ",
                     format(field_max, format = "%Y-%m-%d")),
    logfile = logfile
  )
  
  
  x <- format(x, format = "%Y-%m-%d")
  
  w <- which(is.na(x))
  
  print_validation_message(
    field_name, 
    indices = w,
    message = paste0("Value(s) must have POSIXct class, Date class, ",
                     "or character class in ymd, mdy, or dmy format (with optional HMS).\n", 
                     "Value not imported"),
    logfile = logfile)
  
  x
}

# validate_import_datetime ------------------------------------------

validate_import_datetime <- function(x, field_name, field_min, field_max, logfile)
{
  if (!inherits(x, "Date") && !inherits(x, "POSIXct"))
  {
    suppressWarnings(
      x <- lubridate::parse_date_time(x = x,
                                      orders = c("ymd", "ymd HMS",
                                                 "mdy", "mdy HMS",
                                                 "dmy", "dmy HMS"))
    )
  }
  
  w_low <- which(x < as.POSIXct(field_min))
  print_validation_message(
    field_name,
    indices = w_low,
    message = paste0("Value(s) are before the stated minimum date: ",
                     format(field_min, format = "%Y-%m-%d %H:%M")),
    logfile = logfile
  )
  
  w_high <- which(x > as.POSIXct(field_max))
  print_validation_message(
    field_name,
    indices = w_high,
    message = paste0("Values(s) are after the stated maximum date: ",
                     format(field_max, format = "%Y-%m-%d %H:%M")),
    logfile = logfile
  )
  
  x <- format(x, format = "%Y-%m-%d %H:%M")
  
  w <- which(is.na(x))
  
  print_validation_message(
    field_name, 
    indices = w,
    message = paste0("Value(s) must have POSIXct class, Date class, ",
                     "or character class in ymd, mdy, or dmy format (with optional HMS).\n", 
                     "Value not imported"),
    logfile = logfile)
  
  x
}

# validate_import_datetime_seconds ----------------------------------

validate_import_datetime_seconds <- function(x, field_name, field_min, field_max, logfile)
{
  if (!inherits(x, "Date") && !inherits(x, "POSIXct"))
  {
    suppressWarnings(
      x <- lubridate::parse_date_time(x = x,
                                      orders = c("ymd", "ymd HMS",
                                                 "mdy", "mdy HMS",
                                                 "dmy", "dmy HMS"))
    )
  }
  
  w_low <- which(x < as.POSIXct(field_min))
  print_validation_message(
    field_name,
    indices = w_low,
    message = paste0("Value(s) are before the stated minimum date: ",
                     format(field_min, format = "%Y-%m-%d %H:%M:%S")),
    logfile = logfile
  )
  
  w_high <- which(x > as.POSIXct(field_max))
  print_validation_message(
    field_name,
    indices = w_high,
    message = paste0("Values(s) are after the stated maximum date: ",
                     format(field_max, format = "%Y-%m-%d %H:%M:%S")),
    logfile = logfile
  )
  
  x <- format(x, format = "%Y-%m-%d %H:%M:%S")
  
  w <- which(is.na(x))
  
  print_validation_message(
    field_name, 
    indices = w,
    message = paste0("Value(s) must have POSIXct class, Date class, ",
                     "or character class in ymd, mdy, or dmy format (with optional HMS).\n", 
                     "Value not imported"),
    logfile = logfile)
  
  x
}

# validate_import_time ----------------------------------------------

validate_import_time <- function(x, field_name, field_min, field_max, logfile)
{
  x <- as.character(x)
  
  w_invalid <- !grepl("^(\\d{2}:\\d{2}:00|\\d{2}:\\d{2})$", x)
  x[w_invalid] <- NA
  
  count_minute <- function(t)
  {
    if (is.na(t)) return(NA)
    t <- strsplit(t, ":")
    t <- unlist(t)
    t <- as.numeric(t)
    t[1] * 60 + t[2]
  }
  
  total_min <- vapply(x, count_minute, numeric(1))
  
  print_validation_message(
    field_name,
    indices = which(total_min < count_minute(field_min)),
    message = paste0("Value(s) are before the stated minimum time: ",
                     field_min),
    logfile = logfile
  )
  
  print_validation_message(
    field_name,
    indices = which(total_min > count_minute(field_max)),
    message = paste0("Value(s) are after the stated maximum time: ",
                     field_max),
    logfile = logfile
  )
  
  print_validation_message(
    field_name,
    indices = which(w_invalid),
    message = paste0("Value(s) must be of class `time` or in one of hh:mm:ss or hh:mm formats.\n",
                     "Values not imported",
                     field_min),
    logfile = logfile
  )
  
  substr(x, 1, 5)
}

# validate_import_time_mm_ss ----------------------------------------

validate_import_time_mm_ss <- function(x, field_name, field_min, field_max, logfile)
{
  x <- as.character(x)
  
  x[grepl("^\\d{2}:\\d{2}:\\d{2}$", x)] <- 
    sub("^\\d{2}:", "", x[grepl("^\\d{2}:\\d{2}:\\d{2}$", x)])
  
  w_invalid <- !grepl("^\\d{2}:\\d{2}$", x)
  x[w_invalid] <- NA
  
  count_second <- function(t)
  {
    if (is.na(t)) return(NA)
    t <- strsplit(t, ":")
    t <- unlist(t)
    t <- as.numeric(t)
    t[1] * 60 + t[2]
  }
  
  total_sec <- vapply(x, count_second, numeric(1))
  
  print_validation_message(
    field_name,
    indices = which(total_sec < count_second(field_min)),
    message = paste0("Value(s) are before the stated minimum time: ",
                     field_min),
    logfile = logfile
  )
  
  print_validation_message(
    field_name,
    indices = which(total_sec > count_second(field_max)),
    message = paste0("Value(s) are after the stated maximum time: ",
                     field_max),
    logfile = logfile
  )
  
  print_validation_message(
    field_name,
    indices = which(w_invalid),
    message = paste0("Value(s) must be of class `time` or in one of hh:mm:ss or hh:mm formats.\n",
                     "Values not imported"),
    logfile = logfile
  )
  
  x
}

# validate_import_numeric -------------------------------------------

validate_import_numeric <- function(x, field_name, field_min, field_max, logfile)
{
  suppressWarnings(num_check <- as.numeric(x))
  w <- which(is.na(num_check) & !is.na(x))
  
  suppressWarnings({
    if (!is.numeric(x)) x <- as.numeric(x)
    field_min <- as.numeric(field_min)
    field_max <- as.numeric(field_max)
  })
  
  print_validation_message(
    field_name,
    indices = which(x < field_min),
    message = paste0("Value(s) are less than the stated minimum: ",
                     field_min),
    logfile = logfile
  )
  
  print_validation_message(
    field_name,
    indices = which(x > field_max),
    message = paste0("Value(s) are greater than the stated maximum: ",
                     field_max),
    logfile = logfile
  )
  
  print_validation_message(
    field_name,
    indices = w,
    message = paste0("Value(s) must be numeric or coercible to numeric.\n",
                     "Values not imported"),
    logfile = logfile
  )
  
  x
}

# validate_import_zipcode -------------------------------------------

validate_import_zipcode <- function(x, field_name, logfile)
{
  x <- as.character(x)
  w <- which(!grepl("(\\d{5}|\\d{5}-\\d{4})", x) & !is.na(x))
  
  print_validation_message(
    field_name,
    indices = w,
    message = paste0("Value(s) must be in the format `12345` or `12345-1234`.\n",
                     "Values not imported"),
    logfile = logfile
  )
  
  x
}

# validate_import_yesno ---------------------------------------------

validate_import_yesno <- function(x, field_name, logfile)
{
  x <- as.character(x)
  x <- tolower(x)
  w <- which(!x %in% c("no", "yes", "0", "1") & !is.na(x))
  
  x <- gsub("no", "0", x)
  x <- gsub("yes", "1", x)
  
  print_validation_message(
    field_name,
    indices = w,
    message = paste0("Value(s) must be one of `0`, `1`, `No`, or `Yes` (Ignoring case).\n",
                     "Values not imported"),
    logfile = logfile
  )
  
  x
}

# validate_import_truefalse -----------------------------------------

validate_import_truefalse <- function(x, field_name, logfile)
{
  x <- as.character(x)
  x <- tolower(x)
  w <- which(!x %in% c("true", "false", "0", "1", "no", "yes") & !is.na(x))
  
  print_validation_message(
    field_name,
    indices = w,
    message = paste0("Value(s) must be one of logical or one of `0`, `1`, `No`, `Yes`, `False`, or `True` (Ignoring case).\n",
                     "Values not imported"),
    logfile = logfile
  )
  
  x[w] <- NA
  
  x <- gsub("(true|yes)", 1, x)
  x <- gsub("(false|no)", 0, x)
  x
}

# validate_import_select_dropdown_radio -----------------------------

validate_import_select_dropdown_radio <- function(x, field_name, field_choice, logfile)
{
  x <- as.character(x)
  mapping <- strsplit(field_choice, "[|]")
  mapping <- unlist(mapping)
  mapping <- stringr::str_split_fixed(mapping, ", ", 2)
  mapping <- trimws(mapping)
  
  #* Return labeled values to coded values
  for (i in seq_len(nrow(mapping))){
    x[x==mapping[i, 2]] <- mapping[i, 1]  
  }

  w <- which(!x %in% mapping[, 1] & !is.na(x))
  
  print_validation_message(
    field_name,
    indices = w,
    message = paste0("Value(s) must be one of '",
                     paste0(mapping[, 1], collapse = "', '"), "', '",
                     paste0(mapping[, 2], collapse = "', '"),
                     "'.\n",
                     "Values not imported"),
    logfile = logfile
  )
  
  x
}

# validate_import_checkbox ------------------------------------------

validate_import_checkbox <- function(x, field_name, field_choice, logfile)
{
  x <- trimws(tolower(as.character(x)))
  
  #* Select the labeled string from the options as a valid input for the import.
  checkChoice <- trimws(stringr::str_split_fixed(unlist(strsplit(field_choice, "[|]")), ", ", 2))
  checkChoice <- checkChoice[checkChoice[, 1] == unlist(strsplit(field_name, "___"))[2], ]
  
  w <- which(!x %in% c("Checked", "Unchecked", "0", "1", checkChoice, "") & !is.na(x))

  x <- gsub("checked", "1", x)
  x <- gsub("unchecked", "0", x)
  x[x %in% checkChoice] <- 1
  x[x == ""] <- 0
  x[!x %in% c("0", "1")] <- NA
  
  print_validation_message(
    field_name,
    indices = w,
    message = paste0("Value(s) must be one of '0', '1', 'Checked', 'Unchecked', '",
                     checkChoice, "', '' (ignoring case).\n",
                     "Values not imported"),
    logfile = logfile
  )
  
  x
}

# validate_import_email ---------------------------------------------

validate_import_email <- function(x, field_name, logfile)
{
  x <- as.character(x)
  w <- which(!grepl("[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+[.][A-Za-z]{2,6}$", x) & !is.na(x))
  
  print_validation_message(
    field_name = field_name,
    indices = w,
    message = paste0("Value(s) are not valid e-mail addresses.\n",
                     "Values not imported.")
  )
  
  x[w] <- NA
  
  x
}

# validate_import_phone ---------------------------------------------

validate_import_phone <- function(x, field_name, logfile)
{
  x <- as.character(x)
  x <- gsub("[[:punct:][:space:]]", "", x)
  
  w_long <- which(nchar(x) != 10 & !is.na(x))
  
  w_invalid <- which(grepl("^[2-9][0-8][0-9][2-9][0-9]{6}$", x))
  
  print_validation_message(
    field_name = field_name,
    indices = w_long,
    message = paste0("Value(s) are not 10 digit phone numbers.\n",
                     "Values not imported.")
  )
  
  print_validation_message(
    field_name = field_name,
    indices = w_long,
    message = paste0("Value(s) are not valid North American phone numbers.\n",
                     "Values not imported.")
  )
  
  x[w_long | w_invalid] <- NA
  x
}

# print_validation_message ------------------------------------------

print_validation_message <- function(field_name, indices, message, logfile)
{
  if (length(indices))
  {
    message <- 
      paste0("------------------------------------\n",
             "Field Name: `", field_name, "`\n",
             "Indices: ", paste0(indices, collapse = ", "), "\n",
             message, "\n\n")
    
    if (logfile == "")
    {
      message(message)
    }
    else  
    {
      write(message, 
            file = logfile,
            append = TRUE)
    }
  }
}