context("validateImport_methods.R")

# Tests for validate_import_form_complete ---------------------------

test_that(
  "Acceptable values are properly mapped and returned.", 
  {
    input_value <- c("Incomplete", "Unverified", "Complete", 
                     "0",          "1",          "2", 
                     NA)
    expect_equal(
      validate_import_form_complete(input_value,
                                    field_name = "form_complete", 
                                    logfile = ""), 
      c("0", "1", "2", 
        "0", "1", "2", 
        NA)
    )
  }
)

test_that(
  "Unacceptable values return a message", 
  {
    local_reproducible_output(width = 200)
    expect_message(
      validate_import_form_complete("Invalid", 
                                    field_name = "form_complete", 
                                    logfile = ""), 
      "Values[(]s[)] must be one of: 0, 1, 2, Incomplete, Unverified, or Complete."
    )
  }
)


# validate_import_date ----------------------------------------------

test_that(
  "Date values are converted to YYYY-mm-dd format", 
  {
    date_test <- Sys.Date()
    expect_equal(
      validate_import_date(date_test,  
                           field_name = "date",
                           field_min = NA, 
                           field_max = NA, 
                           logfile = ""),
      format(date_test, "%Y-%m-%d")
    )
  }
)

test_that(
  "POSIXct values are converted to YYYY-mm-dd format", 
  {
    datetime_test <- Sys.time()
    expect_equal(
      validate_import_date(datetime_test,  
                           field_name = "date",
                           field_min = NA, 
                           field_max = NA, 
                           logfile = ""),
      format(datetime_test, "%Y-%m-%d")
    )
  }
)

test_that(
  "ymd, ymd HMS map to YYYY-mm-dd format",
  {
    test_strings <- c("2023-01-01", "2023-01-02 03:04:05")
    
    compare_string <- seq(from = as.Date("2023-01-01"), 
                          to = as.Date("2023-01-02"), 
                          by = "1 day")
    compare_string <- format(compare_string, 
                             format = "%Y-%m-%d")
    expect_equal(
      validate_import_date(test_strings,  
                           field_name = "date",
                           field_min = NA, 
                           field_max = NA, 
                           logfile = ""), 
      compare_string
    )
  }
)

test_that(
  "mdy, mdy HMS YYYY-mm-dd format",
  {
    test_strings <- c("01-01-2023", "01-02-2023 03:04:05")
    
    compare_string <- as.Date(c("2023-01-01", "2023-01-02"))
    compare_string <- format(compare_string, 
                             format = "%Y-%m-%d")
    expect_equal(
      validate_import_date(test_strings,  
                           field_name = "date",
                           field_min = NA, 
                           field_max = NA, 
                           logfile = ""), 
      compare_string
    )
  }
)

test_that(
  "dmy, dmy HMS YYYY-mm-dd format",
  {
    test_strings <- c("13-01-2023", "01-01-2023 03:04:05")
    
    compare_string <- as.Date(c("2023-01-13", "2023-01-01"))
    compare_string <- format(compare_string, 
                             format = "%Y-%m-%d")
    expect_equal(
      validate_import_date(test_strings,  
                           field_name = "date",
                           field_min = NA, 
                           field_max = NA, 
                           logfile = ""), 
      compare_string
    )
  }
)

test_that(
  "NA passes without a message", 
  {
    expect_equal(
      validate_import_date(c("2023-01-01", NA),  
                           field_name = "date",
                           field_min = NA, 
                           field_max = NA, 
                           logfile = ""), 
      c("2023-01-01", NA)
    )
  }
)

test_that(
  "Unmappable values return a message", 
  {
    local_reproducible_output(width = 200)
    expect_message(
      validate_import_date(c("2023-01-33", "not a date"), 
                           field_name = "date",
                           field_min = NA, 
                           field_max = NA, 
                           logfile = ""), 
      "must have POSIXct class, Date class, or character class in ymd, mdy, or dmy format"
    )
  }
)

test_that(
  "When a date is less than field_min, a message is returned", 
  {
    local_reproducible_output(width = 200)
    expect_message(
      validate_import_date(as.Date(c("2023-01-01", "2023-03-01")), 
                           field_name = "date", 
                           field_min = as.Date("2023-02-01"), 
                           field_max = NA, 
                           logfile = ""), 
      "before the stated minimum date"
    )
  }
)

test_that(
  "When a date is greater than field_max, a message is returned", 
  {
    local_reproducible_output(width = 200)
    expect_message(
      validate_import_date(as.Date(c("2023-01-01", "2023-03-01")), 
                           field_name = "date", 
                           field_min = NA, 
                           field_max = as.Date("2023-02-01"), 
                           logfile = ""), 
      "after the stated maximum date"
    )
  }
)

# validate_import_datetime ------------------------------------------
test_that(
  "Date values are converted to YYYY-mm-dd format", 
  {
    date_test <- Sys.Date()
    expect_equal(
      validate_import_datetime(date_test,  
                               field_name = "datetime",
                               field_min = NA, 
                               field_max = NA, 
                               logfile = ""),
      format(date_test, "%Y-%m-%d %H:%M")
    )
  }
)

test_that(
  "POSIXct values are converted to YYYY-mm-dd format", 
  {
    datetime_test <- Sys.time()
    expect_equal(
      validate_import_datetime(datetime_test,  
                               field_name = "datetime",
                               field_min = NA, 
                               field_max = NA, 
                               logfile = ""),
      format(datetime_test, "%Y-%m-%d %H:%M")
    )
  }
)

test_that(
  "ymd, ymd HMS map to YYYY-mm-dd format",
  {
    test_strings <- c("2023-01-01", "2023-01-02 03:04:05")
    
    compare_string <- as.POSIXct(c("2023-01-01 00:00:00", 
                                   "2023-01-02 03:04:05"), 
                                 tz = "UTC")
    compare_string <- format(compare_string, 
                             format = "%Y-%m-%d %H:%M")
    expect_equal(
      validate_import_datetime(test_strings,  
                               field_name = "datetime",
                               field_min = NA, 
                               field_max = NA, 
                               logfile = ""), 
      compare_string
    )
  }
)

test_that(
  "mdy, mdy HMS YYYY-mm-dd format",
  {
    test_strings <- c("01-01-2023", "01-02-2023 03:04:05")
    
    compare_string <- as.POSIXct(c("2023-01-01 00:00:00", "2023-01-02 03:04:05"))
    compare_string <- format(compare_string, 
                             format = "%Y-%m-%d %H:%M")
    expect_equal(
      validate_import_datetime(test_strings,  
                               field_name = "datetime",
                               field_min = NA, 
                               field_max = NA, 
                               logfile = ""), 
      compare_string
    )
  }
)

test_that(
  "dmy, dmy HMS YYYY-mm-dd format",
  {
    test_strings <- c("13-01-2023", "01-01-2023 03:04:05")
    
    compare_string <- as.POSIXct(c("2023-01-13 00:00:00", 
                                   "2023-01-01 03:04:05"), 
                                 tz = "UTC")
    compare_string <- format(compare_string, 
                             format = "%Y-%m-%d %H:%M")
    expect_equal(
      validate_import_datetime(test_strings,  
                               field_name = "datetime",
                               field_min = NA, 
                               field_max = NA, 
                               logfile = ""), 
      compare_string
    )
  }
)

test_that(
  "NA passes without a message", 
  {
    expect_equal(
      validate_import_datetime(c("2023-01-01", NA),  
                               field_name = "datetime",
                               field_min = NA, 
                               field_max = NA, 
                               logfile = ""), 
      c("2023-01-01 00:00", NA)
    )
  }
)

test_that(
  "Unmappable values return a message", 
  {
    local_reproducible_output(width = 200)
    expect_message(
      validate_import_datetime(c("2023-01-33", "not a date"), 
                               field_name = "datetime",
                               field_min = NA, 
                               field_max = NA, 
                               logfile = ""), 
      "must have POSIXct class, Date class, or character class in ymd, mdy, or dmy format"
    )
  }
)

test_that(
  "When a date is less than field_min, a message is returned", 
  {
    local_reproducible_output(width = 200)
    expect_message(
      validate_import_datetime(as.POSIXct(c("2023-01-01", "2023-03-01")), 
                               field_name = "date", 
                               field_min = as.POSIXct("2023-02-01 00:00:00"), 
                               field_max = NA, 
                               logfile = ""), 
      "before the stated minimum date"
    )
  }
)

test_that(
  "When a date is greater than field_max, a message is returned", 
  {
    local_reproducible_output(width = 200)
    expect_message(
      validate_import_datetime(as.Date(c("2023-01-01", "2023-03-01")), 
                               field_name = "date", 
                               field_min = NA, 
                               field_max = as.POSIXct("2023-02-01"), 
                               logfile = ""), 
      "after the stated maximum date"
    )
  }
)

# validate_import_datetime seconds ----------------------------------
test_that(
  "Date values are converted to YYYY-mm-dd format", 
  {
    date_test <- Sys.Date()
    expect_equal(
      validate_import_datetime_seconds(date_test,  
                                       field_name = "datetime",
                                       field_min = NA, 
                                       field_max = NA, 
                                       logfile = ""),
      format(date_test, "%Y-%m-%d %H:%M:%S")
    )
  }
)

test_that(
  "POSIXct values are converted to YYYY-mm-dd format", 
  {
    datetime_test <- Sys.time()
    expect_equal(
      validate_import_datetime_seconds(datetime_test,  
                                       field_name = "datetime",
                                       field_min = NA, 
                                       field_max = NA, 
                                       logfile = ""),
      format(datetime_test, "%Y-%m-%d %H:%M:%S")
    )
  }
)

test_that(
  "ymd, ymd HMS map to YYYY-mm-dd format",
  {
    test_strings <- c("2023-01-01", "2023-01-02 03:04:05")
    
    compare_string <- as.POSIXct(c("2023-01-01 00:00:00", 
                                   "2023-01-02 03:04:05"), 
                                 tz = "UTC")
    compare_string <- format(compare_string, 
                             format = "%Y-%m-%d %H:%M:%S")
    expect_equal(
      validate_import_datetime_seconds(test_strings,  
                                       field_name = "datetime",
                                       field_min = NA, 
                                       field_max = NA, 
                                       logfile = ""), 
      compare_string
    )
  }
)

test_that(
  "mdy, mdy HMS YYYY-mm-dd format",
  {
    test_strings <- c("01-01-2023", "01-02-2023 03:04:05")
    
    compare_string <- as.POSIXct(c("2023-01-01 00:00:00", "2023-01-02 03:04:05"))
    compare_string <- format(compare_string, 
                             format = "%Y-%m-%d %H:%M:%S")
    expect_equal(
      validate_import_datetime_seconds(test_strings,  
                                       field_name = "datetime",
                                       field_min = NA, 
                                       field_max = NA, 
                                       logfile = ""), 
      compare_string
    )
  }
)

test_that(
  "dmy, dmy HMS YYYY-mm-dd format",
  {
    test_strings <- c("13-01-2023", "01-01-2023 03:04:05")
    
    compare_string <- as.POSIXct(c("2023-01-13 00:00:00", 
                                   "2023-01-01 03:04:05"), 
                                 tz = "UTC")
    compare_string <- format(compare_string, 
                             format = "%Y-%m-%d %H:%M:%S")
    expect_equal(
      validate_import_datetime_seconds(test_strings,  
                                       field_name = "datetime",
                                       field_min = NA, 
                                       field_max = NA, 
                                       logfile = ""), 
      compare_string
    )
  }
)

test_that(
  "NA passes without a message", 
  {
    expect_equal(
      validate_import_datetime_seconds(c("2023-01-01", NA),  
                                       field_name = "datetime",
                                       field_min = NA, 
                                       field_max = NA, 
                                       logfile = ""), 
      c("2023-01-01 00:00:00", NA)
    )
  }
)

test_that(
  "Unmappable values return a message", 
  {
    expect_message(
      validate_import_datetime_seconds(c("2023-01-33", "not a date"), 
                                       field_name = "datetime",
                                       field_min = NA, 
                                       field_max = NA, 
                                       logfile = ""), 
      "must have POSIXct class, Date class, or character class in ymd, mdy, or dmy format"
    )
  }
)

test_that(
  "When a date is less than field_min, a message is returned", 
  {
    expect_message(
      validate_import_datetime_seconds(as.POSIXct(c("2023-01-01", "2023-03-01")), 
                                       field_name = "date", 
                                       field_min = as.POSIXct("2023-02-01 00:00:00"), 
                                       field_max = NA, 
                                       logfile = ""), 
      "before the stated minimum date"
    )
  }
)

test_that(
  "When a date is greater than field_max, a message is returned", 
  {
    expect_message(
      validate_import_datetime_seconds(as.Date(c("2023-01-01", "2023-03-01")), 
                                       field_name = "date", 
                                       field_min = NA, 
                                       field_max = as.POSIXct("2023-02-01"), 
                                       logfile = ""), 
      "after the stated maximum date"
    )
  }
)

# validate_import_time ----------------------------------------------

test_that(
  "Character forms of HH:MM and HH:MM:SS pass", 
  {
    time_test <- c("06:15", "06:15:00")
    expect_equal(
      validate_import_time(time_test, 
                           field_name = "time", 
                           field_min = NA, 
                           field_max = NA, 
                           logfile = ""), 
      rep("06:15", 2)
    )
  }
)

test_that(
  "objects of class time pass. Also, NA",
  {
    time_test <- chron::as.times(c("06:15:00", NA))
    expect_equal(
      validate_import_time(time_test, 
                           field_name = "time", 
                           field_min = NA, 
                           field_max = NA, 
                           logfile = ""), 
      c("06:15", NA)
    )
  }
)

test_that(
  "Times before field_min produce a message",
  {
    time_test <- c("06:00", "07:00", "08:00", "09:00")
    expect_message(
      validate_import_time(time_test, 
                           field_name = "time", 
                           field_min = "07:30", 
                           field_max = NA, 
                           logfile = ""), 
      "are before the stated minimum time"
    )
  }
)

test_that(
  "Times after field_max produce a message",
  {
    time_test <- c("06:00", "07:00", "08:00", "09:00")
    expect_message(
      validate_import_time(time_test, 
                           field_name = "time", 
                           field_min = NA, 
                           field_max = "07:30", 
                           logfile = ""), 
      "are after the stated maximum time"
    )
  }
)

# validate_import_time_mm_ss ----------------------------------------

test_that(
  "Character forms of HH:MM and HH:MM:SS pass", 
  {
    time_test <- c("06:15", "00:06:15")
    expect_equal(
      validate_import_time_mm_ss(time_test, 
                           field_name = "time", 
                           field_min = NA, 
                           field_max = NA, 
                           logfile = ""), 
      rep("06:15", 2)
    )
  }
)

test_that(
  "objects of class time pass. Also, NA",
  {
    time_test <- chron::as.times(c("00:06:15", NA))
    expect_equal(
      validate_import_time_mm_ss(time_test,
                                 field_name = "time",
                                 field_min = NA,
                                 field_max = NA,
                                 logfile = ""),
      c("06:15", NA)
    )
  }
)

test_that(
  "Times before field_min produce a message",
  {
    local_reproducible_output(width = 200)
    time_test <- c("06:00", "07:00", "08:00", "09:00")
    expect_message(
      validate_import_time_mm_ss(time_test,
                                 field_name = "time",
                                 field_min = "07:30",
                                 field_max = NA,
                                 logfile = ""),
      "are before the stated minimum time"
    )
  }
)

test_that(
  "Times after field_max produce a message",
  {
    local_reproducible_output(width = 200)
    time_test <- c("06:00", "07:00", "08:00", "09:00")
    expect_message(
      validate_import_time_mm_ss(time_test,
                                 field_name = "time",
                                 field_min = NA,
                                 field_max = "07:30",
                                 logfile = ""),
      "are after the stated maximum time"
    )
  }
)
# validate_import_numeric -------------------------------------------

test_that(
  "Values that can be coerced to numeric pass (including NA)", 
  {
    test_numeric <- c("1.2", pi, NA_character_)
    expect_equal(
      validate_import_numeric(test_numeric, 
                              field_name = "numeric", 
                              field_min = NA, 
                              field_max = NA, 
                              logfile = ""), 
      c(1.2, pi, NA_real_)
    )
  }
)

test_that(
  "Values that cannot be coerced to numeric produce a message", 
  {
    local_reproducible_output(width = 200)
    test_numeric <- c("a", "b", pi)
    expect_message(
      validate_import_numeric(test_numeric, 
                              field_name = "numeric", 
                              field_min = NA, 
                              field_max = NA, 
                              logfile = ""), 
      "must be numeric or coercible to numeric"
    )
  }
)

test_that(
  "Values less than field_min produce a message", 
  {
    local_reproducible_output(width = 200)
    test_numeric <- 1:5
    expect_message(
      validate_import_numeric(test_numeric, 
                              field_name = "numeric", 
                              field_min = 3, 
                              field_max = NA, 
                              logfile = ""),
      "are less than the stated minimum"
    )
  }
)

test_that(
  "Values less than field_min produce a message", 
  {
    local_reproducible_output(width = 200)
    test_numeric <- 1:5
    expect_message(
      validate_import_numeric(test_numeric, 
                              field_name = "numeric", 
                              field_min = NA, 
                              field_max = 3, 
                              logfile = ""),
      "are greater than the stated maximum"
    )
  }
)

# validate_import_zipcode -------------------------------------------

test_that(
  "values in 12345, format or NA pass (from numeric)",
  {
    test_zip <- c(48169, NA_real_)
    expect_equal(
      validate_import_zipcode(test_zip, 
                              field_name = "zip", 
                              logfile = ""), 
      c("48169", NA_character_)
    )
  }
)

test_that(
  "values in 12345, 12345-1234 format or NA pass (from character)",
  {
    test_zip <- c("48169", "48169-0133", NA_real_)
    expect_equal(
      validate_import_zipcode(test_zip, 
                              field_name = "zip", 
                              logfile = ""), 
      c("48169", "48169-0133", NA_real_)
    )
  }
)

test_that(
  "values not in 12345 or 12345-1234 format are converted to NA (so they won't write)",
  {
    test_zip <- c("8169", "48169-01", "48169-abc", "zipcode")
    expect_equal(
      validate_import_zipcode(test_zip, 
                              field_name = "zip", 
                              logfile = ""),
      rep(NA_character_, length(test_zip))
    )
  }
)

test_that(
  "values not in 12345 or 12345-1234 format produce a message",
  {
    test_zip <- c("8169", "48169-01", "48169-abc", "zipcode")
    expect_message(
      validate_import_zipcode(test_zip, 
                              field_name = "zip", 
                              logfile = ""),
      "must be in the format `12345` or `12345-1234`"
    )
  }
)
# validate_import_yesno ---------------------------------------------

test_that(
  "yes, no, 0, 1, and NA are accepted (character)",
  {
    test_yes_no <- c("no", "yes", "0", "1", "No", "Yes", "NO", "YEs", "YES", NA_character_)
    expect_equal(
      validate_import_yesno(test_yes_no, 
                            field_name = "yesno", 
                            logfile = ""), 
      as.character(c(0, 1, 0, 1, 0, 1, 0, 1, 1, NA_real_))
    )
  }
)

test_that(
  "0, 1, and NA are accepted (numeric)",
  {
    test_yes_no <- c(0, 1, NA_real_)
    expect_equal(
      validate_import_yesno(test_yes_no, 
                            field_name = "yesno", 
                            logfile = ""), 
      as.character(c(0, 1, NA_real_))
    )
  }
)

test_that(
  "Unacceptable values are converted to NA to prevent writing (character)",
  {
    expect_equal(
      validate_import_yesno(c("negative", "affirmative"), 
                            field_name = "yesno", 
                            logfile = ""), 
      rep(NA_character_, 2)
    )
  }
)

test_that(
  "unacceptable values produce a message (character)", 
  {
    expect_message(
      validate_import_yesno(c("negative", "affirmative"), 
                            field_name = "yesno", 
                            logfile = ""), 
      "must be one of `0`, `1`, `No`, or `Yes`"
    )
  }
)

test_that(
  "Unacceptable values are converted to NA to prevent writing (numeric)",
  {
    expect_equal(
      validate_import_yesno(c(-1, pi, 12), 
                            field_name = "yesno", 
                            logfile = ""), 
      rep(NA_character_, 3)
    )
  }
)

test_that(
  "unacceptable values produce a message (numeric)", 
  {
    expect_message(
      validate_import_yesno(c(-1, pi, 12), 
                            field_name = "yesno", 
                            logfile = ""), 
      "must be one of `0`, `1`, `No`, or `Yes`"
    )
  }
)

# validate_import_truefalse -----------------------------------------

test_that(
  "true, false, yes, no, 0, 1, and NA are accepted (character)",
  {
    test_true_false <- c("true", "True", "TRUE", "truE", 
                         "false", "False", "FALSE", "falsE", 
                         "yes", "Yes", "YES", "yeS", 
                         "no", "No", "NO", "nO", 
                         "0", "1", NA_character_)
    expect_equal(
      validate_import_truefalse(test_true_false, 
                                field_name = "truefalse", 
                                logfile = ""), 
      as.character(c(1, 1, 1, 1, 
                     0, 0, 0, 0, 
                     1, 1, 1, 1,
                     0, 0, 0, 0, 
                     0, 1, NA_real_))
    )
  }
)

test_that(
  "0, 1, and NA are accepted (numeric)",
  {
    test_true_false <- c(0, 1, NA_real_)
    expect_equal(
      validate_import_truefalse(test_true_false, 
                                field_name = "truefalse", 
                                logfile = ""), 
      as.character(c(0, 1, NA_real_))
    )
  }
)

test_that(
  "TRUE, FALSE, and NA are accepted (logical)",
  {
    test_true_false <- c(TRUE, FALSE, NA)
    expect_equal(
      validate_import_truefalse(test_true_false, 
                                field_name = "truefalse", 
                                logfile = ""), 
      as.character(c(1, 0, NA_real_))
    )
  }
)

test_that(
  "Unacceptable values are converted to NA to prevent writing (character)",
  {
    expect_equal(
      validate_import_truefalse(c("negative", "affirmative"), 
                                field_name = "truefalse", 
                                logfile = ""), 
      rep(NA_character_, 2)
    )
  }
)

test_that(
  "unacceptable values produce a message (character)", 
  {
    expect_message(
      validate_import_truefalse(c("negative", "affirmative"), 
                                field_name = "truefalse", 
                                logfile = ""), 
      "must be one of logical or one of `0`, `1`, `No`, `Yes`, `False`, or `True`"
    )
  }
)

test_that(
  "Unacceptable values are converted to NA to prevent writing (numeric)",
  {
    expect_equal(
      validate_import_truefalse(c(-1, pi, 12), 
                                field_name = "truefalse", 
                                logfile = ""), 
      rep(NA_character_, 3)
    )
  }
)

test_that(
  "unacceptable values produce a message (numeric)", 
  {
    expect_message(
      validate_import_truefalse(c(-1, pi, 12), 
                            field_name = "truefalse", 
                            logfile = ""), 
      "must be one of logical or one of `0`, `1`, `No`, `Yes`, `False`, or `True`"
    )
  }
)


# validate_import_select_dropdown_radio -----------------------------

test_that(
  "mapped pairings with numeric and character codes pass (also NA)",
  {
    test_select <- c("-1", "0", "1", "a", "abc", 
                     "negative one", "zero", "one", "A", "ABC", 
                     NA_character_)
    mapping <- "-1, negative one | 0, zero | 1, one | a, A | abc, ABC"
    expect_equal(
      validate_import_select_dropdown_radio(test_select, 
                                            field_name = "select", 
                                            field_choice = mapping, 
                                            logfile = ""), 
      c("-1", "0", "1", "a", "abc", 
        "-1", "0", "1", "a", "abc", 
        NA_character_)
    )
  }
)

test_that(
  "mapped pairings with numeric and character codes pass (also NA)",
  {
    test_select <- c(-1, 0, 1, NA_real_)
    mapping <- "-1, negative one | 0, zero | 1, one | a, A | abc, ABC"
    expect_equal(
      validate_import_select_dropdown_radio(test_select, 
                                            field_name = "select", 
                                            field_choice = mapping, 
                                            logfile = ""), 
      c("-1", "0", "1",NA_character_)
    )
  }
)

test_that(
  "unmapped values are converted to NA (character)", 
  {
    mapping <- "-1, negative one | 0, zero | 1, one | a, A | abc, ABC"
    expect_equal(
      validate_import_select_dropdown_radio(c("XYZ", "15"), 
                                            field_name = "select", 
                                            field_choice = mapping, 
                                            logfile = ""), 
      c(NA_character_, NA_character_)
    )
  }
)

test_that(
  "unmapped values are converted to NA (numeric)", 
  {
    mapping <- "-1, negative one | 0, zero | 1, one | a, A | abc, ABC"
    expect_equal(
      validate_import_select_dropdown_radio(c(pi, 10), 
                                            field_name = "select", 
                                            field_choice = mapping, 
                                            logfile = ""), 
      c(NA_character_, NA_character_)
    )
  }
)

test_that(
  "unmapped values produce a message (character)", 
  {
    local_reproducible_output(width = 200)
    mapping <- "-1, negative one | 0, zero | 1, one | a, A | abc, ABC"
    expect_message(
      validate_import_select_dropdown_radio(c("XYZ", "15"), 
                                            field_name = "select", 
                                            field_choice = mapping, 
                                            logfile = ""), 
      "must be one of '-1', '0', '1', 'a', 'abc', 'negative one', 'zero', 'one', 'A', 'ABC'"
    )
  }
)

test_that(
  "unmapped values produce a message (numeric)", 
  {
    local_reproducible_output(width = 200)
    mapping <- "-1, negative one | 0, zero | 1, one | a, A | abc, ABC"
    expect_message(
      validate_import_select_dropdown_radio(c(pi, 10), 
                                            field_name = "select", 
                                            field_choice = mapping, 
                                            logfile = ""), 
      "must be one of '-1', '0', '1', 'a', 'abc', 'negative one', 'zero', 'one', 'A', 'ABC'"
    )
  }
)

# validate_import_email ---------------------------------------------

test_that(
  "common email addresses pass", 
  {
    email <- c("somebody@domain.net", 
               "some.body1@domain.org", 
               "345somebody789@domain.net", 
               "somebody-else@domain.com", 
               "salesperson@dash-company.biz", 
               "percy_jackson@camp-half-blood.edu", 
               "someone+spam@domain.widget", 
               "high%shooting@sports.ball", 
               NA_character_)
    expect_equal(
      validate_import_email(email, 
                            field_name = "email", 
                            logfile = ""), 
      email
    )
  }
)

test_that(
  "Invalid e-mails are changed to NA", 
  {
    email <- c("Im@work@nowhere.net", 
               "no-suffix@junkmail", 
               "one-length-suffix@email.g", 
               "long-suffix@email.sunburst")
    expect_equal(
      validate_import_email(email, 
                            field_name = "email", 
                            logfile = ""),
      rep(NA_character_, length(email))
    )
  }
)

test_that(
  "Invalid e-mails are changed to NA", 
  {
    email <- c("Im@work@nowhere.net", 
               "no-suffix@junkmail", 
               "one-length-suffix@email.g", 
               "long-suffix@email.sunburst")
    expect_message(
      validate_import_email(email, 
                            field_name = "email", 
                            logfile = ""),
      "are not valid e-mail addresses"
    )
  }
)

# validate_import_phone ---------------------------------------------

test_that(
  "valid phone numbers pass (including NA)", 
  {
    phone_punct <- c("(207) 555-1234", 
                     "207.555.1234", 
                     "207-555-1234", 
                     "207 555 1234")
    # to test all valid phone numbers would be overly tedious. 
    # we'll just a sample. Change n_size to match your desired rigor
    n_size <- 10
    phone_random <- sprintf("%s%s%s %s%s%s %s%s%s%s", 
                            sample(2:9, n_size, replace = TRUE), 
                            sample(0:8, n_size, replace = TRUE), 
                            sample(0:9, n_size, replace = TRUE), 
                            sample(2:9, n_size, replace = TRUE), 
                            sample(0:9, n_size, replace = TRUE),
                            sample(0:9, n_size, replace = TRUE),
                            sample(0:9, n_size, replace = TRUE),
                            sample(0:9, n_size, replace = TRUE),
                            sample(0:9, n_size, replace = TRUE),
                            sample(0:9, n_size, replace = TRUE))
    test_phone <- c(phone_punct, phone_random, NA_character_)
    
    expect_equal(
      validate_import_phone(test_phone, 
                            field_name = "phone", 
                            logfile = ""),
      gsub("[[:punct:][:space:]]", "", test_phone)
    )
  }
)

test_that(
  "phone numbers of more than 10 digits become NA",
  {
    expect_equal(
      validate_import_phone(c("555-555-5555-5", 
                              "555-555-5555-5555"), 
                            field_name = "phone", 
                            logfile = ""), 
      c(NA_character_, NA_character_)
    )
  }
)

test_that(
  "phone numbers of more than 10 digits produce a message",
  {
    expect_message(
      validate_import_phone(c("555-555-5555-5", 
                              "555-555-5555-5555"), 
                            field_name = "phone", 
                            logfile = ""), 
      "are not 10 digit phone numbers"
    )
  }
)

test_that(
  "phone numbers with invalid format become NA",
  {
    # The fives are valid digits. The non-five digits
    # are placed where those values are not allowed
    expect_equal(
      validate_import_phone(c("055-555-5555",
                              "155-555-5555", 
                              "595-555-5555", 
                              "555-155-5555"), 
                            field_name = "phone", 
                            logfile = ""), 
      c(NA_character_, NA_character_, NA_character_, NA_character_)
    )
  }
)

test_that(
  "phone numbers with invalid format produce a message",
  {
    expect_message(
      validate_import_phone(c("055-555-5555",
                              "155-555-5555", 
                              "595-555-5555", 
                              "555-155-5555"), 
                            field_name = "phone", 
                            logfile = ""), 
      "are not valid North American phone numbers"
    )
  }
)

