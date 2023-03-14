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
