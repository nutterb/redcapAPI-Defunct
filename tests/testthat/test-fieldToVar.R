context("fieldToVar")

rcon <- redcapConnection(url = url, token = API_KEY)

# pull data for tests with handler
rec_ht <- exportRecords(rcon, handlers = list(date_ = as.Date, time = function(x) 1.23, time_mm_ss = function(x) 1.23, time_hh_mm_ss = function(x) 1.23, datetime_ = as.Date, datetime_seconds_ = as.Date))
test_that("date_ = as.Date returns class Date for date_dmy", expect_is(rec_ht$date_dmy, "Date"))
test_that("date_ = as.Date returns class Date for date_mdy", expect_is(rec_ht$date_mdy, "Date"))
test_that("date_ = as.Date returns class Date for date_ymd", expect_is(rec_ht$date_ymd, "Date"))

test_that("time = function(x) 1.23 returns class numeric for time_hhmm", expect_is(rec_ht$time_hhmm, "numeric"))
test_that("time_mm_ss = function(x) 1.23 returns class numeric for time_mmss", expect_is(rec_ht$time_mmss, "numeric"))
test_that("time_ = function(x) 1.23 returns class numeric for time_hhmmss", expect_is(rec_ht$time_hhmmss, "numeric"))

test_that("datetime_ = as.Date returns class Date for datetime_dmy_hm", expect_is(rec_ht$datetime_dmy_hm, "Date"))
test_that("datetime_ = as.Date returns class Date for datetime_mdy_hm", expect_is(rec_ht$datetime_mdy_hm, "Date"))
test_that("datetime_ = as.Date returns class Date for datetime_ymd_hm", expect_is(rec_ht$datetime_ymd_hm, "Date"))

test_that("datetime_seconds_ = as.Date returns class Date for datetime_dmy_hms", expect_is(rec_ht$datetime_dmy_hms, "Date"))
test_that("datetime_seconds_ = as.Date returns class Date for datetime_mdy_hms", expect_is(rec_ht$datetime_mdy_hms, "Date"))
test_that("datetime_seconds_ = as.Date returns class Date for datetime_ymd_hms", expect_is(rec_ht$datetime_ymd_hms, "Date"))

# pull data for tests with dates = true
rec_dt <- exportRecords(rcon, dates = TRUE)
test_that("dates = TRUE returns class POSIXt for date_dmy", expect_is(rec_dt$date_dmy, "POSIXt"))
test_that("dates = TRUE returns class POSIXt for date_mdy", expect_is(rec_dt$date_mdy, "POSIXt"))
test_that("dates = TRUE returns class POSIXt for date_ymd", expect_is(rec_dt$date_ymd, "POSIXt"))

test_that("dates = TRUE returns class times for time_hhmmss", expect_is(rec_dt$time_hhmmss, "times"))
test_that("dates = TRUE returns class times for time_hhmm", expect_is(rec_dt$time_hhmm, "times"))
test_that("dates = TRUE returns class times for time_mmss", expect_is(rec_dt$time_mmss, "times"))

test_that("dates = TRUE returns class Date for datetime_dmy_hm", expect_is(rec_dt$datetime_dmy_hm, "POSIXt"))
test_that("dates = TRUE returns class Date for datetime_mdy_hm", expect_is(rec_dt$datetime_mdy_hm, "POSIXt"))
test_that("dates = TRUE returns class Date for datetime_ymd_hm", expect_is(rec_dt$datetime_ymd_hm, "POSIXt"))

test_that("dates = TRUE returns class Date for datetime_dmy_hms", expect_is(rec_dt$datetime_dmy_hms, "POSIXt"))
test_that("dates = TRUE returns class Date for datetime_mdy_hms", expect_is(rec_dt$datetime_mdy_hms, "POSIXt"))
test_that("dates = TRUE returns class Date for datetime_ymd_hms", expect_is(rec_dt$datetime_ymd_hms, "POSIXt"))

test_that("dates = TRUE returns 2023-02-24 for date_dmy in first rec",
          expect_true(rec_dt$date_dmy[1] == as.POSIXct("2023-02-24")))
test_that("dates = TRUE returns 2023-02-24 for date_mdy in first rec",
          expect_true(rec_dt$date_mdy[1] == as.POSIXct("2023-02-24")))
test_that("dates = TRUE returns 2023-02-24 for date_ymd in first rec",
          expect_true(rec_dt$date_ymd[1] == as.POSIXct("2023-02-24")))

test_that("dates = TRUE returns 12:04:55 for time_hhmmss in first rec",
          expect_true(rec_dt$time_hhmmss[1] ==
                       chron::times("12:04:55", format=c(times="h:m:s"))))
test_that("dates = TRUE returns 12:04:55 for time_hhmm in first rec",
          expect_true(rec_dt$time_hhmm[1] ==
                       chron::times("12:04:00", format=c(times="h:m:s"))))
test_that("dates = TRUE returns 12:04:55 for time_mmss in first rec",
          expect_true(rec_dt$time_mmss[1] ==
                       chron::times("00:02:45", format=c(times="h:m:s"))))

test_that("dates = TRUE returns 2023-02-24 12:04 for datetime_dmy_hm in first rec",
          expect_true(rec_dt$datetime_dmy_hm[1] == 
                        as.POSIXct("2023-02-24 12:04", format="%Y-%m-%d %H:%M")))
test_that("dates = TRUE returns 2023-02-24 12:04 for datetime_mdy_hm in first rec",
          expect_true(rec_dt$datetime_mdy_hm[1] == 
                        as.POSIXct("2023-02-24 12:04", format="%Y-%m-%d %H:%M")))
test_that("dates = TRUE returns 2023-02-24 12:04 datetime_ymd_hm in first rec",
          expect_true(rec_dt$datetime_ymd_hm[1] == 
                        as.POSIXct("2023-02-24 12:04", format="%Y-%m-%d %H:%M")))

test_that("dates = TRUE returns 2023-02-24 12:40:50 for datetime_dmy_hms in first rec",
          expect_true(rec_dt$datetime_dmy_hms[1] ==
                        as.POSIXct("2023-02-24 12:40:50", format="%Y-%m-%d %H:%M:%S")))
test_that("dates = TRUE returns 2023-02-24 12:40:50 for datetime_mdy_hms in first rec",
          expect_true(rec_dt$datetime_mdy_hms[1] ==
                        as.POSIXct("2023-02-24 12:40:50", format="%Y-%m-%d %H:%M:%S")))
test_that("dates = TRUE returns 2023-02-24 12:40:50 for datetime_ymd_hms in first rec",
          expect_true(rec_dt$datetime_ymd_hms[1] ==
                        as.POSIXct("2023-02-24 12:40:50", format="%Y-%m-%d %H:%M:%S")))


# pull data for tests with dates = false
rec_df <- exportRecords(rcon, dates = FALSE)
test_that("dates = FALSE returns class character for date_dmy", expect_is(rec_df$date_dmy, "character"))
test_that("dates = FALSE returns class character for date_mdy", expect_is(rec_df$date_mdy, "character"))
test_that("dates = FALSE returns class character for date_ymd", expect_is(rec_df$date_ymd, "character"))

test_that("dates = FALSE returns class character for time_hhmmss", expect_is(rec_df$time_hhmmss, "character"))
test_that("dates = FALSE returns class character for time_hhmm", expect_is(rec_df$time_hhmm, "character"))
test_that("dates = FALSE returns class character for time_mmss", expect_is(rec_df$time_mmss, "character"))

test_that("dates = FALSE returns class character for datetime_dmy_hm", expect_is(rec_df$datetime_dmy_hm, "character"))
test_that("dates = FALSE returns class character for datetime_mdy_hm", expect_is(rec_df$datetime_mdy_hm, "character"))
test_that("dates = FALSE returns class character for datetime_ymd_hm", expect_is(rec_df$datetime_ymd_hm, "character"))

test_that("dates = FALSE returns class character for datetime_dmy_hms", expect_is(rec_df$datetime_dmy_hms, "character"))
test_that("dates = FALSE returns class character for datetime_mdy_hms", expect_is(rec_df$datetime_mdy_hms, "character"))
test_that("dates = FALSE returns class character for datetime_ymd_hms", expect_is(rec_df$datetime_ymd_hms, "character"))

test_that("dates = FALSE returns '2023-02-24' for date_dmy first rec",
          expect_true(rec_df$date_dmy[1] == "2023-02-24"))
test_that("dates = FALSE returns '2023-02-24' for date_mdy first rec",
          expect_true(rec_df$date_mdy[1] == "2023-02-24"))
test_that("dates = FALSE returns '2023-02-24' for date_ymd first rec",
          expect_true(rec_df$date_ymd[1] == "2023-02-24"))

test_that("dates = FALSE returns '12:04:55' for time_hhmmss first rec",
          expect_true(rec_df$time_hhmmss[1] == "12:04:55"))
test_that("dates = FALSE returns '12:04' for time_hhmm first rec",
          expect_true(rec_df$time_hhmm[1] == "12:04"))
test_that("dates = FALSE returns '02:45' for time_mmss first rec",
          expect_true(rec_df$time_mmss[1] == "02:45"))

test_that("dates = FALSE returns '2023-02-24 12:04' for datetime_dmy_hm first rec",
          expect_true(rec_df$datetime_dmy_hm[1] == "2023-02-24 12:04"))
test_that("dates = FALSE returns '2023-02-24 12:04' for datetime_mdy_hm first rec",
          expect_true(rec_df$datetime_mdy_hm[1] == "2023-02-24 12:04"))
test_that("dates = FALSE returns '2023-02-24 12:04' for datetime_ymd_hm first rec",
          expect_true(rec_df$datetime_ymd_hm[1] == "2023-02-24 12:04"))

test_that("dates = FALSE returns 2023-02-24 12:40:50 for datetime_dmy_hms first rec",
          expect_true(rec_df$datetime_dmy_hms[1] == "2023-02-24 12:40:50"))
test_that("dates = FALSE returns 2023-02-24 12:40:50 for datetime_mdy_hms first rec",
          expect_true(rec_df$datetime_mdy_hms[1] == "2023-02-24 12:40:50"))
test_that("dates = FALSE returns 2023-02-24 12:40:50 for datetime_ymd_hms first rec",
          expect_true(rec_df$datetime_ymd_hms[1] == "2023-02-24 12:40:50"))

