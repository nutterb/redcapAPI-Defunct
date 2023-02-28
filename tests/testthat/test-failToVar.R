context("fieldToVar")

rcon <- redcapConnection(url = url, token = API_KEY)

# pull data for test date_ with handler
rec_hd <- exportRecords(rcon, handlers = list(date_ = as.Date))
test_that("date_ = as.Date returns class Date for date_dmy", expect_is(rec_hd$date_dmy, "Date"))
test_that("date_ = as.Date returns class Date for date_mdy", expect_is(rec_hd$date_mdy, "Date"))
test_that("date_ = as.Date returns class Date for date_ymd", expect_is(rec_hd$date_ymd, "Date"))

# pull data for test time with handler
rec_ht <- exportRecords(rcon, handlers = list(time = function(x) 1.23))
test_that("time = function(x) 1.23 returns class numeric for time_hhmm", expect_is(rec_ht$time_hhmm, "numeric"))

# pull data for test time_mm_ss with handler
rec_htms <- exportRecords(rcon, handlers = list(time_mm_ss = function(x) 1.23))
test_that("time_mm_ss = function(x) 1.23 returns class numeric for time_mmss", expect_is(rec_htms$time_mmss, "numeric"))

# pull data for test time with handler
rec_hthms <- exportRecords(rcon, handlers = list(time_hh_mm_ss = function(x) 1.23))
test_that("time_ = function(x) 1.23 returns class numeric for time_hhmmss", expect_is(rec_hthms$time_hhmmss, "numeric"))

# pull data for test datetime_ with handler
rec_hdt <- exportRecords(rcon, handlers=list(datetime_ = as.Date))
test_that("datetime_ = as.Date returns class Date for datetime_dmy_hm", expect_is(rec_hdt$datetime_dmy_hm, "Date"))
test_that("datetime_ = as.Date returns class Date for datetime_mdy_hm", expect_is(rec_hdt$datetime_mdy_hm, "Date"))
test_that("datetime_ = as.Date returns class Date for datetime_ymd_hm", expect_is(rec_hdt$datetime_ymd_hm, "Date"))

rec_hdst <- exportRecords(rcon, handlers=list(datetime_seconds_ = as.Date))
test_that("datetime_seconds_ = as.Date returns class Date for datetime_dmy_hms", expect_is(rec_hdst$datetime_dmy_hms, "Date"))
test_that("datetime_seconds_ = as.Date returns class Date for datetime_mdy_hms", expect_is(rec_hdst$datetime_mdy_hms, "Date"))
test_that("datetime_seconds_ = as.Date returns class Date for datetime_ymd_hms", expect_is(rec_hdst$datetime_ymd_hms, "Date"))

# pull data for test dates = true
rec_dt <- exportRecords(rcon, dates = TRUE)
test_that("dates = TRUE returns class POSIXt for date_dmy", expect_is(rec_dt$date_dmy, "POSIXt"))
test_that("dates = TRUE returns class POSIXt for date_mdy", expect_is(rec_dt$date_mdy, "POSIXt"))
test_that("dates = TRUE returns class POSIXt for date_ymd", expect_is(rec_dt$date_ymd, "POSIXt"))

test_that("dates = TRUE returns class character for time_hhmmss", expect_is(rec_dt$time_hhmmss, "character")) # fieldToVar returns hour, min, seconds as 'character'
test_that("dates = TRUE returns class times for time_hhmm", expect_is(rec_dt$time_hhmm, "times"))
test_that("dates = TRUE returns class times for time_mmss", expect_is(rec_dt$time_mmss, "times"))

test_that("dates = TRUE returns class Date for datetime_dmy_hm", expect_is(rec_dt$datetime_dmy_hm, "POSIXt"))
test_that("dates = TRUE returns class Date for datetime_mdy_hm", expect_is(rec_dt$datetime_mdy_hm, "POSIXt"))
test_that("dates = TRUE returns class Date for datetime_ymd_hm", expect_is(rec_dt$datetime_ymd_hm, "POSIXt"))

test_that("dates = TRUE returns class Date for datetime_dmy_hms", expect_is(rec_dt$datetime_dmy_hms, "POSIXt"))
test_that("dates = TRUE returns class Date for datetime_mdy_hms", expect_is(rec_dt$datetime_mdy_hms, "POSIXt"))
test_that("dates = TRUE returns class Date for datetime_ymd_hms", expect_is(rec_dt$datetime_ymd_hms, "POSIXt"))

# pull data for test dates = false
rec_df <- exportRecords(rcon, dates = FALSE)
test_that("dates = FALSE returns class character for date_dmy", expect_is(rec_df$date_dmy, "character"))
test_that("dates = FALSE returns class character for date_mdy", expect_is(rec_df$date_mdy, "character"))
test_that("dates = FALSE returns class character for date_ymd", expect_is(rec_df$date_ymd, "character"))

test_that("dates = FALSE returns class Date for time_hhmmss", expect_is(rec_df$time_hhmmss, "character"))
test_that("dates = FALSE returns class Date for time_hhmm", expect_is(rec_df$time_hhmm, "character"))
test_that("dates = FALSE returns class Date for time_mmss", expect_is(rec_df$time_mmss, "character"))

test_that("dates = FALSE returns class Date for datetime_dmy_hm", expect_is(rec_df$datetime_dmy_hm, "character"))
test_that("dates = FALSE returns class Date for datetime_mdy_hm", expect_is(rec_df$datetime_mdy_hm, "character"))
test_that("dates = FALSE returns class Date for datetime_ymd_hm", expect_is(rec_df$datetime_ymd_hm, "character"))

test_that("dates = FALSE returns class Date for datetime_dmy_hms", expect_is(rec_df$datetime_dmy_hms, "character"))
test_that("dates = FALSE returns class Date for datetime_mdy_hms", expect_is(rec_df$datetime_mdy_hms, "character"))
test_that("dates = FALSE returns class Date for datetime_ymd_hms", expect_is(rec_df$datetime_ymd_hms, "character"))


