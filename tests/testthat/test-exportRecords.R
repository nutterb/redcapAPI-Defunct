context("exportRecords")

skip_on_cran() # Do not RUN on CRAN, requires a custom build REDCap database.

rcon <- redcapConnection(url = url, token = API_KEY)

test_that("records can be exported",{
  expect_silent(rec <- exportRecords(rcon))
  expect_gte(length(rec), 1)
})
