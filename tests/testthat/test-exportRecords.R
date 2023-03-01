context("exportRecords")

rcon <- redcapConnection(url = url, token = API_KEY)

test_that("records can be exported",{
  expect_silent(rec <- exportRecords(rcon))
  expect_gte(length(rec), 1)
})
