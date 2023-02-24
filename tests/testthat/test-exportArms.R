context("exportArms")

skip_on_cran()

rcon <- redcapConnection(url = url,
                         token = API_KEY)

test_that("returns NULL for a simple project",{
  expect_null(exportArms(rcon))
})