context("exportArms")

rcon <- redcapConnection(url = url, token = API_KEY)

test_that("returns 2 for a project with 2 arms",{
  expect_equal(length(exportArms(rcon)), 2)
})
