context("redcapConnection")

test_that("redcapApiConnection can be created",
  expect_equal(
    class(redcapConnection(url = url, token = API_KEY)),
    "redcapApiConnection"
  )
)

test_that("redcapConnection throws an error if url is missing",
  expect_error(redcapConnection(token = API_KEY))
)

test_that("redcapConnection throws an  error if token is missing",
  expect_error(redcapConnection(url = url))
)
