context("redcapConnection.R")

if (file.exists("local-token.Rdata")){
  load("local-token.Rdata")
} else {
  url <- "https://redcap.notaplace.net/redcap/api/"
  token_case_01 <- "NOTaREALtoken1234567890123456789"
}

test_that(
  "Create a redcapApiConnection object",
  expect_equal(
    class(redcapConnection(url = url,
                           token = token_case_01)),
    "redcapApiConnection"
  )
)

test_that(
  "Cast an error if url is missing",
  {
    expect_error(
      redcapConnection(token = token)
    )
  }
)

test_that(
  "Cast an error if token is missing",
  {
    expect_error(
      redcapConnection(url = url)
    )
  }
)
