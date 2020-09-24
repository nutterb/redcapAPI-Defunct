context("exportRecords.R")

if (file.exists("local-token.Rdata")){
  load("local-token.Rdata")
} else {
  url <- "https://redcap.notaplace.net/redcap/api/"
  token_case_01 <- "NOTaREALtoken1234567890123456789"
}

rcon <- redcapConnection(url = url,
                         token = token_case_01)

test_that(
  "Export simple records set",
  {
    expect_silent(
      exportRecords(rcon)
    )
  }
)