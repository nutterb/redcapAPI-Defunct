context("exportArms.R")

if (file.exists("local-token.Rdata")){
  load("local-token.Rdata")
} else {
  url <- "https://redcap.notaplace.net/redcap/api/"
  token_case_01 <- "NOTaREALtoken1234567890123456789"
}

rcon <- redcapConnection(url = url,
                         token = token_case_01)

test_that(
  "Return NULL for simple project",
  {
    expect_null(
      exportArms(rcon)
    )
  }
)