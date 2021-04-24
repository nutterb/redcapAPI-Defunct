context("makeApiCall.R")

this_rcon <- redcapConnection(url = "some_url", 
                              token = "ABCDEFGHIJKLMNOPQRSTUVWXYZ123456")

# Functional Requirement 1 ------------------------------------------

test_that(
  "Throw an error if `rcon` is not a `redcapApiConnection`", 
  expect_error(
    makeApiCall(rcon = "not_an_rcon")
  )
)


# Functional Requirement 2 ------------------------------------------

test_that(
  "Throw an error if `body` is not a list",
  {
    expect_error(
      makeApiCall(rcon = this_rcon, 
                  body = "not a list")
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Throw an error if `config` is not a list",
  {
    expect_error(
      makeApiCall(rcon = this_rcon, 
                  config = "not a list")
    )
  }
)
