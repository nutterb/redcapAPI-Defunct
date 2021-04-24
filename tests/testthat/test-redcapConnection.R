context("redcapConnection.R")

# Functional Requirement 1 ------------------------------------------

test_that(
  "Return an object of class `redcapApiConnection`",
  {
    expect_equal(
      class(redcapConnection(url = "https://some_url.com/api", 
                     token = "abcdefghijklmnopqrstuvwxyz123456")),
      "redcapApiConnection"
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Leading and trailing white space are removed from `token`",
  expect_equal(
    redcapConnection(url = "https://some_url.com/api", 
                     token = " abcdefghijklmnopqrstuvwxyz123456 ")$token,
    "abcdefghijklmnopqrstuvwxyz123456"
  )
)


# Functional Requirement 3 ------------------------------------------

test_that(
  "Throw an error if `url` is not a `character(1)`", 
  expect_error(
    redcapConnection(url = 123, 
                     token = "abcdefghijklmnopqrstuvwxyz123456"), 
  )
)

test_that(
  "Throw an error if `url` is not a `character(1)`", 
  expect_error(
    redcapConnection(url = c("url1", "url2"), 
                     token = "abcdefghijklmnopqrstuvwxyz123456"), 
  )
)


# Functional Requirement 4 ------------------------------------------

test_that(
  "Throw an error if `token` is not a `character(1)`", 
  expect_error(
    redcapConnection(url = "https://some_url.com/api", 
                     token = c(TRUE, FALSE)), 
  )
)

test_that(
  "Throw an error if `url` is not a `character(1)`", 
  expect_error(
    redcapConnection(url = "https://some_url.com/api", 
                     token = c("abcdefghijklmnopqrstuvwxyz123456", 
                               "abcdefghijklmnopqrstuvwxyz123456")), 
  )
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "Throw an error if `token` does not have either 32 or 64 characters",
  expect_error(
    redcapConnection(url = "https://some_url.com/api", 
                     token = "abcdefghijklmnopqrstuvwxyz1234567")
  )
)

test_that(
  "Throw an error if `token` does not have either 32 or 64 characters",
  expect_error(
    redcapConnection(url = "https://some_url.com/api", 
                     token = "abcdefghijklmnopqrstuvwxyz12345")
  )
)

test_that(
  "Throw an error if `token` does not have either 32 or 64 characters",
  expect_error(
    redcapConnection(url = "https://some_url.com/api", 
                     token = "abcdefghijklmnopqrstuvwxyz123456abcdefghijklmnopqrstuvwxyz1234567")
  )
)

test_that(
  "Throw an error if `token` does not have either 32 or 64 characters",
  expect_error(
    redcapConnection(url = "https://some_url.com/api", 
                     token = "abcdefghijklmnopqrstuvwxyz123456abcdefghijklmnopqrstuvwxyz12345")
  )
)


# Functional Requirement 6 ------------------------------------------

test_that(
  "Throw an error if `config` is not a `list`", 
  {
    expect_error(
      redcapConnection(url = "https://some_url.com/api", 
                       token = "abcdefghijklmnopqrstuvwxyz123456",
                       config = "this is not a list")
    )
  }
)