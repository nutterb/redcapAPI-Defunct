context("vectorToApiBodyList.R")


test_that(
  "Return a named list", 
  checkmate::expect_list(
    x = vectorToApiBodyList(1:3, "records"), 
    names = "named"
  )
)


test_that(
  "The name of each element in the list starts with the value of `parameter_name`", 
  {
    out <- vectorToApiBodyList(1:3, "just_a_name")
    
    expect_true(
      all(grepl("^just_a_name", names(out)))
    )
  }
)


test_that(
  "Throw an error if `x` is not an atomic vector", 
  {
    expect_error(
      vectorToApiBodyList(list(1:3), "records")
    )
  }
)


test_that(
  "Throw an error if `parameter_name` is not a `character(1)`",
  {
    expect_error(
      vectorToApiBodyList(1:3, 
                          list("parameter_name"))
    )
  }
)

test_that(
  "Throw an error if `parameter_name` is not a `character(1)`",
  {
    expect_error(
      vectorToApiBodyList(1:3, 
                          letters)
    )
  }
)
