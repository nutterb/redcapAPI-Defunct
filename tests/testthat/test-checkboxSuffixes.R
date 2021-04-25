context("checkboxSuffixes.R")

# Functional Requirement 1 ------------------------------------------

test_that(
  "Return a list with two vectors of names and labels.", 
  {
    expect_type(
      checkboxSuffixes(fields = c("checkbox"), 
                       meta_data = TestCase01_MetaData, 
                       version = "6.0.0"), 
      type = "list"
    )
  }
)

test_that(
  "Return a list with two vectors of names and labels.", 
  {
    expect_equal(
      names(checkboxSuffixes(fields = c("checkbox"), 
                             meta_data = TestCase01_MetaData, 
                             version = "6.0.0")), 
      c("name_suffix", "label_suffix")
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Throw an error if `fields` is not a `character`", 
  {
    expect_error(
      checkboxSuffixes(fields = 1:2, 
                       meta_data = TestCase01_MetaData, 
                       version = "6.0.0")
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Throw an error if `meta_data` is not a `data.frame`", 
  {
    expect_error(
      checkboxSuffixes(fields = c("checkbox"), 
                       meta_data = "not a data frame", 
                       version = "6.0.0")
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Throw an error if `meta_data` is not a `data.frame`", 
  {
    expect_error(
      checkboxSuffixes(fields = c("checkbox"), 
                       meta_data = TestCase01_MetaData, 
                       version = c("6.0.0", "7.0.0"))
    )
  }
)

test_that(
  "Throw an error if `meta_data` is not a `data.frame`", 
  {
    expect_error(
      checkboxSuffixes(fields = c("checkbox"), 
                       meta_data = TestCase01_MetaData, 
                       version = TRUE)
    )
  }
)
