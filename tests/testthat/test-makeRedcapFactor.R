context("makeRedcapFactor.R")

radio_row <- which(TestCase01_MetaData$field_name == "radio_buttons")
coding_for_radio <- TestCase01_MetaData$select_choices_or_calculations[radio_row]

checkbox_row <- which(TestCase01_MetaData$field_name == "checkbox")
coding_for_checkbox <- TestCase01_MetaData$select_choices_or_calculations[checkbox_row]

# Functional Requirement 1 ------------------------------------------

test_that(
  "Return a redcapFactor object that inherits `factor` (makeRedcapFactor)",
  {
    expect_true(
      inherits(
        makeRedcapFactor(x = 1:3, 
                         coding = coding_for_radio, 
                         factors = TRUE, 
                         var_name = "radio_buttons"), 
        "redcapFactor"
      )
    )
  }
)


test_that(
  "Return a redcapFactor object that inherits `factor` (makeRedcapFactor)",
  {
    expect_true(
      inherits(
        makeRedcapFactor(x = 1:3, 
                         coding = coding_for_radio, 
                         factors = FALSE, 
                         var_name = "radio_buttons"), 
        "redcapFactor"
      )
    )
  }
)


test_that(
  "Return a redcapFactor object that inherits `factor` (makeRedcapYN)",
  {
    expect_true(
      inherits(
        makeRedcapYN(0:1, 
                     factors = TRUE), 
        "redcapFactor"
      )
    )
  }
)


test_that(
  "Return a redcapFactor object that inherits `factor` (makeRedcapYN)",
  {
    expect_true(
      inherits(
        makeRedcapYN(0:1, 
                     factors = FALSE), 
        "redcapFactor"
      )
    )
  }
)

test_that(
  "Return a redcapFactor object that inherits `factor` (makeRedcapCheckbox)",
  {
    expect_true(
      inherits(
        makeRedcapCheckbox(x = 0:1,
                           suffix = "1", 
                           coding = coding_for_checkbox, 
                           factors = TRUE, 
                           checkboxLabels = TRUE), 
        "redcapFactor"
      )
    )
  }
)


test_that(
  "Return a redcapFactor object that inherits `factor` (makeRedcapCheckbox)",
  {
    expect_true(
      inherits(
        makeRedcapCheckbox(x = 0:1,
                           suffix = "1", 
                           coding = coding_for_checkbox, 
                           factors = FALSE, 
                           checkboxLabels = TRUE), 
        "redcapFactor"
      )
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "When `factors = TRUE` the object returns also inherits `factor` (makeRedcapFactor)", 
  {
    checkmate::expect_class(
      x = makeRedcapFactor(x = 1:3, 
                           coding = coding_for_radio, 
                           factors = TRUE, 
                           var_name = "radio_buttons"), 
      classes = c("redcapFactor", "factor")
    )
  }
)

test_that(
  "When `factors = TRUE` the object returns also inherits `factor` (makeRedcapYN)", 
  {
    checkmate::expect_class(
      makeRedcapYN(0:1, 
                   factors = TRUE), 
      classes = c("redcapFactor", "factor")
    )
  }
)  

test_that(
  "When `factors = TRUE` the object returns also inherits `factor` (makeRedcapCheckbox)", 
  {
    checkmate::expect_class(
      makeRedcapCheckbox(x = 0:1,
                         suffix = "1", 
                         coding = coding_for_checkbox, 
                         factors = TRUE, 
                         checkboxLabels = TRUE), 
      classes = c("redcapFactor", "factor")
    )
  }
)  

# Functional Requirement 3 ------------------------------------------

test_that(
  "Throw an error if `x` is not an atomic vector (makeRedcapFactor)", 
  {
    expect_error(
      makeRedcapFactor(x = list(1:3), 
                       coding = coding_for_radio, 
                       factors = TRUE, 
                       var_name = "radio_buttons")
    )
  }
)

test_that(
  "Throw an error if `x` is not an atomic vector (makeRedcapYN)", 
  {
    expect_error(
      makeRedcapYN(list(0:1), 
                   factors = TRUE)
    )
  }
)

test_that(
  "Throw an error if `x` is not an atomic vector (makeRecapCheckbox)", 
  {
    expect_error(
      makeRedcapCheckbox(x = list(1:3),
                         suffix = "1", 
                         coding = coding_for_checkbox, 
                         factors = TRUE, 
                         checkboxLabels = TRUE)
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Throw an error if `coding` is not a `character(1)` (makeRedcapFactor)", 
  {
    expect_error(
      makeRedcapFactor(x = 1:3, 
                       coding = 1, 
                       factors = TRUE, 
                       var_name = "radio_buttons")
    )
  }
)

test_that(
  "Throw an error if `coding` is not a `character(1)` (makeRedcapFactor)", 
  {
    expect_error(
      makeRedcapFactor(x = 1:3, 
                       coding = letters, 
                       factors = TRUE, 
                       var_name = "radio_buttons")
    )
  }
)

test_that(
  "Throw an error if `coding` is not a `character(1)` (makeRedcapCheckbox)", 
  {
    expect_error(
      makeRedcapCheckbox(x = 0:1,
                         suffix = "1", 
                         coding = 1, 
                         factors = TRUE, 
                         checkboxLabels = TRUE)
    )
  }
)

test_that(
  "Throw an error if `coding` is not a `character(1)` (makeRedcapCheckbox)", 
  {
    expect_error(
      makeRedcapCheckbox(x = 0:1,
                         suffix = "1", 
                         coding = letters, 
                         factors = TRUE, 
                         checkboxLabels = TRUE)
    )
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "Throw an error if `factors` is not a `logical(1)` (makeRedcapFactor)", 
  {
    expect_error(
      makeRedcapFactor(x = 1:3, 
                       coding = coding_for_radio, 
                       factors = "TRUE", 
                       var_name = "radio_buttons")
    )
  }
)

test_that(
  "Throw an error if `factors` is not a `logical(1)` (makeRedcapFactor)", 
  {
    expect_error(
      makeRedcapFactor(x = 1:3, 
                       coding = coding_for_radio, 
                       factors = c(TRUE, FALSE), 
                       var_name = "radio_buttons")
    )
  }
)

test_that(
  "Throw an error if `factors` is not a `logical(1)` (makeRedcapYN)", 
  {
    expect_error(
      makeRedcapYN(x = 0:1,
                   factors = "TRUE")
    )
  }
)

test_that(
  "Throw an error if `factors` is not a `logical(1)` (makeRedcapYN)", 
  {
    expect_error(
      makeRedcapYN(x = 0:1,
                   factors = c(TRUE, FALSE))
    )
  }
)

test_that(
  "Throw an error if `factors` is not a `logical(1)` (makeRedcapCheckbox)", 
  {
    expect_error(
      makeRedcapCheckbox(x = 0:1,
                         suffix = "1", 
                         coding = coding_for_checkbox, 
                         factors = "TRUE", 
                         checkboxLabels = TRUE)
    )
  }
)

test_that(
  "Throw an error if `factors` is not a `logical(1)` (makeRedcapCheckbox)", 
  {
    expect_error(
      makeRedcapCheckbox(x = 0:1,
                         suffix = "1", 
                         coding = coding_for_checkbox, 
                         factors = c(TRUE, FALSE), 
                         checkboxLabels = TRUE)
    )
  }
)

# Functional Requirement 6 ------------------------------------------

test_that(
  "Throw an error if `suffix` is not a `character(1)` (makeRedcapCheckbox)", 
  {
    expect_error(
      makeRedcapCheckbox(x = 0:1,
                         suffix = list("1"), 
                         coding = coding_for_checkbox, 
                         factors = TRUE, 
                         checkboxLabels = TRUE)
    )
  }
)

test_that(
  "Throw an error if `suffix` is not a `character(1)` (makeRedcapCheckbox)", 
  {
    expect_error(
      makeRedcapCheckbox(x = 0:1,
                         suffix = letters, 
                         coding = coding_for_checkbox, 
                         factors = TRUE, 
                         checkboxLabels = TRUE)
    )
  }
)

# Functional Requirement 7 ------------------------------------------

test_that(
  "Throw an error if `checkboxLabels` is not a `logical(1)`", 
  {
    expect_error(
      makeRedcapCheckbox(x = 0:1,
                         suffix = "1", 
                         coding = coding_for_checkbox, 
                         factors = TRUE, 
                         checkboxLabels = "TRUE")
    )
  }
)

test_that(
  "Throw an error if `checkboxLabels` is not a `logical(1)`", 
  {
    expect_error(
      makeRedcapCheckbox(x = 0:1,
                         suffix = "1", 
                         coding = coding_for_checkbox, 
                         factors = TRUE, 
                         checkboxLabels = c(TRUE, FALSE))
    )
  }
)

# Functional Requirement 8 ------------------------------------------

test_that(
  "Throw an error if `var_name` is not a `character(1)`", 
  {
    expect_error(
      makeRedcapFactor(x = 1:3, 
                       coding = coding_for_radio, 
                       factors = TRUE, 
                       var_name = list("radio_buttons"))
    )
  }
)

test_that(
  "Throw an error if `var_name` is not a `character(1)`", 
  {
    expect_error(
      makeRedcapFactor(x = 1:3, 
                       coding = coding_for_radio, 
                       factors = TRUE, 
                       var_name = letters)
    )
  }
)