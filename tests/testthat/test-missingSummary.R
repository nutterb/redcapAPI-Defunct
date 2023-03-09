context("missingSummary")

rcon <- redcapConnection(url = url, 
                         token = API_KEY_TEST_BRANCHING_LOGIC)

# Desired output when `excludeMissingForms = TRUE` ------------------

DesiredOutput <- 
  structure(
    list(
      record_id = structure(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", 
                              "11", "12", "13", "14", "15", "16", "17", "18", "19", "20"), 
                            label = "Record ID", 
                            class = c("labelled", "character")), 
      n_missing = c(1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 5), 
      missing = c("no_prereq_number", "", "", "", "one_prereq_non_checkbox", "", "one_prereq_checkbox", 
                  "", "two_prereq_and", "", "two_prereq_or", "", "two_prereq_and_one_check", 
                  "two_prereq_and_one_check", "", "three_prereq_andor", "", "one_prereq_inequality", 
                  "", "prereq_radio, prereq_number, prereq_date, prereq_yesno, no_prereq_number")
    ), 
    row.names = c(NA, -20L), 
    class = "data.frame"
  )

# Test for argument validation --------------------------------------

test_that(
  "Return an error if `rcon` is not a redcapConnection object",
  {
    expect_error(
      missingSummary(mtcars),
      "no applicable method for 'missingSummary'"
    )
  }
)

test_that(
  "Return an error if `excludeMissingForms` is not logical", 
  {
    expect_error(
      missingSummary(rcon, 
                     excludeMissingForms = "TRUE"), 
      "Variable 'excludeMissingForms': Must be of type 'logical'"
    )
  }
)

test_that(
  "Return an error if `excludeMissingForms` is not length 1", 
  {
    expect_error(
      missingSummary(rcon, 
                     excludeMissingForms = c(TRUE, FALSE)), 
      "Variable 'excludeMissingForms': Must have length 1, but has length 2."
    )
  }
)

test_that(
  "Return an error if `batch.size` is not integerish",
  {
    expect_error(
      missingSummary(rcon, 
                     batch.size = 1.3), 
      "Variable 'batch.size': Must be of type 'integerish'"
    )
  }
)

test_that(
  "Return an error if `batch.size` is not length 1",
  {
    expect_error(
      missingSummary(rcon, 
                     batch.size = c(-1, -2)),
      "Variable 'batch.size': Must have length 1, but has length 2"
    )
  }
)

test_that(
  "Return an error if `fixed_fields` is not a character vector",
  {
    expect_error(
      missingSummary(rcon, 
                     fixed_fields = 1:3), 
      "Variable 'fixed_fields': Must be of type 'character'"
    )
  }
)

# Test functionality ------------------------------------------------

test_that(
  "Missing values are correctly identified around branching logic",
  {
    expect_identical(
      missingSummary(rcon), 
      DesiredOutput
    )
  }
)
