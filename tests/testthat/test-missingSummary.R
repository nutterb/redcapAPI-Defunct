context("missingSummary")

rcon <- redcapConnection(url = url, 
                         token = API_KEY)

# Tests to perform
# * return an error if rcon is not a redcapConnection object
# * return an error if excludeMissingForms is not logical
# * return an error if excludeMissingForms is not length 1.
# * return an error if exportRecordsArgs is not a list. 
# * return an error if exportRecordsArgs is not a named list.
# * return an error if fixed_fields is not a character vector
#
# Needs to be able to identify missing values where
# * missing value with no branching logic
# * missing value with branching logic from one field (non-checkbox)
# * missing value with branching logic from a checkbox field
# * missing value with branching logic from two fields using an AND conjunction
# * missing value with branching logic from two fields using an OR conjuction
# * missing value with branching logic from two fields where one is a checkbox field
# * missing value with branching logic from three fields using both AND and OR conjunctions
# * missing value with branching logic from a field using an inequality
# * missing values identified when excludeMissingForms = FALSE
# Desired output when `excludeMissingForms = TRUE` ------------------

DesiredOutput <- 
  structure(
    list(
      record_id = structure(c("10", "11", "12", "13", "14", "15", "16", "17", "18", "19", 
                              "20", "21", "22", "23", "24", "25", "26", "27", "28", "29"), 
                            label = "Record ID", 
                            class = c("labelled", "character")),
      redcap_event_name = c("event_1_arm_1", "event_1_arm_1", "event_1_arm_1", "event_1_arm_1", 
                            "event_1_arm_1", "event_1_arm_1", "event_1_arm_1", "event_1_arm_1", 
                            "event_1_arm_1", "event_1_arm_1", "event_1_arm_1", "event_1_arm_1", 
                            "event_1_arm_1", "event_1_arm_1", "event_1_arm_1", "event_1_arm_1", 
                            "event_1_arm_1", "event_1_arm_1", "event_1_arm_1", "event_1_arm_1"),
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
    local_reproducible_output(width = 200)
    expect_error(
      missingSummary(mtcars),
      "no applicable method for 'missingSummary'"
    )
  }
)

test_that(
  "Return an error if `excludeMissingForms` is not logical", 
  {
    local_reproducible_output(width = 200)
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
    local_reproducible_output(width = 200)
    expect_error(
      missingSummary(rcon, 
                     excludeMissingForms = c(TRUE, FALSE)), 
      "Variable 'excludeMissingForms': Must have length 1, but has length 2."
    )
  }
)

test_that(
  "Return an error if exportRecordsArgs is not a list.",
  # * return an error if exportRecordsArgs is not a named list.
  # * return an error if exportRecordsArgs has elements that are not arguments to exportRecords.
  {
    local_reproducible_output(width = 200)
    expect_error(
      missingSummary(rcon, 
                     exportRecordsArgs = "branching_logic"), 
      "'exportRecordsArgs': Must be of type 'list'"
    )
  }
)

test_that(
  "Return an error if exportRecordsArgs is not a named list.",
  {
    local_reproducible_output(width = 200)
    expect_error(
      missingSummary(rcon, 
                     exportRecordsArgs = list("branching_logic")), 
      "'exportRecordsArgs': Must have names"
    )
  }
)

test_that(
  "Return an error if `fixed_fields` is not a character vector",
  {
    local_reproducible_output(width = 200)
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
    local_reproducible_output(width = 200)
    expect_identical(
      missingSummary(rcon,
                     exportRecordsArgs = list(fields = "record_id", 
                                              records = as.character(10:29), 
                                              forms = "branching_logic")), 
      DesiredOutput
    )
  }
)
