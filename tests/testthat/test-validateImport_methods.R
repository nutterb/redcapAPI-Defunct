# validate_import_checkbox ------------------------------------------

test_that(
  "0, 1, Unchecked, Checked, '', NA all pass", 
  {
    test_checkbox <- c("0", "1", 
                       "Unchecked", "UNCHECKED", "UnChEcKeD", "unchecked", 
                       "Checked", "CHECKED", "ChEcKeD", "checked", 
                       "", NA_character_)
    check_define <- "check1, Guitar | check2, Lute | check3 , Harp "
    expect_equal(
      validate_import_checkbox(test_checkbox, 
                               field_name = "checkbox___check1", 
                               field_choice = check_define, 
                               logfile = ""), 
      c("0", "1", 
        "0", "0", "0", "0", 
        "1", "1", "1", "1", 
        "0", NA_character_)
    )
  }
)


test_that(
  "0, 1, NA all pass (numeric)", 
  {
    test_checkbox <- c(0, 1, NA_real_)
    check_define <- "check1, Guitar | check2, Lute | check3 , Harp "
    expect_equal(
      validate_import_checkbox(test_checkbox, 
                               field_name = "checkbox___check1", 
                               field_choice = check_define, 
                               logfile = ""), 
      c("0", "1", NA_character_)
    )
  }
)


test_that(
  "codes and labels pass", 
  {
    test_checkbox <- c("check1", "Guitar")
    check_define <- "check1, Guitar | check2, Lute | check3 , Harp "
    expect_equal(
      validate_import_checkbox(test_checkbox, 
                               field_name = "checkbox___check1", 
                               field_choice = check_define, 
                               logfile = ""), 
      c("1", "1")
    )
  }
)

test_that(
  "codes and labels pass (second option)", 
  {
    test_checkbox <- c("check2", "Lute")
    check_define <- "check1, Guitar | check2, Lute | check3 , Harp "
    expect_equal(
      validate_import_checkbox(test_checkbox, 
                               field_name = "checkbox___check2", 
                               field_choice = check_define, 
                               logfile = ""), 
      c("1", "1")
    )
  }
)


test_that(
  "0 code or 0 label returns 0", 
  {
    test_checkbox <- c("0")
    check_define <- "0, 0 | 1, 1"
    expect_equal(
      validate_import_checkbox(test_checkbox, 
                               field_name = "checkbox___0", 
                               field_choice = check_define, 
                               logfile = ""), 
      c("0")
    )
  }
)


test_that(
  "unmapped values produce a message", 
  {
    local_reproducible_output(width = 200)
    test_checkbox <- c("check_lute")
    check_define <- "check1, Guitar | check2, Lute | check3 , Harp "
    expect_message(
      validate_import_checkbox(test_checkbox, 
                               field_name = "checkbox___check2", 
                               field_choice = check_define, 
                               logfile = ""), 
      "must be one of '0', '1', 'Checked', 'Unchecked', 'check2', 'Lute', ''"
    )
  }
)
