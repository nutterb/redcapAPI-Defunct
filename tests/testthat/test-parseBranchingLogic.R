context("parseBranchingLogic")


test_that(
  "Test that equality and inequality is parsed correctly", 
  {
    expect_identical(
      parseBranchingLogic(c("[value] = '5'",
                            "[value] < '5'", 
                            "[value] <= '5'", 
                            "[value] > '5'", 
                            "[value] >= '5'", 
                            "[value] <> '5'", 
                            "[value] != '5'")), 
      list(expression(value == "5"), 
           expression(value < "5"), 
           expression(value <= "5"), 
           expression(value > "5"), 
           expression(value >= "5"), 
           expression(value != "5"), 
           expression(value != "5"))
    )
  }
)

test_that(
  "Test that upper case checkbox labels are converted to lower case in the field name",
  {
    expect_identical(
      parseBranchingLogic("checkbox(UPPER) = '1'"), 
      list(expression(checkbox___upper == '1'))
    )
  }
)

test_that(
  "Test that upper/mixed case labels for radio boxes (or drop downs) are preserved in comparison", 
  {
    expect_identical(
      parseBranchingLogic(c("radio_UPPER = 'ABC'", 
                            "radio_lower = 'abc'", 
                            "radio_MIXed = 'Abc'")), 
      list(expression(radio_UPPER == 'ABC'),
           expression(radio_lower == 'abc'), 
           expression(radio_MIXed == 'Abc'))
    )
  }
)
