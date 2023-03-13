context("deleteRecords")

rcon <- redcapConnection(url = url, token = API_KEY)

test_that("records can be deleted",{
  rec <- exportRecords(rcon)
  rows <- nrow(rec)
  
  rec <- rbind(rec[1,], rec[1,])
  rec$record_id <- c("delete.me", "delete.too")
  expect_success(importRecords(rcon = rcon, data=rec))
  expect_success(deleteRecords(rcon, c("delete.me", "delete.too")), NA)
  
  rec <- exportRecords(rcon)
  
  expect_equal(nrow(rec), rows)
})

test_that("error when trying to delete something that doesn't exist",{
  expect_error(deleteRecords(rcon, c("thisdoesnotexist")), "thisdoesnotexist")
})

test_that("arm restrictions are honored",{
  rec <- exportRecords(rcon)
  rows <- nrow(rec)
  
  rec <- rbind(rec[1,], rec[1,])
  rec$record_id <- c("delete.me", "delete.too")
  
  # FIXME: Remove when importRecords has better validation or override
  # This is an overzealous validation for an instrument not used on event_1
  #rec$prereq_date[2] <- "2023-03-08 CST"
  #rec$one_prereq_checkbox[2] <- "2023-03-08 CST"
  #rec$branching_logic_complete <- 'Incomplete'
  # FIXME: End
  
  rec$redcap_event_name <- c("event_1_arm_1", "event_1_arm_2")
  expect_success(importRecords(rcon = rcon, data=rec))
  expect_success(deleteRecords(rcon, c("delete.me"), arms=1))
  expect_error(deleteRecords(rcon, c("delete.too"), arms = 1), "delete.too")
  
  rec <- exportRecords(rcon)
  expect_equal(nrow(rec), rows+1)
  
  # Cleanup
  deleteRecords(rcon, c("delete.too"))
})