context("deleteRecords")

rcon <- redcapConnection(url = url, token = API_KEY)

test_that("records can be deleted",{
  rec <- exportRecords(rcon)
  rows <- nrow(rec)
  
  rec <- rbind(rec[1,], rec[1,])
  rec$record_id <- c("delete.me", "delete.too")
  expect_error(importRecords(rcon = rcon, data=rec),NA)
  expect_error(deleteRecords(rcon, c("delete.me", "delete.too")), NA)
  
  rec <- exportRecords(rcon)
  
  expect_equal(nrow(rec), rows)
})

test_that("error when trying to delete something that doesn't exist",{
  expect_error(deleteRecords(rcon, c("thisdoesnotexist")), "thisdoesnotexist")
})

test_that("arm restrictions are honored",{
  rec <- exportRecords(rcon)
  rows <- nrow(rec)
  
  rec <- rec[1:2,]
  rec$record_id <- c("delete.me", "delete.too")
  
  rec$redcap_event_name <- c("event_1_arm_1", "event_1_arm_2")
  expect_error(importRecords(rcon = rcon, data=rec), NA)
  
  # Expect deleting from proper arm to work
  expect_error(deleteRecords(rcon, c("delete.me"), arms=1), NA)
  
  # Expect an error when deleting from wrong arm
  expect_error(deleteRecords(rcon, c("delete.too"), arms=1), "delete.too")
  
  rec <- exportRecords(rcon)
  expect_equal(nrow(rec), rows+1)
  
  # Delete from proper arm
  expect_error(deleteRecords(rcon, "delete.too", arms=2), NA)
})
