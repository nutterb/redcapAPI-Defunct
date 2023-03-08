context("deleteRecords")

rcon <- redcapConnection(url = url, token = API_KEY)

test_that("records can be deleted",{
  rec <- exportRecords(rcon)
  
  rows <- nrow(rec)
  rec$record_id <- "delete.me"
  importRecords(rcon = rcon, data=rec)
  
  deleteRecords(rcon, rec$record_id)
  
  rec <- exportRecords(rcon)
  
  expect_equal(nrow(rec), rows)
})
