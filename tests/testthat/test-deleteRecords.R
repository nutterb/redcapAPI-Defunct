context("deleteRecords")

rcon <- redcapConnection(url = url, token = API_KEY)

test_that("records can be deleted",{
  rec <- exportRecords(rcon)
  
  rows <- nrow(rec)
  
  rec <- rec[1,]
  rec$record_id <- "delete.me"
  importRecords(rcon = rcon, data=rec)
  
  deleteRecords(rcon, "delete.me")
  
  rec <- exportRecords(rcon)
  
  expect_equal(nrow(rec), rows)
})
