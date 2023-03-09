context("deleteRecords")

rcon <- redcapConnection(url = url, token = API_KEY)

test_that("records can be deleted",{
  rec <- exportRecords(rcon)
  rows <- nrow(rec)
  
  rec <- rbind(rec[1,], rec[1,])
  rec$record_id <- c("delete.me", "delete.too")
  importRecords(rcon = rcon, data=rec)
  
  deleteRecords(rcon, c("delete.me", "delete.too"))
  
  rec <- exportRecords(rcon)
  
  expect_equal(nrow(rec), rows)
})
