context("allocationTable")

rcon <- redcapConnection(url, API_KEY)

test_that("allocation table can be generated",{
    expect_silent(
      allocationTable(rcon, 
                      random = "treatment", 
                      replicates = 8,
                      block.size = 8,
                      seed.dev = 10,
                      seed.prod = 20,
                      weights = c(Control = 1, Treatment = 1))
    )
})


test_that("allocation table can be updated offline",{

  filename <- tempfile()
  on.exit(unlink(filename))
    
  meta_data20 <- exportMetaData(rcon)
  write.csv(meta_data20, filename, na = "", row.names = FALSE)

  expect_silent(
    allocationTable_offline(filename,
                            random = "treatment", 
                            replicates = 8,
                            block.size = 8,
                            seed.dev = 10,
                            seed.prod = 20,
                            weights = c(Control = 1, Treatment = 1))
  )
})


