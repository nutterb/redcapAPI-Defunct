context("allocationTable.R")

if (file.exists("local-token.Rdata")){
  load("local-token.Rdata")
} else {
  url <- "https://redcap.notaplace.net/redcap/api/"
  token_case_01 <- "NOTaREALtoken1234567890123456789"
  token_case_20 <- token_case_01
}

rcon <- redcapConnection(url, token_case_20)

test_that(
  "Allocation Table can be generated",
  {
    expect_silent(
      allocationTable(rcon, 
                      random = "treatment", 
                      replicates = 8,
                      block.size = 8,
                      seed.dev = 10,
                      seed.prod = 20,
                      weights = c(Control = 1, Treatment = 1))
    )
  }
)


test_that(
  "Allocation Table in offline style",
  {
    meta_data20 <- exportMetaData(rcon)
    write.csv(meta_data20, "MetaDataForAllocation.csv", na = "", row.names = FALSE)
    on.exit(file.remove("MetaDataForAllocation.csv"))
    
    expect_silent(
      allocationTable_offline("MetaDataForAllocation.csv",
                              random = "treatment", 
                              replicates = 8,
                              block.size = 8,
                              seed.dev = 10,
                              seed.prod = 20,
                              weights = c(Control = 1, Treatment = 1))
    )
  }
)


