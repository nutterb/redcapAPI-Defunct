library(RCurl)
result <- postForm(
  uri='https://redcap.ccf.org/redcap/api/',
  token='3B458F8E065E6E873DC55DE6D51203BB',
  content='generateNextRecordName'
)
print(result)


my.grNAME <- 
  httr::POST(url = "https://redcap.ccf.org/redcap/api/",
             body = list(token = "3B458F8E065E6E873DC55DE6D51203BB",
                         content='generateNextRecordName'))

rawToChar(my.grNAME$content)