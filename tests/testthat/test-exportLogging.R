context("exportLogging.R")

rcon <- redcapConnection(url = url, 
                         token = API_KEY)

FullLog <- exportLogging(rcon)

test_that(
  "Return an error when rcon is not a redcapConnection object", 
  {
    local_reproducible_output(width = 200)
    expect_error(
      exportLogging(rcon = "not an rcon"), 
      "no applicable method for 'exportLogging'"
    )
  }
)


test_that(
  "Return an error when log_type is not one of event types",
  {
    expect_error(
      exportLogging(rcon, 
                    logtype = "abc"), 
      "Variable 'logtype': Must be element of set"
    )
  }
)


test_that(
  "Return an error when user is not character", 
  {
    expect_error(
      exportLogging(rcon, 
                    user = 1:2), 
      "Variable 'user': Must be of type 'character'"
    )
  }
)


test_that(
  "Return an error when dag is not a character", 
  {
    expect_error(
      exportLogging(rcon, 
                    dag = 1:2), 
      "Variable 'dag': Must be of type 'character'"
    )
  }
)


test_that(
  "Return an error when beginTime is not POSIXct", 
  {
    expect_error(
      exportLogging(rcon, 
                    beginTime = "2023-01-01 00:00:00"), 
      "Variable 'beginTime': Must be of type 'POSIXct'"
    )
  }
)


test_that(
  "Return an error when beginTime has length > 1", 
  {
    expect_error(
      exportLogging(rcon, 
                    beginTime = rep(Sys.time(), 2)), 
      "Variable 'beginTime': Must have length <= 1"
    )
  }
)


test_that(
  "Return an error when endTime is not POSIXct", 
  {
    expect_error(
      exportLogging(rcon, 
                    endTime = "2023-01-01 00:00:00"), 
      "Variable 'endTime': Must be of type 'POSIXct'"
    )
  }
)


test_that(
  "Return an error when endTime has length > 1", 
  {
    expect_error(
      exportLogging(rcon, 
                    endTime = rep(Sys.time(), 2)), 
      "Variable 'endTime': Must have length <= 1"
    )
  }
)


test_that(
  "Logs are returned when all arguments are default", 
  {
    checkmate::expect_data_frame(
      exportLogging(rcon)
    )
  }
)


test_that(
  "Logs are returned for logtype = 'export'",
  {
    Logs <- exportLogging(rcon, 
                          logtype = "export")
    
    all_export_record <- all(grepl("export", Logs$action))
    expect_true(all_export_record)
  }
)


test_that(
  "Logs are returned for logtype = 'manage'",
  {
    Logs <- exportLogging(rcon, 
                          logtype = "manage")
    
    all_manage_record <- all(grepl("Manage", Logs$action))
    expect_true(all_manage_record)
  }
)


test_that(
  "Logs are returned for logtype = 'user'",
  {
    Logs <- exportLogging(rcon, 
                          logtype = "user")
    # API token creations are classified under "Manage"
    all_user_record <- all(grepl("(user|Manage)", Logs$action))
    expect_true(all_user_record)
  }
)


test_that(
  "Logs are returned for logtype = 'record'",
  {
    Logs <- exportLogging(rcon, 
                          logtype = "record")
    all_record_record <- all(grepl("(Manage|(R|r)ecord)", Logs$action))
    expect_true(all_record_record)
  }
)


test_that(
  "Logs are returned for logtype = 'record_add'",
  {
    Logs <- exportLogging(rcon, 
                          logtype = "record_add")
    all_add_record <- all(grepl("Create record", Logs$action))
    expect_true(all_add_record)
  }
)


test_that(
  "Logs are returned for logtype = 'record_edit'",
  {
    Logs <- exportLogging(rcon, 
                          logtype = "record_edit")
    all_edit_record <- all(grepl("Update record", Logs$action))
    expect_true(all_edit_record)
  }
)


test_that(
  "Logs are returned for logtype = 'record_delete", 
  {
    Logs <- exportLogging(rcon, 
                          logtype = "record_delete")
    all_delete_record <- all(grepl("Delete record", Logs$action))
    expect_true(all_delete_record)
  }
)


test_that(
  "Logs are returned for logtype = 'lock_record'", 
  {
    Logs <- exportLogging(rcon = rcon, 
                          logtype = "lock_record")
    all_lock_record <- all(grepl("Lock[/]Unlock", Logs$action))
    expect_true(all_lock_record)
  }
)


test_that(
  "Logs are returned for logtype = 'page_view'", 
  {
    Logs <- exportLogging(rcon = rcon, 
                          logtype = "page_view")
    all_page_view_record <- all(grepl("Page View", Logs$action))
    expect_true(all_page_view_record)
  }
)


test_that(
  "Logs are returned for an existing user", 
  {
    user_in_log <- unique(FullLog$username)
    user_for_test <- sample(user_in_log, 1)
    skip_if(length(user_for_test) == 0)
    
    Logs <- exportLogging(rcon, 
                          user = user_for_test)
    all_user <- all(Logs$user == user_for_test)
    expect_true(all_user)
  }
)


test_that(
  "Empty logs are returned for a non-existing user", 
  {
    Logs <- exportLogging(rcon, 
                          user = "this user doesn't exist")
    expect_true(nrow(Logs) == 0)
  }
)



test_that(
  "Logs are returned for an existing record", 
  {
    records <- FullLog$record
    records <- records[!is.na(records)]
    records <- trimws(records)
    records <- unique(records)
    
    record_for_test <- sample(records, 1)
    
    Logs <- exportLogging(rcon, 
                          record = record_for_test)
    all_record <- all(Logs$record == record_for_test)
    expect_true(all_record)
  }
)


test_that(
  "Empty logs are returned for a non-existing user", 
  {
    records <- FullLog$record
    records <- records[!is.na(records)]
    records <- trimws(records)
    records <- unique(records)
    # by appending abc, to all of the existing record, we guarantee
    # that the record used doesn't exist
    records <- sprintf("%s-abc", 
                       records)
    
    record_for_test <- sample(records, 1)
    
    record_for_test <- sprintf("%s-abc", 
                               record_for_test)
    
    Logs <- exportLogging(rcon, 
                          record = record_for_test)
    
    expect_true(nrow(Logs) == 0)
  }
)


test_that(
  "Logs are returned for an existing Data Access Group", 
  {
    skip_if(TRUE, 
            "At the time of writing, DAGs aren't set up in the test project. This test is skipped")
    
    dag_for_test <- "ENTER DAG NAME HERE"
    Logs <- exportLogging(rcon, 
                          dag = dag_for_test)
    all_dag <- TRUE # write condition for verification here
    expect_true(all_dag)
  }
)

test_that(
  "Empty logs are returned for a non-existing data access group", 
  {
    skip_if(TRUE, 
            "At the time of writing, DAGs aren't set up in the test project. This test is skipped")
  
    dag_for_test <- "GarfieldLikesLasagna" # I'm going to go out on a limb and hope that we never define such a data access group
    Logs <- exportLogging(rcon, 
                          dag = dag_for_test)
    all_dag <- TRUE # write condition for verification here
    
    expect_true(nrow(Logs) == 0)
  }
)


test_that(
  "Logs are returned after a beginTime", 
  {
    times <- FullLog$timestamp
    times <- sort(times)
    index <- seq_along(times)
    index <- median(index)
    
    time_this_test <- times[index]
    
    Logs <- exportRecords(rcon, 
                          beginTime = time_this_test)
    
    all_after_begin <- all(Logs$timestamp >= time_this_test)
    expect_true(all_after_begin)
  }
)

test_that(
  "Logs are returned before an endTime", 
  {
    times <- FullLog$timestamp
    times <- sort(times)
    index <- seq_along(times)
    index <- median(index)
    
    time_this_test <- times[index]
    
    Logs <- exportRecords(rcon, 
                          endTime = time_this_test)
    
    all_before_end <- all(Logs$timestamp <= time_this_test)
    expect_true(all_before_end)
  }
)

