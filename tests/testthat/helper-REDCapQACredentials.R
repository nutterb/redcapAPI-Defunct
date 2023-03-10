  #############################################################################
 #
# To properly do a full integration test a REDCap
# instance is required that matches the intended test cases.
#
# This helper will pull the required values from a keyring
# similar to how `rccola` works for securitiy purposes.
# This package cannot depend on `rccola` without creating 
# a circular dependency so minimal code is copyied locally
# 
# To duplicate our test database see: inst/extdata
#
# Create a keyring with
# 
#   options(keyring_backend=keyring::backend_file) # Because MACOS is so irritating
#   keyring::keyring_create('TestRedcapAPI', 'YOURPASSWORDHERE')
#   keyring::key_set_with_value('TestRedcapAPI', username='TestRedcapAPI', keyring='TestRedcapAPI', password='YOURAPIKEYHERE')
# To remove invalid password/API_KEY
#   keyring::key_delete('TestRedcapAPI', 'TestRedcapAPI', 'TestRedcapAPI')

options(keyring_backend=keyring::backend_file) # Because MACOS is so irritating 
url <- "https://redcap.vanderbilt.edu/api/" # Our institutions REDCap instance
if(!exists("password")){
  password <- getPass::getPass("Enter Password for keyring 'testRedcapAPI'")
}

if(!exists("API_KEY")){
  keyring::keyring_unlock('TestRedcapAPI', password)
  API_KEY <- keyring::key_get('TestRedcapAPI', 'TestRedcapAPI', 'TestRedcapAPI')
}
