redcap
======

R interface to REDCap (http://www.project-redcap.org/)

KNOWN ISSUES:

- Validate import doesn't know how to handle time (HH:MM and MM:SS) formats

- I may need to add an additional argument to exportRecords, such as 'forceAsCharacter.'  In some applications, 
  such as ICD-9 codes, xxx.00 is not the same as xxx or xxx.0 and the characters after the decimal are informative 
  and must be retained.  Since REDCap doesn't have a 'character' validation and I believe it is fairly common
  practice to not choose 'number' validation for numbers, it is probably very difficult to distinguish between 
  numeric and character data when the field can be represented as a numeric.  I am guessing it is easier to give 
  a character vector of the fields we want forced as characters and let the user dictate the appropriate behavior.

- exportUsers only has an API interface.  This should probably written as a method such as exportUsers.ApiConnection and
  exportUsers.DbConnection

- exportUsers doesn't always apply the permissions formats correctly.  It seems to work on databases created more recently.  Perhaps a version issue? (doubtful)

- Factors created during exportRecords() have additional attributes 'redcapLevels' and 'redcapLabels' which correspond to the coded and labeled values for the factors, respectively.  These would be useful for coding and uncoding factors, as desired.  For example, uncoding Alive/Dead as 0/1.  A utility function should be written to access the attributes for recoding, simplifying the work for the user.

- the attribute 'redcapLevels' is not assigned to vectors created in exportRecords() where factors=TRUE. This seems to only apply to the drop down variable type.

- Calculated fields are not recalculated when exported.  They should be recalculated after export.  I hope to automate this process soon.

- All calls to the API are made in a single call.  If your server is small, it could tie up the server from other users.  I hope to incorporate some of REDCapR's features for breaking an export into smaller batches in the future.

- The current design of the package results in many API calls, especially to retrieve the meta data.  Including an argument that allows the meta data to be passed to the function in place of an additional call to the API could reduce the number of records in the audit log, if you care about that sort of thing.
