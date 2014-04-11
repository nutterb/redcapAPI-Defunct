redcap
======

R interface to REDCap (http://www.project-redcap.org/)

KNOWN ISSUES:

- fieldToVar doesn't convert date_time variables to POSIXct class.  These fields export to R as characters
- fieldToVar doesn't do anything with time variables (HH:MM).  These fields export to R as characters

- I may need to add an additional argument to exportRecords, such as 'forceAsCharacter.'  In some applications, 
  such as ICD-9 codes, xxx.00 is not the same as xxx or xxx.0 and the characters after the decimal are informative 
  and must be retained.  Since REDCap doesn't have a 'character' validation and I believe it is fairly common
  practice to not choose 'number' validation for numbers, it is probably very difficult to distinguish between 
  numeric and character data when the field can be represented as a numeric.  I am guessing it is easier to give 
  a character vector of the fields we want forced as characters and let the user dictate the appropriate behavior.
