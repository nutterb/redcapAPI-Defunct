## Test environments
* local Linux install (R-3.4.4)
* remote Linux install (R-3.5.0; ubuntu 4.8.4-2ubuntu1~14.04.3)
* win-builder (release R 3.5.1)
* win-builder (2018-07-31 r75040)

## R CMD check results

This release provides a handful of bug fixes following the previous release.

There were no notes, warnings, or errors produced by CHECK.

## A note on testing

In response to a previous observation from CRAN maintainers, we
have begun writing unit tests for this package.  Due to security concerns
around disclosing servers and tokens, we have deliberately prevented tests
from running on CRAN, and only conduct the tests locally.  We did, however, 
want testing code to be available to other users, so we have included the 
tests in the code so that users may run them independently.   

## Downstream dependencies
There are no downstream dependencies for this package
at this time.