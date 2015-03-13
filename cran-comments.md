## Test environments
* local Windows install (devel)
* ubuntu 12.04 (on travis-ci), R 3.1.2
* win-builder (devel and release)

## R CMD check results
In the previous CRAN submission, the NOTEs below were returned by CRAN. 
We apologize for not catching them earlier.  Our oversight was not upgrading
our version of RTools before submitting the package.

1. **The title field should be in title case...**: The title field has been rewritten appropriately 
2. **Found the following apparent S3 methods exported but not registered**: We have altered the documentation to properly register the methods.

After rerunning the checks, we have only one note left regarding the new package maintainer.  Mr. Nutter will submit his written consent to CRAN@R-project.org separately.

There was 1 NOTE:

* A note was given about a new maintainer.  
  Stephen Lane will be taking over as the maintainer
  due to a change in Mr. Nutter's employment.

## Downstream dependencies
There are no downstream dependencies for this package
at this time.