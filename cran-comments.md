## Test environments
* Ubuntu 4.4.0-103-generic, R 3.4.3
* ubuntu 14.04.5 (on travis-ci), R 3.4.2
* win-builder devel (2018-01-04 r74054)

## R CMD check results
There is a note about the change in Maintainer.  Stephen Lane is no longer 
involved in the development of redcapAPI. Benjamin Nutter will resume the 
role of Maintainer.  Dr. Lane notified CRAN of his consent via e-mail on 
7 Jan 2018.

There were no other notes, warnings, or errors produced by CHECK.

### Response to CRAN Review

The redundant R has been removed from the title.

Package and software names in the title and description are now quoted.

The description now has a link to the REDCap Project home page.

The date field has been updated.

I have not made changes to the examples.  In principle, I agree that it would 
be better to have working examples.  Unfortunately, accessing data from REDCap requires
a unique token that is associated with the user on the institution's secure
server.  To provide a token would violate data security policies. Thus, it is not 
feasible to run meaningful examples outside of the `dontrun` block.  

I have changed functions to no longer write to the working directory.

Thank you for your thorough review.

## Downstream dependencies
There are no downstream dependencies for this package
at this time.