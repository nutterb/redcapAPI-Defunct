

[![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.11826.png)](http://dx.doi.org/10.5281/zenodo.11826)
![](http://cranlogs.r-pkg.org/badges/grand-total/redcapAPI)

redcapAPI
======

*NOTE*: Ownership transfer of this package to VUMC Biostatistics is complete.
Existing
ickets in the older git repo will be transitioned over the next couple months.

The research community owes a big thanks to [Benjamin Nutter](https://github.com/nutterb/redcapAPI)
for his years of service keeping this package current.

The package `redcapAPI` is an R interface to REDCap (http://www.project-redcap.org/), originally created by [Jeffrey Horner](https://github.com/jeffreyhorner).

Please read the documentation on your institutions REDCap installation.

Issues may be reported at [Issues](https://github.com/vubiostat/redcapAPI/issues)

This package was developed under REDCap Version 13.2.4. Institutions can be a little behind on updating REDCap and so some features of the API may not always work.

### Supplemental Gists

* [Generate a Report of Fields with Missing Values in a REDCap Database Using the redcapAPI package](https://gist.github.com/nutterb/501c370418abb58bee78) (includes a version for "offline" use).

### Transfer breakage to be fixed

[![Build Status](https://travis-ci.org/nutterb/redcapAPI.png?branch=master)](https://travis-ci.org/nutterb/redcapAPI)
[![Coverage Status](https://coveralls.io/repos/github/nutterb/redcapAPI/badge.svg?branch=master)](https://coveralls.io/github/nutterb/redcapAPI?branch=master)

*Possible deprecation*. The redcapDbConnection methods are underdeveloped as I don't personally have access to the REDCap database (and so am unable to test features).  If you have that kind of access, feel free to develop the redcapDbConnection methods.
