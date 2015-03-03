[![Build Status](https://travis-ci.org/nutterb/redcapAPI.png?branch=master)](https://travis-ci.org/nutterb/redcapAPI)

[![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.11826.png)](http://dx.doi.org/10.5281/zenodo.11826)

redcapAPI
======

The package `redcapAPI` is an R interface to REDCap (http://www.project-redcap.org/), and is an actively developed fork of [redcap](https://github.com/vubiostat/redcap), originally created by [Jeffrey Horner](https://github.com/jeffreyhorner).

Please read the documentation on your institutions REDCap installation.

Issues may be reported at https://github.com/nutterb/redcapAPI/issues

Please consider contributing tips and clarifications to the package wiki at https://github.com/nutterb/redcapAPI/wiki

This package was developed under REDCap Version 5.8.2.  My institution is usually a little behind on updating REDCap and so some features of the API may not always be available.

The redcapDbConnection methods are underdeveloped as I don't personally have access to the REDCap database (and so am unable to test features).  If you have that kind of access, feel free to develop the redcapDbConnection methods.


### Supplemental Gists

* [Generate a Report of Fields with Missing Values in a REDCap Database Using the redcapAPI package](https://gist.github.com/nutterb/501c370418abb58bee78) (includes a version for "offline" use).
