## Test environments

- local OS X install (R 3.4.1)
- ubuntu 12.04 (on travis-ci with older, release and devel) 
- winbuilder (older, release and devel)

## R CMD check results

There were no ERRORs or WARNINGs. 

There was 2 NOTEs on R 3.3.3 "older":

* checking DESCRIPTION meta-information ... NOTE
Authors@R field gives persons with no valid roles:
  YouGov [fnd]

This is due to usage of person("YouGov", role = "fnd") in DESCRIPTION. "fnd" was not available in R version 3.3.3.

* License components with restrictions and base license permitting such: MIT + file LICENSE

