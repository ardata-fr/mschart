## Test environments

- local OS X install (R 3.4.2)
- ubuntu 12.04 (on travis-ci with older, release and devel) 
- winbuilder (older, release and devel)

## R CMD check results

There were no ERROR or WARNING. 

There was 2 NOTEs on R 3.3.3 "older":

* checking DESCRIPTION meta-information ... NOTE
Authors@R field gives persons with no valid roles:
  YouGov [fnd]
Author field differs from that derived from Authors@R
  Author:    'David Gohel [aut, cre], YouGov [fnd]'
  Authors@R: 'David Gohel [aut, cre]'

This is due to usage of person("YouGov", role = "fnd") in DESCRIPTION. "fnd" was not available in R version 3.3.3.

* License components with restrictions and base license permitting such: MIT + file LICENSE

