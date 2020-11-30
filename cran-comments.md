## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
> On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... NOTE
  
  New submission
  Maintainer: 'Tan Tran <vinhtantran@gmail.com>'

> On ubuntu-gcc-release (r-release)
  checking package dependencies ... NOTE
  Packages suggested but not available for checking: 'covr', 'vdiffr'

0 errors √ | 0 warnings √ | 2 notes x

Explanations:
* New submission
* covr and vdiffr are used for unit testing of plotting functions. They are monitoring packages and shouldn't cause R CMD check failures on the CRAN machines.
* Rewrite the DESCRIPTION's Description field to address reviewer's comment on not to start it with the package name. There is currently no references in the Description.
