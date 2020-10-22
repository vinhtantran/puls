
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PULS

<!-- badges: start -->

<!-- badges: end -->

The goal of PULS is to …

## Installation

You can install the released version of PULS from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("PULS")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
remotes::install_github("vinhtantran/PULS")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(PULS)
#> Loading required package: cluster
#> Loading required package: fda
#> Loading required package: Matrix
#> 
#> Attaching package: 'fda'
#> The following object is masked from 'package:graphics':
#> 
#>     matplot
#> Loading required package: fda.usc
#> Loading required package: splines
#> Loading required package: MASS
#> Loading required package: mgcv
#> Loading required package: nlme
#> This is mgcv 1.8-33. For overview type 'help("mgcv-package")'.
#> ----------------------------------------------------------------------------------
#>  Functional Data Analysis and Utilities for Statistical Computing
#>  fda.usc version 2.0.2 (built on 2020-02-17) is now loaded
#>  fda.usc is running sequentially usign foreach package
#>  Please, execute ops.fda.usc() once to run in local parallel mode
#>  Deprecated functions: min.basis, min.np, anova.hetero, anova.onefactor, anova.RPm
#>  New functions: optim.basis, optim.np, fanova.hetero, fanova.onefactor, fanova.RPm
#> ----------------------------------------------------------------------------------
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/master/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
