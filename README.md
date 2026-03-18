
<!-- README.md is generated from README.Rmd. Please edit that file -->

# saair

<!-- badges: start -->

[![R-CMD-check](https://github.com/tarias7274/saair/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tarias7274/saair/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of saair (San Antonio Air) is to assist scientists and
engineers in the Dr. Brown, Dr. Lopez-Ochoa, and related labs with
expediently and effectively studying heat and air quality in the City of
San Antonio, Texas. Formatting help code as a function enables more
robust and easy-to-maintain documentation and easy import into any code
file of any project without having to maintain disparate definitions
within several scripts or definitions across repositories or directories
with inconsistent access.

## Installation

You can install the development version of saair from
[GitHub](https://github.com/tarias7274/saair.git) with:

``` r
# install.packages("saair")
devtools::install_github("tarias7274/saair")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(saair)
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
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" alt="" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
