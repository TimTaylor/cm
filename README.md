
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cm

<!-- badges: start -->
<!-- badges: end -->

WIP on proof of concept (poc) for a contact matrix class that could
underpin various packages
(e.g. [socialmixr](https://cran.r-project.org/package=socialmixr)).

## Installation

You can install cm from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("TimTaylor/cm")
```

## Example

``` r
library(tibble)
library(cm)

dat <- tribble(
        ~age,  ~gender,    ~age,  ~gender, ~value,
     "young",   "male",   "old", "female",     1L,
     "young", "female",   "old", "female",     2L,
       "old", "female", "young", "female",     2L
)

from <- .subset(dat, 1:2)
to <- .subset(dat, 3:4)
value <- .subset2(dat, 5L)

contact_matrix(from, to, value)
#> dimensions: 4 x 4
#> groups:     age, gender
#> 
#>                  [old / female] [old / male] [young / female] [young / male]
#> [old / female]                0            0                2              0
#> [old / male]                  0            0                0              0
#> [young / female]              2            0                0              0
#> [young / male]                1            0                0              0
```
