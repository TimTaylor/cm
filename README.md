
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cm

<!-- badges: start -->
<!-- badges: end -->

Proof of concept (poc) for a contact matrix class that could underpin
various packages
(e.g. [socialmixr](https://cran.r-project.org/package=socialmixr)).

## Installation

You can install cm from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("epi-helpers/cm")
```

## Overview

### A contact matrix class

The underlying `contact_matrix` class is very simple with the only
requirements on the underlying data being that it must be a numeric
matrix of equal dimensions (i.e. an equal number of rows and columns).
Encoded with this data is an additional `from` attribute which conveys
to a user whether the matrix should be read as from “rows to columns” or
from “columns to rows”.

### Subclasses

`cm` also provides two contact matrix subclasses based on those
discussed in :

-   a `social_contact_matrix`; and
-   a `population_contact_matrix`.

The motivation for explicit classes is to avoid confusion on behalf of
package users/developers (e.g. are contacts represented as from “rows to
columns” or “columns to rows”; is this matrix symmetric with respect to
population weightings?).

The subclasses deliberately simple. For social contact matrices these
are just a subclass of a contact matrix. For population contact matrices
they require an additional attribute that encapsulates the population
weightings that give symmetry, that is , where represents the population
in group *k*.

### Example

We use data from
[socialmixr](https://cran.r-project.org/package=socialmixr) to
understand how we could utilise explicit matrix classes. Currently
construction is slightly long-winded as I still need to provide general
coercion functions for ready formed matrices (remember this is just a
poc).

``` r
library(cm)
library(socialmixr)
#> 
#> Attaching package: 'socialmixr'
#> The following object is masked from 'package:utils':
#> 
#>     cite

# first obtain data for the social contact matrix and underlying population
dat <- contact_matrix(
    polymod,
    countries = "United Kingdom",
    age.limits = c(0, 1, 5, 15)
)
#> Using POLYMOD social contact data. To cite this in a publication, use the 'cite' function
#> Removing participants that have contacts without age information. To change this behaviour, set the 'missing.contact.age' option
dat <- dat$matrix
dat2 <- contact_matrix(
    polymod,
    countries = "United Kingdom",
    age.limits = c(0, 1, 5, 15),
    symmetric = TRUE
)
#> Using POLYMOD social contact data. To cite this in a publication, use the 'cite' function
#> Removing participants that have contacts without age information. To change this behaviour, set the 'missing.contact.age' option
#> Warning in pop_age(survey.pop, part.age.group.present, ...): Not all age groups represented in population data (5-year age band).
#>   Linearly estimating age group sizes from the 5-year bands.
popn <- dat2$demography$population

# currently we require long format data
(dat <- as.data.frame(as.table(dat)))
#>    age.group contact.age.group       Freq
#> 1      [0,1)             [0,1) 0.40000000
#> 2      [1,5)             [0,1) 0.11250000
#> 3     [5,15)             [0,1) 0.02450980
#> 4        15+             [0,1) 0.03230337
#> 5      [0,1)             [1,5) 0.80000000
#> 6      [1,5)             [1,5) 1.93750000
#> 7     [5,15)             [1,5) 0.50490196
#> 8        15+             [1,5) 0.35814607
#> 9      [0,1)            [5,15) 1.26666667
#> 10     [1,5)            [5,15) 1.46250000
#> 11    [5,15)            [5,15) 7.94607843
#> 12       15+            [5,15) 1.29073034
#> 13     [0,1)               15+ 5.93333333
#> 14     [1,5)               15+ 5.45000000
#> 15    [5,15)               15+ 6.21568627
#> 16       15+               15+ 9.59410112

# create a social contact matrix
(social_mat <- with(
    dat,
    social_contact_matrix(age.group, contact.age.group, Freq, from = "rows")
))
#> From: rows
#>   To: columns
#>  dim: 4 x 4
#> 
#>             [0,1)     [1,5)   [5,15)      15+
#> [0,1)  0.40000000 0.8000000 1.266667 5.933333
#> [1,5)  0.11250000 1.9375000 1.462500 5.450000
#> [5,15) 0.02450980 0.5049020 7.946078 6.215686
#> 15+    0.03230337 0.3581461 1.290730 9.594101

# convert to a population contact matrix
(population_mat <- as_population_matrix(social_mat, population = popn))
#> From: rows
#>   To: columns
#>  dim: 4 x 4
#> 
#>             [0,1)     [1,5)   [5,15)      15+
#> [0,1)  0.40000000 0.6250000 0.764365 4.122919
#> [1,5)  0.15625000 1.9375000 1.406063 5.929829
#> [5,15) 0.07148821 0.5260153 7.946078 7.428739
#> 15+    0.05759306 0.3313352 1.109550 9.594101
#> 
#> Population:
#>    [0,1)    [1,5)   [5,15)      15+ 
#>   690734  2762936  7385454 49447627

# compare this to the data already loaded from socialmixr
(original <- dat2$matrix)
#>       contact.age.group
#>             [0,1)     [1,5)   [5,15)      15+
#>   [1,] 0.40000000 0.6250000 0.764365 4.122919
#>   [2,] 0.15625000 1.9375000 1.406063 5.929829
#>   [3,] 0.07148821 0.5260153 7.946078 7.428739
#>   [4,] 0.05759306 0.3313352 1.109550 9.594101
dimnames(original) <- NULL
all.equal(as.matrix(population_mat), original)
#> [1] TRUE
```
