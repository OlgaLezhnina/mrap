
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mrap

<!-- badges: start -->
<!-- badges: end -->

The goal of mrap is to provide wrapper functions to reduce the user’s
effort in writing machine-readable data.

## Installation

You can install the development version of mrap from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("OlgaLezhnina/mrap_R")
```

## Example

For instance, you have results of ANOVA on Iris data. On the help page,
you see that the group_comparison schema should be used. Instead of
writing the data manually with dtreg, use the group_comparison function
from mrap. Arguments code_string, input_data, and test_results should be
provided.

``` r
library(mrap)
## get the data
attach(iris)
virginica <- iris[iris$Species== "virginica", ]
versicolor <- iris[iris$Species== "versicolor", ]
## run the test
my_anova <- stats::aov(Petal.Length ~ Species, iris)
## write the results as a data frame
my_results <- summary(my_anova)[[1]]
## use the relevant wrapper to write an instance
my_instance <-
  mrap::group_comparison("stats::aov(Petal.Length ~ Species, data = iris)",
                         iris,
                         my_results)
## make any changes to the instance
my_instance$label <- "ANOVA for Iris petal length"
## write the result as JSON-LD using dtreg
my_json <- dtreg::to_jsonld(my_instance)
```
