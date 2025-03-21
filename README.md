
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mrap

<!-- badges: start -->
<!-- badges: end -->

The goal of mrap is to provide wrapper functions to reduce the user’s
effort in writing machine-reusable data.

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
from mrap.

``` r
library(mrap)
## get the data
attach(iris)
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

Such wrappers as group_comparison have three arguments: code_string,
input_data, and test_results.

- Argument code_string should be a string(in R, a character vector) and
  include package::function notation.

- In code_string, the data label should be specified as “data = iris”,
  not just “iris”. Not specifying the data label is acceptable, please
  add it manually to the instance.

- In code_string, target variable is recognized by our wrappers before
  the ~ sign in the formula (strings a, b, and c) or when explicitly
  specified in two or more vectors (string d). Otherwise (strings e
  or f) you will get a warning reminding to add the target name
  manually.

``` r
## (a) "package::function(Petal.Length ~ Species), data = iris"
## (b) "package::function(iris$Petal.Length ~ iris$Species), data = iris"
## (c) "package::function(cbind(Petal.Length, Petal.Width) ~ Species), data = iris"
## (d) "package::function(setosa$Petal.Length, virginica$Petal.Length, versicolor$Petal.Length)"
## (e) "package::function(one_vector, another_vector)"
## (f) "package::function(data_1$one_vector, data_2$another_vector)"
```
