
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mrap

<!-- badges: start -->
<!-- badges: end -->

The goal of mrap is to provide wrapper functions to reduce the user’s
effort in writing machine-readable data with the [dtreg
package](https://cran.r-project.org/package=dtreg). Analytical wrappers
facilitate writing the data with the schemata used by [TIB Knowledge
Loom](https://knowledgeloom.tib.eu/). All-in-one wrappers (currently,
`mrap::stats_aov`) will cover functions from `stats` and other
well-known packages.

## Installation

The easiest way is to install mrap from CRAN:

``` r
install.packages("mrap")
```

You can install the development version of mrap with:

``` r
# install.packages("devtools")
library(devtools)
# devtools::install_gitlab("TIBHannover/TODO", build_vignettes = TRUE)
```

## Example

For instance, you conducted ANOVA on Iris data.

``` r
library(mrap)
attach(iris)
my_anova <- stats::aov(Petal.Length ~ Species, iris)
my_results <- summary(my_anova)[[1]]
```

On the [help page](https://knowledgeloom.tib.eu/pages/help), you see
that the `group_comparison` schema should be used. Instead of writing
the data manually with dtreg, use the `group_comparison` function from
mrap. Arguments `code_string`, `input_data`, and `test_results` should
be provided.

``` r
inst_gc <-
  mrap::group_comparison("stats::aov(Petal.Length ~ Species, data = iris)",
                         iris,
                         my_results)
                         my_json <- mrap::to_jsonld(inst_da)
```

Alternatively, you can use the all-in-one wrapper for `stats::aov`
function. It returns the ANOVA results similar to the original function
and a `group_comparison` instance:

``` r
aov <- mrap::stats_aov(Petal.Length ~ Species, data = iris)
results <- aov$anova
inst_gc <- aov$dtreg_object
```

The resulting `group_comparison` instance can be modified and included
into the `data_analysis` instance. The final instance can be written as
JSON-LD:

``` r
inst_gc$label <- "ANOVA for Iris petal length"
inst_da <- mrap::data_analysis(inst_gc)
my_json <- mrap::to_jsonld(inst_da)
```

For more information, please see the [help
page](https://knowledgeloom.tib.eu/pages/help) and the mrap vignette. To
access the vignette, you can run:

``` r
vignette("mrap", package="mrap")
```
