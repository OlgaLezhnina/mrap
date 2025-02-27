% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stats_aov.R
\name{stats_aov}
\alias{stats_aov}
\title{Wrap stats::aov function}
\usage{
stats_aov(..., jsonld = FALSE)
}
\arguments{
\item{...}{the same arguments as in the wrapped function}

\item{jsonld}{Boolean whether the output should be JSON-LD string}
}
\value{
a list of ANOVA object and JSON-LD
}
\description{
Wrap stats::aov function
}
\examples{
results <- stats_aov(Petal.Length ~ Species, data = iris)

}
