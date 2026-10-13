
<!-- README.md is generated from README.Rmd. Please edit that file -->

# thresholdr <a href="https://thresholdr.r-dcm.org"><img src="man/figures/logo.png" align="right" height="138" alt="thresholdr website" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/thresholdr)](https://CRAN.R-project.org/package=thresholdr)
[![R-CMD-check](https://github.com/r-dcm/thresholdr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/r-dcm/thresholdr/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/r-dcm/thresholdr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/r-dcm/thresholdr?branch=main)
<!-- badges: end -->

The goal of thresholdr is to estimate optimal probability thresholds for
determining classifications when the true state is unknown, such as when
using diagnostic classification models.

## Installation

You can install the development version of thresholdr like so:

``` r
# install.packages("remotes")
remotes::install_github("r-dcm/thresholdr")
```

## Example usage

When the true classifications are known, we can calculate the optimal
threshold directly using a supported method, such as Youden’s J
statistic. For example, `dcm_probs` contains estimated probabilities of
proficiency from a simulated diagnostic classification model, along with
the true classifications:

``` r
library(thresholdr)

calc_youden(estimates = dcm_probs$att1$estimate, truth = dcm_probs$att1$truth)
#> [1] 0.3170266
```

In practice, however, the true classifications are unknown. Instead, we
can estimate the optimal threshold from the probabilities alone, for
example by resampling, and compare it to the traditional threshold of
0.5:

``` r
set.seed(1213)
optimal_resample(
  estimates = dcm_probs$att1$estimate,
  optimal_method = "youden",
  comp_thresholds = 0.5
)
#> # A tibble: 2 × 5
#>   threshold   sensitivity   specificity       j_index        distance
#>       <dbl>    <rvar[1d]>    <rvar[1d]>    <rvar[1d]>      <rvar[1d]>
#> 1     0.371  0.89 ± 0.020  0.91 ± 0.012  0.80 ± 0.025  0.021 ± 0.0053
#> 2     0.5    0.86 ± 0.021  0.94 ± 0.011  0.79 ± 0.026  0.025 ± 0.0067
```

To estimate thresholds for every attribute in a model fitted with
`{measr}`, `{GDINA}`, or `{CDM}`, use `find_threshold()`. For a complete
walk-through, see the `vignette("thresholdr")` article.
