# Use iteration to estimate an optimal probability threshold

Use iteration to estimate an optimal probability threshold when true
classifications are unknown.

## Usage

``` r
optimal_iterate(
  estimates,
  weighting_method,
  optimal_method,
  ...,
  additional_criterion = NULL,
  iter_burnin = 100,
  iter_retain = 1000,
  comp_thresholds = NULL,
  metrics = NULL
)
```

## Arguments

- estimates:

  A vector of probabilities.

- weighting_method:

  The method for generating classifications, weighted by the current
  estimate of the optimal threshold. One of "beta", "distance".

- optimal_method:

  The method for estimating the optimal threshold. One of "youden",
  "topleft", "cz", "gmean".

- ...:

  Additional arguments passed to the corresponding weighting method.

- additional_criterion:

  Optional. If provided, must be a class probability metric from
  [yardstick](https://yardstick.tidymodels.org/reference/yardstick-package.html).

- iter_burnin:

  The number of iterations to run and then discard (see details below).

- iter_retain:

  The number of iterations to retain (see details below).

- comp_thresholds:

  Additional threshold values to evaluate against the average optimal
  threshold (e.g., to compare the optimal threshold to a competing
  threshold such as 0.5). If `NULL` (the default), no additional
  thresholds are included in the performance evaluation.

- metrics:

  Either `NULL` or a
  [`yardstick::metric_set()`](https://yardstick.tidymodels.org/reference/metric_set.html)
  with a list of performance metrics to calculate. The metrics should
  all be oriented towards hard class predictions (e.g.,
  [`yardstick::sensitivity()`](https://yardstick.tidymodels.org/reference/sens.html),
  [`yardstick::accuracy()`](https://yardstick.tidymodels.org/reference/accuracy.html),
  [`yardstick::recall()`](https://yardstick.tidymodels.org/reference/recall.html))
  and not class probabilities. A set of default metrics is used when
  `NULL` (see
  [`probably::threshold_perf()`](https://probably.tidymodels.org/reference/threshold_perf.html)
  for details).

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with 1 row per threshold. The columns are:

- `.threshold`: The optimal threshold.

- If `additional_criterion` was specified, an
  [rvar](https://mc-stan.org/posterior/reference/rvar.html) containing
  the distribution of class probability metrics across all retained
  iterations.

- A set of [rvar](https://mc-stan.org/posterior/reference/rvar.html)
  objects for each of the specified performance metrics, containing the
  distributions across all retained iterations (i.e., 1 column per
  specified metric).

## Details

To initialize the iteration process, a vector of "true" values is
generated using
[`generate_truth()`](https://thresholdr.r-dcm.org/reference/generate_truth.md).
Then, the optimal threshold is calculated using the set of generated
"true" values and the specified `optimal_method`. A new vector of "true"
values is then generated, with classifications biased in the direction
of the calculated optimal threshold using the method specified by
`weighting_method`. That is, `estimates` will be less likely to result
in a classification 1 if the threshold is .8 than if it is .5. Using the
updated vector of "true" values, a new optimal threshold is calculated.
This proceeds for the specified number of iterations. The total number
of iterations is given by `iter_burnin + iter_retain`; however, the
first `iter_burnin` iterations are discarded. For example, if you
specify 100 burn-in iterations and 1,000 retained iterations, a total of
1,100 total iterations will be completed, but results will be based only
on the final 1,000 iterations. The optimal threshold is then calculated
as the average of the threshold values from the retained iterations.

Convergence of the iteration process is monitored using the \\\hat{R}\\
statistic described by Vehtari et al. (2021). By default, the
\\\hat{R}\\ statistic is calculated for the optimal threshold values
that are estimated at each iteration. Optionally, users may specify and
`additional_criterion` to be monitored with the \\\hat{R}\\. For
example, we could calculate the area under the ROC curve with the "true"
values used at each iteration to monitor that value for convergence as
well. A warning is produced if the threshold or, if specified, the
`additional_criterion` do not meet the convergence criteria of an
\\\hat{R}\\ less than 1.01 recommended by Vehtari et al. (2021).

Finally, the average threshold is applied to the samples of "true"
values that were generated at each iteration to calculate performance
metrics for each iteration (e.g., sensitivity, specificity). In
addition, we can also specify additional thresholds to compare
(`comp_thresholds`) that may be of interest (e.g., comparing our optimal
threshold to the traditional threshold of 0.5). Thus, the final returned
object includes each of the investigated thresholds (i.e., the optimal
threshold and any specified in `comp_thresholds`) and the distribution
of the performance metrics across all retained iterations for each of
the thresholds. To change the metrics that are provided by default,
specify new `metrics`.

## References

Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., & Bürkner, P.-C.
(2021). Rank-normalization, folding, and localization: An improved
\\\hat{R}\\ for assessing convergence of MCMC (with discussion).
*Bayesian Analysis, 16*(2), 667-718.
[doi:10.1214/20-BA1221](https://doi.org/10.1214/20-BA1221)

## See also

Other threshold approximation methods:
[`optimal_resample()`](https://thresholdr.r-dcm.org/reference/optimal_resample.md)

## Examples

``` r
est <- runif(100)
optimal_iterate(estimates = est, weighting_method = "distance",
                optimal_method = "youden", iter_retain = 100)
#> # A tibble: 1 × 5
#>   .threshold   sensitivity   specificity      j_index      distance
#>        <dbl>    <rvar[1d]>    <rvar[1d]>   <rvar[1d]>    <rvar[1d]>
#> 1      0.542  0.79 ± 0.047  0.72 ± 0.061  0.51 ± 0.07  0.13 ± 0.037
```
