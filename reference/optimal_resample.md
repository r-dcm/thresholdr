# Use resampling to estimate an optimal probability threshold

Use resampling to estimate an optimal probability classification
threshold when true classifications are unknown.

## Usage

``` r
optimal_resample(
  estimates,
  optimal_method,
  samples = 1000,
  weight_by = NULL,
  comp_thresholds = NULL,
  metrics = NULL
)
```

## Arguments

- estimates:

  A vector of probabilities.

- optimal_method:

  The method for estimating the optimal threshold. One of "youden",
  "topleft", "cz", "gmean".

- samples:

  The number of samples of generated true values to create.

- weight_by:

  Optional. If provided, must be a class probability metric from
  [yardstick](https://yardstick.tidymodels.org/reference/yardstick-package.html).
  Used to weight the optimal threshold from each resample when
  calculating the overall optimal threshold (see details below). If
  `NULL` (the default), all resamples are weighted equally.

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

- `.threshold`: The averaged optimal threshold.

- If `weight_by` was specified, an
  [rvar](https://mc-stan.org/posterior/reference/rvar.html) containing
  the distribution of class probability metrics across all samples.

- A set of [rvar](https://mc-stan.org/posterior/reference/rvar.html)
  objects for each of the specified performance metrics, containing the
  distributions across all samples (i.e., 1 column per specified
  metric).

## Details

For each sample requested, a new vector of "true" values is generated
using
[`generate_truth()`](https://thresholdr.r-dcm.org/reference/generate_truth.md).
Then, the optimal threshold is calculated for each set of generated
"true" values using the specified `optimal_method`.

The final optimal threshold is then calculated as the average of the
thresholds that were estimated for each sample. If desired, a distance-
weighted mean can be specified by supplying a class probability metric
from the
[yardstick](https://yardstick.tidymodels.org/reference/yardstick-package.html)
(e.g.,
[`yardstick::roc_auc()`](https://yardstick.tidymodels.org/reference/roc_auc.html)).
When `weight_by` is specified, the chosen metric is computed for the
provided `estimates` and each sample of generated "true" values. For
example, we could calculate the area under the ROC curve (AUC) for each
sample. The weight given to each threshold is then determined by the
distance of corresponding AUC values from other AUC values (i.e., give
less weight to outlying metrics).

Finally, the average threshold is applied to each of the generated
samples of "true" values to calculate performance metrics for each
resample (e.g., sensitivity, specificity). In addition, we can also
specify additional thresholds to compare (`comp_thresholds`) that may be
of interest (e.g., comparing our optimal threshold to the traditional
threshold of 0.5). Thus, the final returned object includes each of the
investigated thresholds (i.e., the optimal threshold and any specified
in `comp_thresholds`) and the distribution of performance metrics across
all resamples for each of the thresholds. To change the metrics that are
provided by default, specify new `metrics`.

## See also

Other threshold approximation methods:
[`optimal_iterate()`](https://thresholdr.r-dcm.org/reference/optimal_iterate.md)

## Examples

``` r
est <- runif(100)
optimal_resample(estimates = est, optimal_method = "youden", samples = 200)
#> # A tibble: 1 × 5
#>   .threshold   sensitivity   specificity      j_index      distance
#>        <dbl>    <rvar[1d]>    <rvar[1d]>   <rvar[1d]>    <rvar[1d]>
#> 1      0.541  0.76 ± 0.046  0.76 ± 0.052  0.53 ± 0.09  0.12 ± 0.044
```
