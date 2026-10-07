# Top-left statistic

Calculate the optimal probability classification threshold using the
point closest to the top-left corner of the ROC plot.

## Usage

``` r
calc_topleft(estimates, truth)
```

## Arguments

- estimates:

  A vector of classification probabilities. Values should represent the
  probability of `1` in the `truth` argument.

- truth:

  An integer vector of `0` and `1` representing the true
  classifications.

## Value

A numeric scalar representing the optimal probability threshold.

## Details

The top-left corner of an ROC plot represents perfect sensitivity and
specificity. The top-left statistic is determined by calculating the
distance from each point on the ROC curve (e.g.,
[`create_roc()`](https://thresholdr.r-dcm.org/reference/create_roc.md))
to the top-left corner of the plot. The point on the plot that is the
closest to the top-left corner is the optimal threshold. The optimality
criterion is then defined as:

\$\$\text{min}((1 - sensitivity)^2 + (1 - specificity)^2)\$\$

## See also

Other optimal threshold methods:
[`calc_cz()`](https://thresholdr.r-dcm.org/reference/calc_cz.md),
[`calc_gmean()`](https://thresholdr.r-dcm.org/reference/calc_gmean.md),
[`calc_youden()`](https://thresholdr.r-dcm.org/reference/calc_youden.md)

## Examples

``` r
calc_topleft(estimates = dcm_probs$att1$estimate,
             truth = dcm_probs$att1$truth)
#> [1] 0.3170266

calc_topleft(estimates = dcm_probs$att2$estimate,
             truth = dcm_probs$att2$truth)
#> [1] 0.4363361

calc_topleft(estimates = dcm_probs$att3$estimate,
             truth = dcm_probs$att3$truth)
#> [1] 0.3722605
```
