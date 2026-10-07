# Youden's J statistic

Calculate the optimal probability classification threshold using
Youden's J statistic.

## Usage

``` r
calc_youden(estimates, truth)
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

Youden's J statistic (Youden, 1950) is defined as `sensitivty` +
`specificity` - 1. As such, the value of the J-index ranges from \[0,
1\], and is 1 when there are no false positive and no false negatives
(i.e., sensitivity and specificity). The J-index is calculated for each
possible threshold defined by the ROC curve (e.g.,
[`create_roc()`](https://thresholdr.r-dcm.org/reference/create_roc.md)).
The optimality criterion is then defined as:

\$\$\text{max}(sensitivity + specificity - 1)\$\$

## References

Youden, W. J. (1950). Index for rating diagnostic tests. *Cancer, 3*(1),
32-35.
[doi:10.1002/1097-0142(1950)3:1\<32::AID-CNCR2820030106\>3.0.CO;2-3](https://doi.org/10.1002/1097-0142%281950%293%3A1%3C32%3A%3AAID-CNCR2820030106%3E3.0.CO%3B2-3)

## See also

Other optimal threshold methods:
[`calc_cz()`](https://thresholdr.r-dcm.org/reference/calc_cz.md),
[`calc_gmean()`](https://thresholdr.r-dcm.org/reference/calc_gmean.md),
[`calc_topleft()`](https://thresholdr.r-dcm.org/reference/calc_topleft.md)

## Examples

``` r
calc_youden(estimates = dcm_probs$att1$estimate,
            truth = dcm_probs$att1$truth)
#> [1] 0.3170266

calc_youden(estimates = dcm_probs$att2$estimate,
            truth = dcm_probs$att2$truth)
#> [1] 0.6789389

calc_youden(estimates = dcm_probs$att3$estimate,
            truth = dcm_probs$att3$truth)
#> [1] 0.3722605
```
