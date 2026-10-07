# Concordance probability method

Calculate the optimal probability classification threshold using the
concordance probability method (CZ).

## Usage

``` r
calc_cz(estimates, truth)
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

The concordance probability method (Liu, 2012) is defined as the product
of the sensitivity and specificity at a given threshold cut-point. By
utilizing the product of both metrics, the value for the cut-point
remains within the range of \[0, 1\]. The optimal threshold is the
cut-point that maximizes the area of the rectangle related to the ROC
curve.

The optimality criterion is then defined as:

\$\$\text{max}(sensitivity \* specificity)\$\$

## References

Liu, X. (2012). Classification accuracy and cut point selection.
*Statistics in Medicine, 31*(23), 2676-2686.
[doi:10.1002/sim.4509](https://doi.org/10.1002/sim.4509)

## See also

Other optimal threshold methods:
[`calc_gmean()`](https://thresholdr.r-dcm.org/reference/calc_gmean.md),
[`calc_topleft()`](https://thresholdr.r-dcm.org/reference/calc_topleft.md),
[`calc_youden()`](https://thresholdr.r-dcm.org/reference/calc_youden.md)

## Examples

``` r
calc_cz(estimates = dcm_probs$att1$estimate,
            truth = dcm_probs$att1$truth)
#> [1] 0.3170266

calc_cz(estimates = dcm_probs$att2$estimate,
            truth = dcm_probs$att2$truth)
#> [1] 0.4363361

calc_cz(estimates = dcm_probs$att3$estimate,
            truth = dcm_probs$att3$truth)
#> [1] 0.3722605
```
