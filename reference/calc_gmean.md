# G-mean method

Calculate the optimal probability classification threshold using the
G-Mean method.

## Usage

``` r
calc_gmean(estimates, truth)
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

The G-mean method (Kubat & Matwin, 1997) is defined as the square root
of the product of sensitivity and specificity at a given threshold. The
optimal threshold is the threshold with the greatest g-mean.

The optimality criterion is then defined as:

\$\$\text{max}\sqrt{(sensitivity \* specificity)}\$\$

## References

Kubat, M. & Matwin, S. (1997, July 8-12). *Addressing the curse of
imbalanced training sets: One-sided selection* \[Paper presentation\].
International Conference on Machine Learning, Nashville, TN.

## See also

Other optimal threshold methods:
[`calc_cz()`](https://thresholdr.r-dcm.org/reference/calc_cz.md),
[`calc_topleft()`](https://thresholdr.r-dcm.org/reference/calc_topleft.md),
[`calc_youden()`](https://thresholdr.r-dcm.org/reference/calc_youden.md)

## Examples

``` r
calc_gmean(estimates = dcm_probs$att1$estimate,
            truth = dcm_probs$att1$truth)
#> [1] 0.3170266

calc_gmean(estimates = dcm_probs$att2$estimate,
            truth = dcm_probs$att2$truth)
#> [1] 0.4363361

calc_gmean(estimates = dcm_probs$att3$estimate,
            truth = dcm_probs$att3$truth)
#> [1] 0.3722605
```
