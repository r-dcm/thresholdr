# Calculate a receiver operating characteristic curve

Construct the full ROC curve given probability estimates and true
classifications.

## Usage

``` r
create_roc(estimates, truth)
```

## Arguments

- estimates:

  A vector of classification probabilities. Values should represent the
  probability of `1` in the `truth` argument.

- truth:

  An integer vector of `0` and `1` representing the true
  classifications.

## Value

An [roc_df](https://yardstick.tidymodels.org/reference/roc_curve.html)
object.

## Examples

``` r
create_roc(estimates = dcm_probs$att1$estimate, truth = dcm_probs$att1$truth)
#> # A tibble: 418 × 3
#>      .threshold specificity sensitivity
#>           <dbl>       <dbl>       <dbl>
#>  1 -Inf             0                 1
#>  2    0.0000716     0                 1
#>  3    0.000336      0.00316           1
#>  4    0.000390      0.00633           1
#>  5    0.000684      0.00949           1
#>  6    0.000694      0.0127            1
#>  7    0.000808      0.0158            1
#>  8    0.00109       0.0190            1
#>  9    0.00111       0.0222            1
#> 10    0.00122       0.0253            1
#> # ℹ 408 more rows
```
