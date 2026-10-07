# Generate binary values from a vector of probabilities

For each probability a binary outcome is generated. In practice, it is
likely useful generate multiple truth values that can be tested.

## Usage

``` r
generate_truth(estimates, ...)

generate_beta_wt_truth(estimates, threshold = 0.5, ..., precision = 2)

generate_distance_wt_truth(estimates, threshold = 0.5, ...)
```

## Arguments

- estimates:

  A vector of probabilities.

- ...:

  Currently unused.

- threshold:

  For Beta- and distance-weighted generation, the probability
  classification threshold.

- precision:

  For Beta-weighted generation, the precision for the Beta distribution.
  See details for specifics.

## Value

An integer vector of `0` and `1`, the same length as `estimates`.

## Details

When generating truth values, `generate_truth()` uses
[`rbern()`](https://thresholdr.r-dcm.org/reference/rbern.md) to create
truth values from the supplied probabilities based on a Bernoulli
distribution.

When using a classification threshold other than .5, you may wish to
weight the truth generation process. For example, a proficiency estimate
of 0.7 should be more likely to result in a truth value of `1` if the
classification threshold is .5 than if the threshold is .8. There are
currently two weighting methods implemented: Beta-weighted and
distance-weighted.

For Beta-weighted generation we generate a series of random numbers from
a Beta distribution. The Beta distribution is defined by its mean
(`threshold`) and `precision`. For proficiency estimate, we draw a
random value from the Beta distribution. If the random value is less
than the estimate, the generated truth value is `1`, and `0` otherwise.
When using a flat Beta distribution, this method is equivalent to
unweighted generation.

For distance-weighted generation, the `threshold` is subtracted from
each proficiency estimate. We then add .5 to each difference, creating
new proficiency estimates. For example, if we specified a `threshold` of
0.6, respondents with an original proficiency estimate of .8 would have
a new proficiency estimate of `.8 - .6 + .5 = .7`. That, because the
classification threshold has increased, this respondent is less likely
to be classified as proficient. Respondents with an original estimate
equal to the `threshold` will always have a new estimate of .5. The new
estimates are then used to generate truth values using
[`rbern()`](https://thresholdr.r-dcm.org/reference/rbern.md). When the
`threshold` is set to .5, this method is equivalent to unweighted
generation.

## Author

W. Jake Thompson

Jonathan A. Pedroza

## Examples

``` r
generate_truth(runif(10))
#>  [1] 0 1 1 0 0 0 0 0 1 0

generate_beta_wt_truth(runif(10), threshold = 0.7, precision = 5)
#>  [1] 0 0 1 0 1 1 0 0 0 0

generate_distance_wt_truth(runif(10), threshold = 0.6)
#>  [1] 0 1 1 1 0 0 1 0 0 0
```
