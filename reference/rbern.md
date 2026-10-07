# Random Bernoulli draws

Random generation for the Bernoulli distribution with parameter `prob`.

## Usage

``` r
rbern(n, prob)
```

## Source

Based on the [rbinom](https://rdrr.io/r/stats/Binomial.html) function,
with `size = 1`.

## Arguments

- n:

  number of observations. If `length(n) > 1`, the length is taken to be
  the number required.

- prob:

  probability of success on each trial.

## Value

An integer vector of length `n`, consisting of `0` or `1`.

## See also

[Distributions](https://rdrr.io/r/stats/Distributions.html) for other
standard distributions, including
[dbinom](https://rdrr.io/r/stats/Binomial.html) for the binomial.

## Examples

``` r
# Generate random Bernoulli variables
rbern(n = 10, prob = 0.5)
#>  [1] 1 0 0 0 0 0 1 0 1 1
```
