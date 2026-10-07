# Convert between parametrizations of the Beta distribution

Given two shape parameters, calculate the mean and precision of the Beta
distribution; or, given a mean and precision, calculate the
corresponding shape parameters.

## Usage

``` r
shapes_to_muphi(shape1, shape2)

muphi_to_shapes(mu, phi)
```

## Arguments

- shape1:

  The first shape parameter of the Beta distribution.

- shape2:

  The second shape parameter of the Beta distribution.

- mu:

  The mean of the Beta distribution.

- phi:

  The precision of the Beta distribution.

## Value

A list with two elements containing either the converted shape
parameters or mean and precision values.

## Details

The Beta distribution is defined by two shape parameters, α and β. Often
it is desirable to parametrize the Beta distribution in terms of its
mean and precision, rather than the shape parameters. Given, shape
parameters, the mean (μ) and precision (φ) are calculated as:

\$\$\mu = \frac{\alpha}{\alpha + \beta}\$\$ \$\$\phi = \alpha +
\beta\$\$

Similarly, given a mean and precision for the Beta distribution, the
shape parameters can be calculated as:

\$\$\alpha = \mu\phi\$\$ \$\$\beta = (1 - \mu)\phi\$\$

For an intuitive introduction to the Beta distribution, see this
[post](https://www.andrewheiss.com/blog/2021/11/08/beta-regression-guide/)
from Andrew Heiss.

## Author

Andrew Heiss

## Examples

``` r
shapes_to_muphi(6, 4)
#> $mu
#> [1] 0.6
#> 
#> $phi
#> [1] 10
#> 

muphi_to_shapes(0.6, 10)
#> $shape1
#> [1] 6
#> 
#> $shape2
#> [1] 4
#> 
```
