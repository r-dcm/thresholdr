#' Convert between parametrizations of the Beta distribution
#'
#' Given two shape parameters, calculate the mean and precision of the Beta
#' distribution; or, given a mean and precision, calculate the corresponding
#' shape parameters.
#'
#' @param shape1 The first shape parameter of the Beta distribution
#' @param shape2 The second shape parameter of the Beta distribution
#' @param mu The mean of the Beta distribution
#' @param phi The precision of the Beta distribution
#'
#' @details
#' The Beta distribution is defined by two shape parameters, &alpha; and
#' &beta;. Often it is desirable to parametrize the Beta distribution in terms
#' of its mean and precision, rather than the shape parameters. Given, shape
#' parameters, the mean (&mu;) and precision (&phi;) are calculated as:
#'
#' \deqn{\mu = \frac{\alpha}{\alpha + \beta}}
#' \deqn{\phi = \alpha + \beta}
#'
#' Similarly, given a mean and precision for the Beta distribution, the shape
#' parameters can be calculated as:
#'
#' \deqn{\alpha = \mu\phi}
#' \deqn{\beta = (1 - \mu)\phi}
#'
#' For an intuitive introduction to the Beta distribution, see this
#' [post](https://www.andrewheiss.com/blog/2021/11/08/beta-regression-guide/)
#' from Andrew Heiss.
#'
#' @return A list with two elements containing either the converted shape
#'   parameters or mean and precision values.
#' @export
#'
#' @examples
#' shapes_to_muphi(6, 4)
#'
#' muphi_to_shapes(0.6, 10)
#'
#' @rdname beta-conversion
#' @author Andrew Heiss
shapes_to_muphi <- function(shape1, shape2) {
  shape1 <- check_double(shape1, lb = 0, inclusive = FALSE)
  shape2 <- check_double(shape2, lb = 0, inclusive = FALSE)

  mu <- shape1 / (shape1 + shape2)
  phi <- shape1 + shape2
  return(list(mu = mu, phi = phi))
}

#' @export
#' @rdname beta-conversion
muphi_to_shapes <- function(mu, phi) {
  mu <- check_double(mu, lb = 0, ub = 1, inclusive = FALSE)
  phi <- check_double(phi, lb = 0, inclusive = FALSE)

  shape1 <- mu * phi
  shape2 <- (1 - mu) * phi
  return(list(shape1 = shape1, shape2 = shape2))
}


#' Bound a variable between allowable values
#'
#' @param x A numeric value
#' @param lb The lower bound of allowable values
#' @param ub The upper bound of allowable values
#' @param buffer A buffer around `lb` and `ub`. For example, with an `lb` of 0
#'   and `buffer` of 1e-5, all values less than 0 would be set to 1e-5.
#'
#' @return A numeric the same length as `x`.
#' @noRd
bound <- function(x, lb, ub, buffer = 1e-5) {
  pmax(pmin(x, (ub - buffer)), (lb + buffer))
}
