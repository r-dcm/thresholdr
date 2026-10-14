#' Convert between parametrizations of the Beta distribution
#'
#' Given two shape parameters, calculate the mean and precision of the Beta
#' distribution; or, given a mean and precision, calculate the corresponding
#' shape parameters.
#'
#' @param shape1 The first shape parameter of the Beta distribution.
#' @param shape2 The second shape parameter of the Beta distribution.
#' @param mu The mean of the Beta distribution.
#' @param phi The precision of the Beta distribution.
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
#' @concept Beta
#'
#' @examples
#' shapes_to_muphi(6, 4)
#'
#' muphi_to_shapes(0.6, 10)
#'
#' @rdname beta-conversion
#' @author Andrew Heiss
shapes_to_muphi <- function(shape1, shape2) {
  rlang::check_number_decimal(shape1, min = 0)
  rlang::check_number_decimal(shape2, min = 0)

  mu <- shape1 / (shape1 + shape2)
  phi <- shape1 + shape2
  list(mu = mu, phi = phi)
}

#' @export
#' @rdname beta-conversion
muphi_to_shapes <- function(mu, phi) {
  rlang::check_number_decimal(mu, min = 0, max = 1)
  rlang::check_number_decimal(phi, min = 0)

  shape1 <- mu * phi
  shape2 <- (1 - mu) * phi
  list(shape1 = shape1, shape2 = shape2)
}


#' Find the Beta mean that corresponds to a given median
#'
#' @param median The desired median of the Beta distribution.
#' @param phi The precision of the Beta distribution.
#'
#' @concept Beta
#'
#' @return The mean (&mu;) of the Beta distribution with precision `phi` and a
#'   median of `median`.
#' @noRd
median_to_mu <- function(median, phi) {
  if (median == 0.5) {
    return(0.5)
  }

  stats::uniroot(
    \(mu) stats::pbeta(median, mu * phi, (1 - mu) * phi) - 0.5,
    interval = c(1e-8, 1 - 1e-8),
    tol = 1e-10
  )$root
}


#' Bound a variable between allowable values
#'
#' @param x A numeric value.
#' @param lb The lower bound of allowable values.
#' @param ub The upper bound of allowable values.
#' @param buffer A buffer around `lb` and `ub`. For example, with an `lb` of 0
#'   and `buffer` of 1e-5, all values less than 0 would be set to 1e-5.
#'
#' @return A numeric the same length as `x`.
#' @noRd
bound <- function(x, lb, ub, buffer = 1e-5) {
  pmax(pmin(x, (ub - buffer)), (lb + buffer))
}


#' Generate truth values that include both 0 and 1 values
#'
#' @param draw A function with no arguments that returns a vector of `0` and
#'   `1` integers.
#' @param max_tries The maximum number of times to call `draw`.
#' @param call The call stack for the error message.
#'
#' @return The first result of `draw()` that contains both `0` and `1` values.
#'   If the result is of length 1, it is returned immediately.
#' @noRd
draw_mixed <- function(draw, max_tries = 100, call = rlang::caller_env()) {
  for (i in seq_len(max_tries)) {
    truth <- draw()
    if (length(truth) < 2 || (any(truth == 0L) && any(truth == 1L))) {
      return(truth)
    }
  }

  cli::cli_abort(
    c(
      paste(
        "Could not generate truth values containing both {.val {0}} and",
        "{.val {1}} values after {max_tries} attempts."
      ),
      i = "Check whether all {.arg estimates} are close to 0 or 1."
    ),
    call = call
  )
}
