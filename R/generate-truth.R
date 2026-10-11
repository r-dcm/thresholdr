#' Generate binary values from a vector of probabilities
#'
#' For each probability a binary outcome is generated. In practice, it is likely
#' useful generate multiple truth values that can be tested.
#'
#' @param estimates A vector of probabilities.
#' @param threshold For weighted generation, the probability classification
#'   threshold.
#' @param precision For Beta-weighted generation, the precision for the Beta
#'   distribution. See details for specifics.
#' @param ... Currently unused.
#'
#' @details
#' When generating truth values, [generate_truth()] uses [rbern()] to create
#' truth values from the supplied probabilities based on a Bernoulli
#' distribution.
#'
#' When using a classification threshold other than .5, you may wish to weight
#' the truth generation process. For example, a proficiency estimate of 0.7
#' should be more likely to result in a truth value of `1` if the classification
#' threshold is .5 than if the threshold is .8. There are currently three
#' weighting methods implemented: Beta-weighted, distance-weighted, and
#' log-odds-weighted. For all three methods, a respondent with an estimate equal
#' to the `threshold` has a .5 probability of a generated truth value of `1`.
#'
#' For Beta-weighted generation we generate a series of random numbers from a
#' Beta distribution. The Beta distribution is defined by its median
#' (`threshold`) and `precision` (i.e., the mean is chosen such that the median
#' of the distribution is equal to `threshold`). For each proficiency estimate,
#' we draw a random value from the Beta distribution. If the random value is
#' less than the estimate, the generated truth value is `1`, and `0` otherwise.
#' When the `threshold` is .5 and the `precision` is 2 (i.e., a flat Beta
#' distribution), this method is equivalent to unweighted generation.
#'
#' For distance-weighted generation, the `threshold` is subtracted from each
#' proficiency estimate. We then add .5 to each difference, creating new
#' proficiency estimates. For example, if we specified a `threshold` of 0.6,
#' respondents with an original proficiency estimate of .8 would have a new
#' proficiency estimate of `.8 - .6 + .5 = .7`. That, because the classification
#' threshold has increased, this respondent is less likely to be classified as
#' proficient. Respondents with an original estimate equal to the `threshold`
#' will always have a new estimate of .5. The new estimates are then used to
#' generate truth values using [rbern()]. When the `threshold` is set to .5,
#' this method is equivalent to unweighted generation.
#'
#' For log-odds-weighted generation, the log-odds of the `threshold` are
#' subtracted from the log-odds of each proficiency estimate, and the result is
#' converted back to the probability scale. For example, if we specified a
#' `threshold` of 0.6, respondents with an original proficiency estimate of .8
#' would have a new proficiency estimate of
#' `plogis(qlogis(.8) - qlogis(.6)) = .73`. This is equivalent to adjusting the
#' prior odds of proficiency, and unlike distance-weighted generation, the new
#' estimates never need to be truncated to remain within \[0, 1\]. The new
#' estimates are then used to generate truth values using [rbern()]. When the
#' `threshold` is set to .5, this method is equivalent to unweighted
#' generation.
#'
#' @return An integer vector of `0` and `1`, the same length as `estimates`.
#' @export
#'
#' @concept Beta
#'
#' @examples
#' generate_truth(runif(10))
#'
#' generate_beta_wt_truth(runif(10), threshold = 0.7, precision = 5)
#'
#' generate_distance_wt_truth(runif(10), threshold = 0.6)
#'
#' generate_logodds_wt_truth(runif(10), threshold = 0.6)
generate_truth <- function(estimates, ...) {
  check_double(estimates, min = 0, max = 1)
  rbern(n = length(estimates), prob = estimates)
}

#' @author W. Jake Thompson
#' @export
#' @rdname generate_truth
generate_beta_wt_truth <- function(
  estimates,
  threshold = 0.5,
  ...,
  precision = 2
) {
  check_double(estimates, min = 0, max = 1)
  rlang::check_number_decimal(threshold, min = 0, max = 1)
  rlang::check_number_decimal(precision, min = 0)

  beta_mu <- median_to_mu(median = bound(threshold, 0, 1), phi = precision)
  beta_shapes <- muphi_to_shapes(mu = beta_mu, phi = precision)

  as.integer(
    estimates >
      stats::rbeta(
        n = length(estimates),
        shape1 = beta_shapes$shape1,
        shape2 = beta_shapes$shape2
      )
  )
}

#' @author Jonathan A. Pedroza
#' @export
#' @rdname generate_truth
generate_distance_wt_truth <- function(estimates, threshold = 0.5, ...) {
  check_double(estimates, min = 0, max = 1)
  rlang::check_number_decimal(threshold, min = 0, max = 1)

  bound(
    estimates - threshold + .5,
    lb = max(0 - threshold + 0.5, 0),
    ub = min(1 - threshold + 0.5, 1)
  ) |>
    generate_truth()
}

#' @export
#' @rdname generate_truth
generate_logodds_wt_truth <- function(estimates, threshold = 0.5, ...) {
  check_double(estimates, min = 0, max = 1)
  rlang::check_number_decimal(threshold, min = 0, max = 1)

  stats::plogis(
    stats::qlogis(estimates) - stats::qlogis(bound(threshold, 0, 1))
  ) |>
    generate_truth()
}
