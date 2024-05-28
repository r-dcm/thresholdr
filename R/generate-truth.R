#' Generate binary values from a vector of probabilities
#'
#' For each probability a binary outcome is generated. In practice, it is likely
#' useful generate multiple truth values that can be tested.
#'
#' @param estimates A vector of probabilities
#' @param threshold For Beta- and distance-weighted generation, the probability
#'   classification threshold.
#' @param precision For Beta-weighted generation, the precision for the Beta
#'   distribution. See Details for specifics.
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
#' threshold is .5 than if the threshold is .8. There are currently two
#' weighting methods implemented: Beta-weighted and distance-weighted.
#'
#' For Beta-weighted generation we generate a series of random numbers from a
#' Beta distribution. The Beta distribution is defined by its mean (`threshold`)
#' and `precision`. For proficiency estimate, we draw a random value from the
#' Beta distribution. If the random value is less than the estimate, the
#' generated truth value is `1`, and `0` otherwise. When using a flat Beta
#' distribution, this method is equivalent to unweighted generation.
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
#' @return An integer vector of `0` and `1`, the same length as `estimates`.
#' @export
#'
#' @examples
#' generate_truth(runif(10))
#'
#' generate_beta_wt_truth(runif(10), threshold = 0.7, precision = 5)
#'
#' generate_distance_wt_truth(runif(10), threshold = 0.6)
generate_truth <- function(estimates, ...) {
  estimates <- check_double(estimates, lb = 0, ub = 1)

  vapply(estimates, \(x) rbern(n = 1, prob = x), integer(1))
}

#' @author W. Jake Thompson
#' @export
#' @rdname generate_truth
generate_beta_wt_truth <- function(estimates, threshold = 0.5, ...,
                                   precision = 2) {
  estimates <- check_double(estimates, lb = 0, ub = 1)
  threshold <- check_double(threshold, lb = 0, ub = 1)
  precision <- check_double(precision, lb = 0, inclusive = FALSE)

  beta_shapes <- muphi_to_shapes(mu = threshold, phi = precision)

  as.integer(estimates > rbeta(n = length(estimates),
                               shape1 = beta_shapes$shape1,
                               shape2 = beta_shapes$shape2))
}

#' @author Jonathan A. Pedroza
#' @export
#' @rdname generate_truth
generate_distance_wt_truth <- function(estimates, threshold = 0.5, ...) {
  estimates <- check_double(estimates, lb = 0, ub = 1)
  threshold <- check_double(threshold, lb = 0, ub = 1)

  bound(estimates - threshold + .5,
        lb = max(0 - threshold + 0.5, 0),
        ub = min(1 - threshold + 0.5, 1)) |>
    generate_truth()
}
