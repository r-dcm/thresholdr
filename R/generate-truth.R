#' Generate binary values from a vector of probabilities
#'
#' For each probability a binary outcome is created using [rbern()]. In
#' practice, it is likely useful generate multiple truth values that can be
#' tested.
#'
#' @param estimates A vector of probabilities
#'
#' @return An integer vector of `0` and `1`, the same length as `estimate`.
#' @export
#'
#' @examples
#' generate_truth(runif(10))
#'
#' generate_truth(runif(5))
generate_truth <- function(estimates) {
  estimates <- check_double(estimates, lb = 0, ub = 1)

  vapply(estimates, \(x) rbern(n = 1, prob = x), integer(1))
}

generate_weighted_truth <- function(estimates, mu = 0.5, phi = 2) {
  estimates <- check_double(estimates, lb = 0, ub = 1)

  beta_shapes <- muphi_to_shapes(mu, phi)

  tibble::tibble(estimate = estimates,
                 random = runif(n = length(estimates))) |>
    dplyr::mutate(
      dll_rand = dbeta(.data$random, shape1 = beta_shapes$shape1,
                       shape2 = beta_shapes$shape2),
      dll_mean = dbeta(mu, shape1 = beta_shapes$shape1,
                       shape2 = beta_shapes$shape2),
      diff = abs(.data$dll_rand - .data$dll_mean),
      regress_pct = .data$diff / dll_mean,
      regress_pct = scales::rescale(.data$regress_pct,
                                    c(min(.data$regress_pct), 0.5)),
      new_random = (.data$regress_pct * mu) +
        ((1 - .data$regress_pct) * .data$random),
      truth = as.integer(.data$new_random < .data$estimate)
    ) |>
    dplyr::pull("truth")
}


shapes_to_muphi <- function(shape1, shape2) {
  mu <- shape1 / (shape1 + shape2)
  phi <- shape1 + shape2
  return(list(mu = mu, phi = phi))
}

muphi_to_shapes <- function(mu, phi) {
  shape1 <- mu * phi
  shape2 <- (1 - mu) * phi
  return(list(shape1 = shape1, shape2 = shape2))
}
