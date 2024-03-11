#' Generate binary values from a vector of probabilities
#'
#' For each probability a binary outcome is created using [rbern()]. In
#' practice, it is likely useful generate multiple truth values that can be
#' tested.
#'
#' @param estimate A vector of probabilities
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
