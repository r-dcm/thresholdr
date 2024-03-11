#' Create a tibble of predictions
#'
#' @param estimate A vector of probability estimates.
#' @param truth A vector of `0` and `1` integers.
#'
#' @return A [tibble][tibble::tibble-package] with two columns and rows equal to
#'   the length of `estimate` and `truth`.
#'
#' @noRd
estimate_tibble <- function(estimate, truth) {
  tibble::tibble(
    estimate = estimate,
    truth = factor(truth, levels = c(0, 1))
  )
}
