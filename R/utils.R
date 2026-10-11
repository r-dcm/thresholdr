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


#' Create a grouped tibble of predictions for multiple draws
#'
#' @param estimate A vector of probability estimates.
#' @param truths A list of vectors of `0` and `1` integers. Each vector should
#'   be the same length as `estimate`.
#'
#' @return A [tibble][tibble::tibble-package], grouped by `.draw`, with three
#'   columns (`.draw`, `estimate`, and `truth`) and rows equal to
#'   `length(estimate) * length(truths)`.
#'
#' @noRd
draws_tibble <- function(estimate, truths) {
  tibble::tibble(
    .draw = rep(seq_along(truths), each = length(estimate)),
    estimate = rep(estimate, times = length(truths)),
    truth = factor(unlist(truths), levels = c(0, 1))
  ) |>
    dplyr::group_by(.data$.draw)
}


#' Calculate a class probability metric for each draw
#'
#' @param draws A grouped tibble created by `draws_tibble()`.
#' @param criterion_function A class probability metric function from
#'   [yardstick][yardstick::yardstick-package].
#'
#' @return A numeric vector with one value per draw.
#'
#' @noRd
draw_criterion <- function(draws, criterion_function) {
  criterion_function(
    draws,
    truth = "truth",
    "estimate",
    event_level = "second"
  ) |>
    dplyr::pull(".estimate") |>
    rep_len(dplyr::n_groups(draws))
}


#' Calculate performance metrics for each draw
#'
#' @param draws A grouped tibble created by `draws_tibble()`.
#' @param thresholds The thresholds to evaluate.
#' @param metrics Either `NULL` or a [yardstick::metric_set()].
#'
#' @return A [tibble][tibble::tibble-package] with one row per draw and
#'   threshold, and one column per metric.
#'
#' @noRd
draw_performance <- function(draws, thresholds, metrics) {
  probably::threshold_perf(
    draws,
    truth = "truth",
    estimate = "estimate",
    thresholds = thresholds,
    event_level = "second",
    metrics = metrics
  ) |>
    dplyr::ungroup() |>
    dplyr::select(".draw", ".threshold", ".metric", ".estimate") |>
    tidyr::pivot_wider(names_from = ".metric", values_from = ".estimate")
}
