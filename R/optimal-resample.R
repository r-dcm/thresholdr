#' Use resampling to estimate an optimal probability threshold
#'
#' Use resampling to estimate an optimal probability classification threshold
#' when true classifications are unknown.
#'
#' @param estimates A vector of probabilities.
#' @param optimal_method The method for estimating the optimal threshold. One of
#'   `r paste0("\"", optimal_method_choices(), "\"", collapse = ", ")`.
#' @param samples The number of samples of generated true values to create.
#' @param weight_by Optional. If provided, must be a class probability metric
#'   from [yardstick][yardstick::yardstick-package] used to weight the optimal
#'   threshold from each resample when calculate the overall optimal threshold
#'   (see Details below). If `NULL` (the default), all resamples are weighted
#'   equally.
#' @param comp_thresholds Additional threshold values to evaluate against the
#'   average optimal threshold (e.g., to compare the optimal threshold to a
#'   competing threshold such as 0.5). If `NULL` (the default), no additional
#'   thresholds are included in the performance evaluation.
#' @param metrics Either `NULL` or a [yardstick::metric_set()] with a list of
#'   performance metrics to calculate. The metrics should all be oriented
#'   towards hard class predictions (e.g., [yardstick::sensitivity()],
#'   [yardstick::accuracy()], [yardstick::recall()]) and not class
#'   probabilities. A set of default metrics is used when `NULL` (see
#'   [probably::threshold_perf()] for details).
#'
#' @details
#' For each sample requested, a new vector of "true" values is generated using
#' [generate_truth()]. Then, the optimal threshold is calculated for each set of
#' generated "true" values using the specified `optimal_method`.
#'
#' The final optimal threshold is then calculated the average of the thresholds
#' that were estimated for each sample. If desired, a distance-weighted mean can
#' be specified by supplying a class probability metric from the
#' [yardstick][yardstick::yardstick-package] (e.g., [yardstick::roc_auc()]).
#' When `weight_by` is specified, the chosen metric is computed for the
#' provided `estimates` and each sample of generated "true" values. For example,
#' we could calculate the area under the ROC curve (AUC) for each sample. The
#' weight given to each threshold is then determined by the distance of
#' corresponding AUC values from other AUC values (i.e., give less weight to
#' outlying metrics).
#'
#' Finally, the average threshold is applied to each of the generated samples of
#' "true" values to calculate performance metrics for each resample (e.g.,
#' sensitivity, specificity). In addition, we can also specify additional
#' thresholds to compare (`comp_thresholds`) that may be of interest (e.g.,
#' comparing our optimal threshold to the traditional threshold of 0.5). Thus,
#' the final returned object includes each of the investigated thresholds (i.e.,
#' the optimal threshold and any specified in `comp_thresholds`) and the
#' distribution of performance metrics across all resamples for each of the
#' thresholds. To change the metrics that are provided by default, specify new
#' `metrics`.
#'
#' @return A [tibble][tibble::tibble-package] with 1 row per threshold. The
#' columns are:
#'   * `threshold`: The averaged optimal threshold.
#'   * If `weight_by` was specified, an [rvar][posterior::rvar()] containing the
#'     distribution of class probability metrics across all samples.
#'   * A set of [rvar][posterior::rvar()] objects for each of the specified
#'     performance metrics, containing the distributions across all samples
#'     (i.e., 1 column per specified metric).
#' @export
#'
#' @examples
#' est <- runif(100)
#' optimal_resample(estimates = est, optimal_method = "youden", samples = 200)
optimal_resample <- function(estimates, optimal_method, samples = 1000,
                             weight_by = NULL, comp_thresholds = NULL,
                             metrics = NULL) {
  # check inputs -----
  estimates <- check_double(estimates, lb = 0, ub = 1)
  optimal_method <- rlang::arg_match(optimal_method, optimal_method_choices())
  samples <- check_integer(samples, lb = 0, inclusive = FALSE, exp_length = 1)
  weight_function <- if (is.null(weight_by)) {
    weight_by <- "identity"
    \(data, truth, ...) tibble::tibble(.estimate = 1)
  } else {
    check_prob_metric(weight_by)
  }
  comp_thresholds <- check_double(comp_thresholds, lb = 0, ub = 1,
                                  allow_null = TRUE)

  # identify needed functions -----
  optimal_function <- get_optimal_function(optimal_method)

  # calculate optimal threshold and performance -----
  tibble::tibble(sample_id = seq_len(samples),
                 estimates = list(estimates)) |>

    # create data
    dplyr::mutate(
      truth = lapply(estimates, generate_truth),
      dat = purrr::map2(.data$estimates, .data$truth,
                        estimate_tibble)
    ) |>

    # calculate thresholds
    dplyr::mutate(
      threshold = purrr::map2_dbl(.data$estimates, .data$truth,
                                  optimal_function),
      wt_func = purrr::map_dbl(
        .data$dat,
        \(x) {
          weight_function(x, truth = "truth", "estimate",
                          event_level = "second") |>
            dplyr::pull(.data$.estimate)
        }
      ),
      weight = purrr::map_dbl(
        .data$wt_func,
        \(x, full, weight_by) {
          if (weight_by == "identity") return(1)
          1 / sum(abs(x - full))
        },
        full = .data$wt_func, weight_by = weight_by
      ),
      wt_threshold = stats::weighted.mean(.data$threshold, .data$weight)
    ) |>

    # calculate performance
    dplyr::mutate(
      results = purrr::map2(
        .data$dat, .data$wt_threshold,
        \(x, y, comp, metrics) {
          probably::threshold_perf(
            x,
            truth = "truth",
            estimate = "estimate",
            thresholds = c(y, comp),
            event_level = "second",
            metrics = metrics
          ) |>
            dplyr::select(".threshold", ".metric", ".estimate") |>
            tidyr::pivot_wider(names_from = ".metric",
                               values_from = ".estimate")
        },
        comp = comp_thresholds, metrics = metrics
      )
    ) |>

    # clean-up
    dplyr::select(-c("estimates", "truth", "dat")) |>
    tidyr::unnest("results") |>
    dplyr::mutate(
      wt_func = dplyr::case_when(
        .data$.threshold %in% comp_thresholds ~ NA_real_,
        .default = .data$wt_func
      )
    ) |>
    dplyr::rename(!!weight_by := "wt_func") |>
    dplyr::select("sample_id", !!weight_by, ".threshold":dplyr::last_col(),
                  -dplyr::any_of("identity")) |>
    dplyr::summarize(dplyr::across(dplyr::where(is.double), posterior::rvar),
                     .threshold = posterior::E(.data$.threshold),
                     .by = ".threshold") |>
    dplyr::relocate(".threshold", .before = 1)
}
