#' Use iteration to estimate an optimal probability threshold
#'
#' Use iteration to estimate an optimal probability threshold when true
#' classifications are unknown.
#'
#' @inheritParams optimal_resample
#' @param weight_method The method for generating classifications, weighted by
#'   the current estimate of the optimal threshold. One of
#'   `r paste0("\"", weighting_method_choices(), "\"", collapse = ", ")`.
#' @param ... Additional arguments passed to the corresponding weighting method.
#' @param additional_criterion Optional. If provided, must be a class
#'   probability metric from [yardstick][yardstick::yardstick-package].
#' @param iter_burnin The number of iterations to run and then discard (see
#'   Details below).
#' @param iter_retain The number of iterations to retain (see Details below).
#'
#' @details
#' Additional details...
#'
#' @return A [tibble][tibble::tibble-package] with 1 row per threshold. The
#' columns are:
#'   * `.threshold`: The optimal threshold.
#'   * If `additional_criterion` was specified, an [rvar][posterior::rvar()]
#'     containing the distribution of class probability metrics across all
#'     retained iterations.
#'   * A set of [rvar][posterior::rvar()] objects for each of the specified
#'     performance metrics, containing the distributions across all retained
#'     iterations (i.e., 1 column per specified metric).
#' @export
#' @family threshold approximation methods
#'
#' @examples
#' est <- runif(100)
#' optimal_iterate(estimates = est, weight_method = "distance",
#'                 optimal_method = "youden", iter_retain = 100)
optimal_iterate <- function(estimates, weight_method, optimal_method, ...,
                            additional_criterion = NULL,
                            iter_burnin = 100, iter_retain = 1000,
                            comp_thresholds = NULL, metrics = NULL) {
  # check inputs -----
  estimates <- check_double(estimates, lb = 0, ub = 1)
  weight_method <- rlang::arg_match(weight_method, weighting_method_choices())
  optimal_method <- rlang::arg_match(optimal_method, optimal_method_choices())
  converge_function <- if (is.null(additional_criterion)) {
    additional_criterion <- "identity"
    \(data, truth, ...) tibble::tibble(.estimate = 1)
  } else {
    check_prob_metric(additional_criterion)
  }
  iter_burnin <- check_integer(iter_burnin, lb = 0, inclusive = FALSE,
                               exp_length = 1)
  iter_retain <- check_integer(iter_retain, lb = 0, inclusive = FALSE,
                               exp_length = 1)
  comp_thresholds <- check_double(comp_thresholds, lb = 0, ub = 1,
                                  allow_null = TRUE)

  # identify needed functions -----
  optimal_function <- get_optimal_function(optimal_method)
  weight_function <- get_weighting_function(weight_method)

  # initial state -----
  iter_truth <- generate_truth(estimates)
  iter_threshold <- optimal_function(estimates = estimates, truth = iter_truth)
  iter_conv <- converge_function(estimate_tibble(estimates, iter_truth),
                                 truth = "truth", "estimate",
                                 event_level = "second") |>
    dplyr::pull(".estimate")

  # burn-in iterations -----
  for (i in seq_len(iter_burnin)) {
    iter_truth <- weight_function(estimates, threshold = iter_threshold, ...)
    iter_threshold <- optimal_function(estimates = estimates,
                                       truth = iter_truth)
    iter_conv <- converge_function(estimate_tibble(estimates, iter_truth),
                                   truth = "truth", "estimate",
                                   event_level = "second") |>
      dplyr::pull(".estimate")
  }

  # retained iterations -----
  dat <- vector(mode = "list", length = iter_retain)
  thr <- vector(mode = "double", length = iter_retain)
  cnv <- vector(mode = "double", length = iter_retain)
  for (i in seq_len(iter_retain)) {
    iter_truth <- weight_function(estimates, threshold = iter_threshold, ...)
    iter_threshold <- optimal_function(estimates = estimates,
                                       truth = iter_truth)
    iter_conv <- converge_function(estimate_tibble(estimates, iter_truth),
                                   truth = "truth", "estimate",
                                   event_level = "second") |>
      dplyr::pull(".estimate")

    dat[[i]] <- estimate_tibble(estimates, iter_truth)
    thr[i] <- iter_threshold
    cnv[i] <- iter_conv
  }

  # check for convergence -----
  chain <- tibble::tibble(iteration = seq_len(iter_retain), dat = dat) |>
    dplyr::mutate(threshold = thr, conv = cnv,
                  mean_thr = mean(.data$threshold))

  chain_diagnostics <- chain |>
    dplyr::select("threshold", "conv") |>
    dplyr::summarize(dplyr::across(dplyr::everything(), posterior::rvar)) |>
    dplyr::rename(!!additional_criterion := "conv") |>
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to = "parameter", values_to = "draws") |>
    dplyr::filter(.data$parameter != "identity") |>
    dplyr::mutate(
      chain_summary = purrr::map(
        .data$draws,
        \(x) {
          posterior::summarize_draws(
            x,
            posterior::default_convergence_measures()
          ) |>
            dplyr::select(-"variable")
        }
      )
    ) |>
    tidyr::unnest("chain_summary")

  # return results -----
  results <- chain |>
    dplyr::mutate(
      results = purrr::map2(
        .data$dat, .data$mean_thr,
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
    dplyr::select(-"dat") |>
    tidyr::unnest("results") |>
    dplyr::mutate(
      conv = dplyr::case_when(
        .data$.threshold %in% comp_thresholds ~ NA_real_,
        .default = .data$conv
      )
    ) |>
    dplyr::rename(!!additional_criterion := "conv") |>
    dplyr::select("iteration", !!additional_criterion,
                  ".threshold":dplyr::last_col(),
                  -dplyr::any_of("identity")) |>
    dplyr::summarize(dplyr::across(dplyr::where(is.double), posterior::rvar),
                     .threshold = posterior::E(.data$.threshold),
                     .by = ".threshold") |>
    dplyr::relocate(".threshold", .before = 1)

  # print diagnostics and return -----
  if (any(chain_diagnostics$rhat > 1.01)) {
    chain_diagnostics |>
      dplyr::filter(.data$rhat > 1.01) |>
      glue::glue_data("Statistic did not converge (Rhat > 1.01): ",
                      "{{.arg {paste(parameter)}}}") |>
      cli::cli_warn()
  }

  return(results)
}
