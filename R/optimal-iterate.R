#' Use iteration to estimate an optimal probability threshold
#'
#' Use iteration to estimate an optimal probability threshold when true
#' classifications are unknown.
#'
#' @inheritParams optimal_resample
#' @param weighting_method The method for generating classifications, weighted
#'   by the current estimate of the optimal threshold. One of
#'   `r paste0("\"", weighting_method_choices(), "\"", collapse = ", ")`.
#' @param ... Additional arguments passed to the corresponding weighting method.
#' @param additional_criterion Optional. If provided, must be a class
#'   probability metric from [yardstick][yardstick::yardstick-package].
#' @param chains The number of independent chains to run (see details below).
#' @param iter_burnin The number of iterations to run and then discard in each
#'   chain (see details below).
#' @param iter_retain The number of iterations to retain in each chain (see
#'   details below).
#'
#' @details
#' The goal of the iteration process is to find a self-consistent threshold.
#' That is, a threshold that, when used to generate "true" values, results in
#' the same optimal threshold. To initialize the iteration process, a vector of
#' "true" values is generated using the method specified by `weighting_method`
#' and an initial threshold. Then, the optimal threshold is calculated using the
#' set of generated "true" values and the specified `optimal_method`. A new
#' vector of "true" values is then generated, with classifications biased in the
#' direction of the calculated optimal threshold using the method specified by
#' `weighting_method`. That is, `estimates` will be less likely to result in a
#' classification 1 if the threshold is .8 than if it is .5. Using the updated
#' vector of "true" values, a new optimal threshold is calculated. This proceeds
#' for the specified number of iterations. The total number of iterations in
#' each chain is given by `iter_burnin + iter_retain`; however, the first
#' `iter_burnin` iterations are discarded. For example, if you specify 100
#' burn-in iterations and 250 retained iterations, a total of 350 iterations
#' will be completed in each chain, but results will be based only on the final
#' 250 iterations.
#'
#' Multiple `chains` are run independently, each starting from a different
#' initial threshold. When `chains = 1`, the initial threshold is .5 (i.e.,
#' equivalent to [generate_truth()]). Otherwise, initial thresholds are evenly
#' spaced from .1 to .9 (e.g., .1, .37, .63, and .9 for 4 chains). Dispersed
#' starting values make it possible to detect whether the final threshold
#' depends on where the iteration process began. The retained iterations from
#' all chains are pooled (i.e., `chains * iter_retain` total draws), and the
#' optimal threshold is calculated as the average of the threshold values from
#' all retained iterations.
#'
#' Convergence of the iteration process is monitored using the \eqn{\hat{R}}
#' statistic described by Vehtari et al. (2021). By default, the \eqn{\hat{R}}
#' statistic is calculated across chains for the optimal threshold values that
#' are estimated at each iteration. Optionally, users may specify an
#' `additional_criterion` to be monitored with the \eqn{\hat{R}}.
#' For example, we could calculate the area under the ROC curve with the "true"
#' values used at each iteration to monitor that value for convergence as well.
#' A warning is produced if the threshold or, if specified, the
#' `additional_criterion` do not meet the convergence criteria of an
#' \eqn{\hat{R}} less than 1.01 recommended by Vehtari et al. (2021). Bulk and
#' tail effective sample sizes are also calculated, and all diagnostics are
#' returned in the `"diagnostics"` attribute of the returned object.
#'
#' Finally, the average threshold is applied to the samples of "true" values
#' that were generated at each iteration to calculate performance metrics for
#' each iteration (e.g., sensitivity, specificity). In addition, we can also
#' specify additional thresholds to compare (`comp_thresholds`) that may be of
#' interest (e.g., comparing our optimal threshold to the traditional threshold
#' of 0.5). Thus, the final returned object includes each of the investigated
#' thresholds (i.e., the optimal threshold and any specified in
#' `comp_thresholds`) and the distribution of the performance metrics across all
#' retained iterations for each of the thresholds. To change the metrics that
#' are provided by default, specify new `metrics`.
#'
#' @return A [tibble][tibble::tibble-package] with 1 row per threshold. The
#' columns are:
#'   * `threshold`: The optimal threshold and any specified `comp_thresholds`.
#'   * If `additional_criterion` was specified, an [rvar][posterior::rvar()]
#'     containing the distribution of class probability metrics across all
#'     retained iterations.
#'   * A set of [rvar][posterior::rvar()] objects for each of the specified
#'     performance metrics, containing the distributions across all retained
#'     iterations (i.e., 1 column per specified metric).
#'
#' The returned object also has two attributes:
#'   * `"converged"`: A logical indicating whether all monitored statistics
#'     have an \eqn{\hat{R}} less than 1.01.
#'   * `"diagnostics"`: A [tibble][tibble::tibble-package] with one row per
#'     monitored statistic (`parameter`) and the \eqn{\hat{R}} (`rhat`), bulk
#'     effective sample size (`ess_bulk`), and tail effective sample size
#'     (`ess_tail`).
#' @export
#' @family threshold approximation methods
#' @concept rvar
#'
#' @examples
#' est <- runif(100)
#' optimal_iterate(
#'   estimates = est,
#'   weighting_method = "distance",
#'   optimal_method = "youden",
#'   chains = 2,
#'   iter_burnin = 50,
#'   iter_retain = 50
#' )
#'
#' @references Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., & Bürkner,
#'   P.-C. (2021). Rank-normalization, folding, and localization: An improved
#'   \eqn{\hat{R}} for assessing convergence of MCMC (with discussion).
#'   *Bayesian Analysis, 16*(2), 667-718. \doi{10.1214/20-BA1221}
optimal_iterate <- function(
  estimates,
  optimal_method,
  ...,
  weighting_method = "distance",
  additional_criterion = NULL,
  chains = 4,
  iter_burnin = 100,
  iter_retain = 250,
  comp_thresholds = NULL,
  metrics = NULL
) {
  # check inputs -----
  check_double(estimates, min = 0, max = 1)
  weighting_method <- rlang::arg_match(
    weighting_method,
    weighting_method_choices()
  )
  optimal_method <- rlang::arg_match(optimal_method, optimal_method_choices())
  converge_function <- if (is.null(additional_criterion)) {
    additional_criterion <- "identity"
    \(data, truth, ...) tibble::tibble(.estimate = 1)
  } else {
    check_prob_metric(additional_criterion)
  }
  rlang::check_number_whole(chains, min = 1)
  rlang::check_number_whole(iter_burnin, min = 1)
  rlang::check_number_whole(iter_retain, min = 1)
  check_double(comp_thresholds, min = 0, max = 1, allow_null = TRUE)

  # identify needed functions -----
  optimal_function <- get_optimal_function(optimal_method)
  weight_function <- get_weighting_function(weighting_method)

  # run chains -----
  inits <- if (chains == 1) 0.5 else seq(0.1, 0.9, length.out = chains)
  chain <- purrr::imap(
    inits,
    \(init, chain_id) {
      iterate_chain(
        estimates = estimates,
        init = init,
        chain_id = chain_id,
        optimal_function = optimal_function,
        weight_function = weight_function,
        iter_burnin = iter_burnin,
        iter_retain = iter_retain,
        ...
      )
    }
  ) |>
    purrr::list_rbind()

  draws <- draws_tibble(estimates, chain$truth)
  chain <- chain |>
    dplyr::mutate(
      conv = draw_criterion(draws, converge_function),
      .draw = dplyr::row_number()
    )

  # check for convergence -----
  diag_draws <- list(threshold = chain$threshold)
  if (additional_criterion != "identity") {
    diag_draws[[additional_criterion]] <- chain$conv
  }
  chain_diagnostics <- purrr::imap(
    diag_draws,
    \(x, parameter) {
      draws <- matrix(x, nrow = iter_retain, ncol = chains)
      tibble::tibble(
        parameter = parameter,
        rhat = posterior::rhat(draws),
        ess_bulk = posterior::ess_bulk(draws),
        ess_tail = posterior::ess_tail(draws)
      )
    }
  ) |>
    purrr::list_rbind()

  # return results -----
  results <- draw_performance(
    draws,
    thresholds = c(mean(chain$threshold), comp_thresholds),
    metrics = metrics
  ) |>
    dplyr::left_join(
      dplyr::select(chain, ".draw", "chain", "iteration", "conv"),
      by = ".draw"
    ) |>

    # clean-up
    dplyr::mutate(
      conv = dplyr::case_when(
        .data$.threshold %in% comp_thresholds ~ NA_real_,
        .default = .data$conv
      )
    ) |>
    dplyr::rename(!!additional_criterion := "conv") |>
    dplyr::select(
      "chain",
      "iteration",
      !!additional_criterion,
      ".threshold",
      dplyr::everything(),
      -".draw",
      -dplyr::any_of("identity")
    ) |>
    dplyr::summarize(
      dplyr::across(
        dplyr::where(is.double),
        \(x) posterior::rvar(x, nchains = chains)
      ),
      .threshold = posterior::E(.data$.threshold),
      .by = ".threshold"
    ) |>
    dplyr::relocate(".threshold", .before = 1) |>
    dplyr::rename(threshold = ".threshold")

  # print diagnostics and return -----
  attr(results, "diagnostics") <- chain_diagnostics
  if (any(chain_diagnostics$rhat > 1.01)) {
    attr(results, "converged") <- FALSE

    chain_diagnostics |>
      dplyr::filter(.data$rhat > 1.01) |>
      glue::glue_data(
        "Statistic did not converge (Rhat > 1.01): ",
        "{{.arg {paste(parameter)}}}"
      ) |>
      cli::cli_warn()
  } else {
    attr(results, "converged") <- TRUE
  }

  results
}


#' Run a single chain of the iteration process
#'
#' @inheritParams optimal_iterate
#' @param init The threshold used to generate the first set of "true" values.
#' @param chain_id An integer identifying the chain.
#' @param optimal_function The function used to calculate the optimal threshold.
#' @param weight_function The function used to generate weighted "true" values.
#'
#' @return A [tibble][tibble::tibble-package] with one row per retained
#'   iteration, containing the generated "true" values (`truth`) and the
#'   estimated optimal `threshold`.
#' @noRd
iterate_chain <- function(
  estimates,
  init,
  chain_id,
  optimal_function,
  weight_function,
  iter_burnin,
  iter_retain,
  ...
) {
  iter_threshold <- init

  tru <- vector(mode = "list", length = iter_retain)
  thr <- vector(mode = "double", length = iter_retain)
  for (i in seq_len(iter_burnin + iter_retain)) {
    iter_truth <- weight_function(estimates, threshold = iter_threshold, ...)
    iter_threshold <- optimal_function(
      estimates = estimates,
      truth = iter_truth
    )

    # burn-in iterations are discarded
    if (i <= iter_burnin) {
      next
    }

    tru[[i - iter_burnin]] <- iter_truth
    thr[i - iter_burnin] <- iter_threshold
  }

  tibble::tibble(
    chain = chain_id,
    iteration = seq_len(iter_retain),
    truth = tru,
    threshold = thr
  )
}
