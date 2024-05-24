# estimates <- dcm_probs$att1$estimate
# optimal_method <- "topleft"
# converge <- 0.0001
# additional_criteria <- "roc_auc"
# max_iter <- 1000
# comp_thresholds <- NULL
# metrics <- NULL

optimal_roc_iteration <- function(estimates, optimal_method, converge = 0.0001,
                                  additional_criteria = NULL, max_iter = 1000,
                                  comp_thresholds = NULL, metrics = NULL) {
  # check inputs -----
  estimates <- check_double(estimates, lb = 0, ub = 1)
  optimal_method <- rlang::arg_match(optimal_method, optimal_method_choices())
  converge <- check_double(converge, lb = 0, inclusive = FALSE, exp_length = 1)
  converge_function <- if (is.null(additional_criteria)) {
    additional_criteria <- "identity"
    \(data, truth, ...) tibble::tibble(.estimate = 1)
  } else {
    check_prob_metric(additional_criteria)
  }
  max_iter <- check_integer(max_iter, lb = 0, inclusive = FALSE, exp_length = 1)
  comp_thresholds <- check_double(comp_thresholds, lb = 0, ub = 1,
                                  allow_null = TRUE)

  # identify needed functions -----
  optimal_function <- get_optimal_function(optimal_method)

  # initial state -----
  truth <- generate_truth(estimates)
  threshold <- optimal_function(estimates = estimates, truth = truth)
  conv <- converge_function(estimate_tibble(estimates, truth),
                            truth = "truth", "estimate",
                            event_level = "second") |>
    dplyr::pull(".estimate")

  # iterate roc -----
  has_converged <- FALSE
  iter <- 1L
  while(!has_converged && iter <= max_iter) {
    # iter_truth <- generate_truth(dist_thresh(estimate_tibble(estimates, truth), threshold))
    iter_truth <- generate_weighted_truth(estimates, mu = threshold,
                                          phi = 5)

    iter_threshold <- optimal_function(estimates = estimates,
                                       truth = iter_truth)
    iter_conv <- converge_function(estimate_tibble(estimates, iter_truth),
                                   truth = "truth", "estimate",
                                   event_level = "second") |>
      dplyr::pull(".estimate")
    has_converged <- abs(iter_threshold - threshold) < converge &&
      abs(iter_conv - conv) < converge

    if (has_converged) {
      final_threshold <- iter_threshold
      final_conv <- iter_conv
      final_truth <- iter_truth
    } else if (iter == max_iter) {
      cli::cli_warn(paste("Convergence criteria was not met before reaching",
                          "{.arg max_iter}"))
      final_threshold <- iter_threshold
      final_conv <- iter_conv
      break()
    } else {
      threshold <- iter_threshold
      conv <- iter_conv
      iter <- iter + 1
    }
  }
}
