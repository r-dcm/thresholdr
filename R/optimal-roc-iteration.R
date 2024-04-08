estimates <- dcm_probs$att1$estimate
optimal_method <- "topleft"
converge <- 0.0001
max_iter <- 1000
comp_thresholds <- NULL
metrics <- NULL

optimal_roc_iteration <- function(estimates, optimal_method, converge = 0.0001,
                                  max_iter = 1000,
                                  comp_thresholds = NULL, metrics = NULL) {
  # check inputs -----
  estimates <- check_double(estimates, lb = 0, ub = 1)
  optimal_method <- rlang::arg_match(optimal_method, optimal_method_choices())
  converge <- check_double(converge, lb = 0, inclusive = FALSE, exp_length = 1)
  comp_thresholds <- check_double(comp_thresholds, lb = 0, ub = 1,
                                  allow_null = TRUE)

  # identify needed functions -----
  optimal_function <- get_optimal_function(optimal_method)

  # initial state -----
  truth <- generate_truth(estimates)
  threshold <- optimal_function(estimates = estimates, truth = truth)
  auc <- yardstick::roc_auc(estimate_tibble(estimates, truth),
                            truth = "truth", "estimate",
                            event_level = "second")

  # iterate roc -----
  conv <- FALSE
  iter <- 1L
  while(!conv && iter < max_iter) {
    # NEED FUNCTION HERE
    # * Randomly generate truth, weighted by current value of `threshold`
    # * E.g., estimate of .8 should be less likely to give value of 1 if the
    #   `threshold` is greater than .5, but still more likely than .6, .7, etc.
    # iter_truth <- ...

    iter_threshold <- optimal_function(estimates = estimates,
                                       truth = iter_truth)
    iter_auc <- yardstick::roc_auc(estimate_tibble(estimates, iter_truth),
                                   truth = "truth", "estimate",
                                   event_level = "second")
    conv <- abs(iter_threshold - threshold) < converge &&
      abs(iter_auc - auc) < converge

    if (conv) {
      final_threshold <- iter_threshold
      final_auc <- iter_auc
    } else if (iter >= max_iter) {
      cli::cli_warn(paste("Convergence criteria was not met before reaching",
                          "{.arg max_iter}"))
      final_threshold <- iter_threshold
      final_auc <- iter_auc
    } else {
      threshold <- iter_threshold
      auc <- iter_auc
    }
  }
}
