#' Calculate a receiver operating characteristic curve
#'
#' Construct the full ROC curve given probability estimates and true
#' classifications.
#'
#' @param estimates A vector of classification probabilities. Values should
#'   represent the probability of `1` in the `truth` argument.
#' @param truth An integer vector of `0` and `1` representing the true
#'   classifications.
#'
#' @return An [roc_df][yardstick::roc_curve()] object.
#' @export
#'
#' @examples
#' create_roc(estimates = dcm_probs$att1$estimate, truth = dcm_probs$att1$truth)
create_roc <- function(estimates, truth) {
  # input checks -----
  estimates <- check_double(estimates, lb = 0, ub = 1)
  truth <- check_integer(truth, lb = 0L, ub = 1L,
                         exp_length = length(estimates))

  # create data -----
  roc_dat <- estimate_tibble(estimates, truth)
  roc_mod <- yardstick::roc_curve(roc_dat, truth = "truth", "estimate",
                                  event_level = "second")

  return(roc_mod)
}
