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
  check_double(estimates, min = 0, max = 1)
  check_integer(truth, min = 0, max = 1, exp_length = length(estimates))

  # create data -----
  roc_dat <- estimate_tibble(estimates, truth)
  roc_mod <- yardstick::roc_curve(
    roc_dat,
    truth = "truth",
    "estimate",
    event_level = "second"
  )

  return(roc_mod)
}
