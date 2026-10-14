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

  roc_mod
}


#' Calculate the points of a receiver operating characteristic curve
#'
#' A fast, internal alternative to [create_roc()] for use in the optimal
#' threshold methods. The returned `.threshold`, `sensitivity`, and
#' `specificity` values are identical to those from [yardstick::roc_curve()],
#' where estimates greater than or equal to the threshold are classified as
#' `1`.
#'
#' @inheritParams create_roc
#' @param call The call stack for the error message.
#'
#' @return A list with elements:
#'   * `.threshold`: The unique `estimates`, plus `-Inf` and `Inf`.
#'   * `sensitivity` and `specificity`: Values at each threshold.
#'   * `tp_gt` and `fp_gt`: The number of true and false positives at each
#'     threshold when only estimates strictly greater than the threshold are
#'     classified as `1`.
#'   * `n_pos` and `n_neg`: The number of `1` and `0` values in `truth`.
#' @noRd
roc_table <- function(estimates, truth, call = rlang::caller_env()) {
  ord <- order(estimates)
  est <- estimates[ord]
  tru <- truth[ord]

  n_pos <- sum(tru)
  n_neg <- length(tru) - n_pos
  if (n_pos == 0 || n_neg == 0) {
    cli::cli_abort(
      "{.arg truth} must contain both {.val {0}} and {.val {1}} values.",
      call = call
    )
  }
  cum_pos <- c(0, cumsum(tru))
  cum_neg <- c(0, cumsum(1 - tru))

  thresholds <- c(-Inf, unique(est), Inf)
  n_lt <- findInterval(thresholds, est, left.open = TRUE)
  n_le <- findInterval(thresholds, est)

  list(
    .threshold = thresholds,
    sensitivity = (n_pos - cum_pos[n_lt + 1]) / n_pos,
    specificity = 1 - ((n_neg - cum_neg[n_lt + 1]) / n_neg),
    tp_gt = n_pos - cum_pos[n_le + 1],
    fp_gt = n_neg - cum_neg[n_le + 1],
    n_pos = n_pos,
    n_neg = n_neg
  )
}


#' Calculate the area under a receiver operating characteristic curve
#'
#' A fast, internal alternative to [yardstick::roc_auc_vec()]. The area is
#' calculated using the trapezoidal rule, in the same way as yardstick, so the
#' result is identical.
#'
#' @param roc A list returned by `roc_table()`.
#'
#' @return A numeric scalar.
#' @noRd
roc_table_auc <- function(roc) {
  n <- length(roc$.threshold)
  dx <- roc$specificity[-1] - roc$specificity[-n]
  height <- (roc$sensitivity[-n] + roc$sensitivity[-1]) / 2
  sum(height * dx)
}
