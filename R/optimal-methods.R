#' Youden's J statistic
#'
#' Calculate the optimal probability classification threshold using Youden's J
#' statistic.
#'
#' @inheritParams create_roc
#'
#' @details
#' Youden's J statistic (Youden, 1950) is defined as `sensitivty` +
#' `specificity` - 1. As such, the value of the J-index ranges from \[0, 1\],
#' and is 1 when there are no false positive and no false negatives (i.e.,
#' sensitivity and specificity). The J-index is calculated for each possible
#' threshold defined by the ROC curve (e.g., [create_roc()]).  The optimality
#' criterion is then defined as:
#'
#' \deqn{\text{max}(sensitivity + specificity - 1)}
#'
#' @return A numeric scalar representing the optimal probability threshold.
#' @export
#' @family Optimal threshold methods
#'
#' @examples
#' calc_youden(estimates = dcm_probs$att1$estimate,
#'             truth = dcm_probs$att1$truth)
#'
#' calc_youden(estimates = dcm_probs$att2$estimate,
#'             truth = dcm_probs$att2$truth)
#'
#' calc_youden(estimates = dcm_probs$att3$estimate,
#'             truth = dcm_probs$att3$truth)
#'
#' @references Youden, W. J. (1950). Index for rating diagnostic tests.
#'   *Cancer, 3*(1), 32-35.
#'   \doi{10.1002/1097-0142(1950)3:1<32::AID-CNCR2820030106>3.0.CO;2-3}
calc_youden <- function(estimates, truth) {
  # input checks -----
  estimates <- check_double(estimates, lb = 0, ub = 1)
  truth <- check_integer(truth, lb = 0L, ub = 1L,
                         exp_length = length(estimates))

  # calculate threshold -----
  roc <- create_roc(estimates = estimates, truth = truth)

  yd <- roc |>
    dplyr::mutate(youden = .data$sensitivity + .data$specificity - 1) |>
    dplyr::slice_max(order_by = .data$youden, n = 1, with_ties = FALSE) |>
    dplyr::pull(".threshold")

  return(yd)
}


#' Top-left statistic
#'
#' Calculate the optimal probability classification threshold using the point
#' closest to the top-left corner of the ROC plot.
#'
#' @inheritParams create_roc
#'
#' @details
#' The top-left corner of an ROC plot represents perfect sensitivity and
#' specificity. The top-left statistic is determined by calculating the distance
#' from each point on the ROC curve (e.g., [create_roc()]) to the top-left
#' corner of the plot. The point on the plot that is the closest to the top-left
#' corner is the optimal threshold. The optimality criterion is then defined as:
#'
#' \deqn{\text{min}((1 - sensitivity)^2 + (1 - specificity)^2)}
#'
#' @return A numeric scalar representing the optimal probability threshold.
#' @export
#' @family Optimal threshold methods
#'
#' @examples
#' calc_topleft(estimates = dcm_probs$att1$estimate,
#'              truth = dcm_probs$att1$truth)
#'
#' calc_topleft(estimates = dcm_probs$att2$estimate,
#'              truth = dcm_probs$att2$truth)
#'
#' calc_topleft(estimates = dcm_probs$att3$estimate,
#'              truth = dcm_probs$att3$truth)
calc_topleft <- function(estimates, truth) {
  # input checks -----
  estimates <- check_double(estimates, lb = 0, ub = 1)
  truth <- check_integer(truth, lb = 0L, ub = 1L,
                         exp_length = length(estimates))

  # calculate threshold -----
  roc <- create_roc(estimates = estimates, truth = truth)

  tl <- roc |>
    dplyr::mutate(topleft = ((1 - .data$sensitivity) ^ 2) +
                    ((1 - .data$specificity) ^ 2)) |>
    dplyr::slice_min(order_by = .data$topleft, n = 1, with_ties = FALSE) |>
    dplyr::pull(".threshold")

  return(tl)
}
