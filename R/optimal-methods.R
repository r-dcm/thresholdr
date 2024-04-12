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


#' Concordance Probability Method
#'
#' Calculate the optimal probability classification threshold using the 
#' concordance probability method (CZ). 
#' 
#' @inheritParams create_roc
#'
#' @details
#' The concordance probability method (Liu, 2012) is defined as the product of 
#' the sensitivity and specificity at a given threshold cut-point. By utilizing 
#' the product of both metrics, the value for the cut-point remains within the 
#' range of \[0, 1\]. The optimal threshold is the cut-point that maximizes 
#' the area of the rectangle related to the ROC curve.
#' 
#' The optimality criterion is then defined as:
#'
#' \deqn{\text{max}(sensitivity * specificity)}
#'
#' @return A numeric scalar representing the optimal probability threshold.
#' @export
#' @family Optimal threshold methods
#'
#' @examples
#' calc_cz(estimates = dcm_probs$att1$estimate,
#'             truth = dcm_probs$att1$truth)
#'
#' calc_cz(estimates = dcm_probs$att2$estimate,
#'             truth = dcm_probs$att2$truth)
#'
#' calc_cz(estimates = dcm_probs$att3$estimate,
#'             truth = dcm_probs$att3$truth)
#'
#' @references Liu, X. (2012). Classification accuracy and cut point selection.
#'   *Statistics in Medicine, 31*(23), 2676-2686.
#'   \doi{10.1002/sim.4509}
calc_cz <- function(estimates, truth) {
  # input checks -----
  estimates <- check_double(estimates, lb = 0, ub = 1)
  truth <- check_integer(truth, lb = 0L, ub = 1L,
                         exp_length = length(estimates))

  # calculate threshold -----
  roc <- create_roc(estimates = estimates, truth = truth)

  cz <- roc |>
    dplyr::mutate(cz = .data$sensitivity * .data$specificity) |>
    dplyr::slice_max(order_by = .data$cz, n = 1, with_ties = FALSE) |>
    dplyr::pull(".threshold")
  
  return(cz)
}


#' G-Mean Method
#'
#' Calculate the optimal probability classification threshold using the
#' G-Mean method. 
#' 
#' @inheritParams create_roc
#'
#' @details
#' The G-mean method (Kubat & Matwin, 1997) is defined as the square root of the product of sensitivity and specificity
#' at a given threshold. The optimal threshold is the threshold with the greatest g-mean.
#' 
#' The optimality criterion is then defined as:
#'
#' \deqn{\text{max}\sqrt{(sensitivity * specificity)}}
#'
#' @return A numeric scalar representing the optimal probability threshold.
#' @export
#' @family Optimal threshold methods
#'
#' @examples
#' calc_gmean(estimates = dcm_probs$att1$estimate,
#'             truth = dcm_probs$att1$truth)
#'
#' calc_gmean(estimates = dcm_probs$att2$estimate,
#'             truth = dcm_probs$att2$truth)
#'
#' calc_gmean(estimates = dcm_probs$att3$estimate,
#'             truth = dcm_probs$att3$truth)
#'
#' @references Kubat, M. & Matwin, S. (1997). Addressing the curse of imbalanced
#' training sets: One-sided selection. *Icml 97*(1), 179.
calc_gmean <- function(estimates, truth) {
  # input checks -----
  estimates <- check_double(estimates, lb = 0, ub = 1)
  truth <- check_integer(truth, lb = 0L, ub = 1L,
                         exp_length = length(estimates))

  # calculate threshold -----
  roc <- create_roc(estimates = estimates, truth = truth)

  g <- roc |>
    dplyr::mutate(g = sqrt(.data$sensitivity * .data$specificity)) |>
    dplyr::slice_max(order_by = .data$g, n = 1, with_ties = FALSE) |>
    dplyr::pull(".threshold")
  
  return(g)
}