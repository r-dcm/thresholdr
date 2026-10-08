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
#' @family optimal threshold methods
#' @concept J
#'
#' @examples
#' calc_youden(
#'   estimates = dcm_probs$att1$estimate,
#'   truth = dcm_probs$att1$truth
#' )
#'
#' calc_youden(
#'   estimates = dcm_probs$att2$estimate,
#'   truth = dcm_probs$att2$truth
#' )
#'
#' calc_youden(
#'   estimates = dcm_probs$att3$estimate,
#'   truth = dcm_probs$att3$truth
#' )
#'
#' @references Youden, W. J. (1950). Index for rating diagnostic tests.
#'   *Cancer, 3*(1), 32-35.
#'   \doi{10.1002/1097-0142(1950)3:1<32::AID-CNCR2820030106>3.0.CO;2-3}
calc_youden <- function(estimates, truth) {
  # input checks -----
  check_double(estimates, min = 0, max = 1)
  check_integer(truth, min = 0, max = 1, exp_length = length(estimates))

  # calculate threshold -----
  roc <- create_roc(estimates = estimates, truth = truth)

  yd <- roc |>
    dplyr::mutate(youden = .data$sensitivity + .data$specificity - 1) |>
    dplyr::slice_max(order_by = .data$youden, n = 1, with_ties = FALSE) |>
    dplyr::pull(".threshold")

  yd
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
#' @family optimal threshold methods
#'
#' @examples
#' calc_topleft(
#'   estimates = dcm_probs$att1$estimate,
#'   truth = dcm_probs$att1$truth
#' )
#'
#' calc_topleft(
#'   estimates = dcm_probs$att2$estimate,
#'   truth = dcm_probs$att2$truth
#' )
#'
#' calc_topleft(
#'   estimates = dcm_probs$att3$estimate,
#'   truth = dcm_probs$att3$truth
#' )
#'
#' @references Perkins, N. J., and Schisterman, E. F. (2006). The inconsistency
#'   of "optimal" cut-points using two ROC based criteria. *American Journal of
#'   Epidemiology, 163*(7), 670-675. \doi{10.1093/aje/kwj063}
calc_topleft <- function(estimates, truth) {
  # input checks -----
  check_double(estimates, min = 0, max = 1)
  check_integer(truth, min = 0, max = 1, exp_length = length(estimates))

  # calculate threshold -----
  roc <- create_roc(estimates = estimates, truth = truth)

  tl <- roc |>
    dplyr::mutate(
      topleft = ((1 - .data$sensitivity)^2) +
        ((1 - .data$specificity)^2)
    ) |>
    dplyr::slice_min(order_by = .data$topleft, n = 1, with_ties = FALSE) |>
    dplyr::pull(".threshold")

  tl
}


#' Concordance probability method
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
#' @family optimal threshold methods
#'
#' @examples
#' calc_cz(
#'   estimates = dcm_probs$att1$estimate,
#'   truth = dcm_probs$att1$truth
#' )
#'
#' calc_cz(
#'   estimates = dcm_probs$att2$estimate,
#'   truth = dcm_probs$att2$truth
#' )
#'
#' calc_cz(
#'   estimates = dcm_probs$att3$estimate,
#'   truth = dcm_probs$att3$truth
#' )
#'
#' @references Liu, X. (2012). Classification accuracy and cut point selection.
#'   *Statistics in Medicine, 31*(23), 2676-2686.
#'   \doi{10.1002/sim.4509}
calc_cz <- function(estimates, truth) {
  # input checks -----
  check_double(estimates, min = 0, max = 1)
  check_integer(truth, min = 0, max = 1, exp_length = length(estimates))

  # calculate threshold -----
  roc <- create_roc(estimates = estimates, truth = truth)

  cz <- roc |>
    dplyr::mutate(cz = .data$sensitivity * .data$specificity) |>
    dplyr::slice_max(order_by = .data$cz, n = 1, with_ties = FALSE) |>
    dplyr::pull(".threshold")

  cz
}


#' Index of union method
#'
#' Calculate the optimal probability classification threshold using the
#' index of union method (IU).
#'
#' @inheritParams create_roc
#'
#' @details
#' The index of union method (Unal, 2017) is defines the optimal cut point as
#' the point that minimizes the summed absolute differences between the area
#' under the curve (AUC) and the sensitivity and specificity.
#'
#' The optimality criterion is defined as:
#'
#' \deqn{\text{min}(|sensitivity - AUC| + |specificity - AUC|)}
#'
#' @return A numeric scalar representing the optimal probability threshold.
#' @export
#' @family optimal threshold methods
#'
#' @examples
#' calc_iu(
#'   estimates = dcm_probs$att1$estimate,
#'   truth = dcm_probs$att1$truth
#' )
#'
#' calc_iu(
#'   estimates = dcm_probs$att2$estimate,
#'   truth = dcm_probs$att2$truth
#' )
#'
#' calc_iu(
#'   estimates = dcm_probs$att3$estimate,
#'   truth = dcm_probs$att3$truth
#' )
#'
#' @references Unal, I. (2017). Defining an optimal cut-point value in ROC
#'   analysis: An alternative approach. *Computational and Mathematical Methods
#'   in Medicine*, Article 3762651. \doi{10.1155/2017/3762651}
calc_iu <- function(estimates, truth) {
  # input checks -----
  check_double(estimates, min = 0, max = 1)
  check_integer(truth, min = 0, max = 1, exp_length = length(estimates))

  # calculate threshold -----
  auc <- yardstick::roc_auc_vec(
    truth = factor(truth),
    estimate = estimates,
    event_level = "second"
  )
  roc <- create_roc(estimates = estimates, truth = truth)

  iu <- roc |>
    dplyr::mutate(
      iu = abs(.data$sensitivity - auc) + abs(.data$specificity - auc)
    ) |>
    dplyr::slice_min(order_by = .data$iu, n = 1, with_ties = FALSE) |>
    dplyr::pull(".threshold")

  iu
}


#' G-mean method
#'
#' Calculate the optimal probability classification threshold using the
#' G-Mean method.
#'
#' @inheritParams create_roc
#'
#' @details
#' The G-mean method (Kubat & Matwin, 1997) is defined as the square root of the
#' product of sensitivity and specificity at a given threshold. The optimal
#' threshold is the threshold with the greatest g-mean.
#'
#' The optimality criterion is then defined as:
#'
#' \deqn{\text{max}\sqrt{(sensitivity * specificity)}}
#'
#' @return A numeric scalar representing the optimal probability threshold.
#' @export
#' @family optimal threshold methods
#'
#' @examples
#' calc_gmean(
#'   estimates = dcm_probs$att1$estimate,
#'   truth = dcm_probs$att1$truth
#' )
#'
#' calc_gmean(
#'   estimates = dcm_probs$att2$estimate,
#'   truth = dcm_probs$att2$truth
#' )
#'
#' calc_gmean(
#'   estimates = dcm_probs$att3$estimate,
#'   truth = dcm_probs$att3$truth
#' )
#'
#' @references Kubat, M. & Matwin, S. (1997, July 8-12). *Addressing the curse
#' of imbalanced training sets: One-sided selection* \[Paper presentation\].
#' International Conference on Machine Learning, Nashville, TN.
calc_gmean <- function(estimates, truth) {
  # input checks -----
  check_double(estimates, min = 0, max = 1)
  check_integer(truth, min = 0, max = 1, exp_length = length(estimates))

  # calculate threshold -----
  roc <- create_roc(estimates = estimates, truth = truth)

  g <- roc |>
    dplyr::mutate(g = sqrt(.data$sensitivity * .data$specificity)) |>
    dplyr::slice_max(order_by = .data$g, n = 1, with_ties = FALSE) |>
    dplyr::pull(".threshold")

  g
}

#' Minimum p-value method
#'
#' Calculate the optimal probability classification threshold using the
#' minimum p-value method.
#'
#' @inheritParams create_roc
#'
#' @details
#' The minimum p-value method (Miller & Siegmund, 1982; Rota & Antolini, 2014)
#' attempts to identify the cut-point that best separates the two groups
#' according to the maximum achievable Chi-square statistic. For each possible
#' threshold, a 2x2 contingency table is created from the true status and the
#' predicted status, given the current threshold. We then calculate a 1-degree
#' of freedom Chi-square test on the association between the true and predicted
#' statuses, where a large test statistic (and therefore small p-value) indicate
#' there is an association.
#'
#' The optimality criterion is then defined as the threshold that results in the
#' maximum Chi-square statistic or, conversely, the minimum p-value.
#'
#' @return A numeric scalar representing the optimal probability threshold.
#' @export
#' @family optimal threshold methods
#'
#' @examples
#' calc_minp(
#'   estimates = dcm_probs$att1$estimate,
#'   truth = dcm_probs$att1$truth
#' )
#'
#' calc_minp(
#'   estimates = dcm_probs$att2$estimate,
#'   truth = dcm_probs$att2$truth
#' )
#'
#' calc_minp(
#'   estimates = dcm_probs$att3$estimate,
#'   truth = dcm_probs$att3$truth
#' )
#'
#' @references Miller, R., & Siegmund, D. (1982). Maximally selected Chi square
#'   statistics. *Biometrics, 38*(4), 1011-1016. \doi{10.2307/2529881}
#' @references Rota, M., & Antolini, L. (2014). Finding the optimal cut-point
#'   for Gaussian and Gamma distributed biomarkers. *Computational Statistics
#'   and Data Analysis, 69*, 1-14. \doi{10.1016/j.csda.2013.07.015}
calc_minp <- function(estimates, truth) {
  # input checks -----
  check_double(estimates, min = 0, max = 1)
  check_integer(truth, min = 0, max = 1, exp_length = length(estimates))

  # calculate threshold -----
  roc <- create_roc(estimates = estimates, truth = truth)

  roc |>
    dplyr::mutate(
      minp = vapply(
        .data$.threshold,
        roc_chisq,
        double(1),
        estimates = estimates,
        truth = truth
      )
    ) |>
    dplyr::slice_max(order_by = .data$minp, n = 1, with_ties = FALSE) |>
    dplyr::pull(".threshold")
}
