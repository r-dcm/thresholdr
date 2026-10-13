#' Methods for identifying optimal probability classification thresholds
#'
#' @return A vector of supported methods for identifying an optimal threshold.
#' @noRd
optimal_method_choices <- function() {
  c("youden", "topleft", "cz", "gmean", "iu", "minp")
}


#' Determine the function needed for each optimal threshold method
#'
#' @param method The method to use for finding the optimal threshold. Should be
#'   one of the values included in `optimal_method_choices()`.
#'
#' @return The function that corresponds to the chosen method.
#' @noRd
get_optimal_function <- function(method) {
  rlang::arg_match(method, values = optimal_method_choices())

  switch(
    method,
    "youden" = calc_youden,
    "topleft" = calc_topleft,
    "cz" = calc_cz,
    "gmean" = calc_gmean,
    "iu" = calc_iu,
    "minp" = calc_minp,
    cli::cli_abort("No function found for {.arg method = \"{method}\"}")
  )
}


#' Methods for generating weighted proficiency classifications
#'
#' @return A vector of supported methods for weighting proficiency
#'   classifications.
#' @noRd
weighting_method_choices <- function() {
  c("beta", "distance", "logodds")
}


#' Determine the function needed for each weighting method
#'
#' @param method The method to use for finding the optimal threshold. Should be
#'   one of the values included in `weighting_method_choices()`.
#'
#' @return The function that corresponds to the chosen method.
#' @noRd
get_weighting_function <- function(method) {
  rlang::arg_match(method, values = weighting_method_choices())

  switch(
    method,
    "beta" = generate_beta_wt_truth,
    "distance" = generate_distance_wt_truth,
    "logodds" = generate_logodds_wt_truth,
    cli::cli_abort("No function found for {.arg method = \"{method}\"}")
  )
}


#' Methods for approximating the optimal threshold
#'
#' @return A vector of supported methods for approximating an optimal threshold
#'   when true classifications are unknown.
#' @noRd
approx_method_choices <- function() {
  c("resample", "iterate")
}


#' Determine the function needed for each approximation method
#'
#' @param method The method to use for approximating the optimal threshold.
#'   Should be one of the values included in `approx_method_choices()`.
#'
#' @return The function that corresponds to the chosen method.
#' @noRd
get_approx_function <- function(method) {
  rlang::arg_match(method, values = approx_method_choices())

  switch(
    method,
    "resample" = optimal_resample,
    "iterate" = optimal_iterate,
    cli::cli_abort("No function found for {.arg method = \"{method}\"}")
  )
}
