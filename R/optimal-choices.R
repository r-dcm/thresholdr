#' Methods for identifying optimal probability classification thresholds
#'
#' @return A vector of supported methods for identifying an optimal threshold.
#' @noRd
optimal_method_choices <- function() {
  c("youden", "topleft", "cz", "gmean")
}


#' Determine the function needed for each optimal threshold method
#'
#' @param method The method to use for finding the optimal threshold. Should be
#'   one of the values included in [optimal_method_choices()].
#'
#' @return The function that corresponds to the chosen method.
#' @noRd
get_optimal_function <- function(method) {
  rlang::arg_match(method, values = optimal_method_choices())

  switch(method,
         "youden" = calc_youden,
         "topleft" = calc_topleft,
         "cz" = calc_cz,
         "gmean" = calc_gmean,
         cli::cli_abort("No function found for {.arg method = \"{method}\"}"))
}


#' Methods for generating weighted proficiency classifications
#'
#' @return A vector of supported methods for weighting proficiency
#'   classifications.
#' @noRd
weighting_method_choices <- function() {
  c("beta", "distance")
}


#' Determine the function needed for each weighting method
#'
#' @param method The method to use for finding the optimal threshold. Should be
#'   one of the values included in [weighting_method_choices()].
#'
#' @return The function that corresponds to the chosen method.
#' @noRd
get_weighting_function <- function(method) {
  rlang::arg_match(method, values = weighting_method_choices())

  switch(method,
         "beta" = generate_beta_wt_truth,
         "distance" = generate_distance_wt_truth,
         cli::cli_abort("No function found for {.arg method = \"{method}\"}"))
}
