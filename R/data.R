#' Example probabilities from a diagnostic classification model
#'
#' Estimates of attribute mastery and true classification values for use in
#' demonstrating the functionality of the threshold
#'
#' @details
#' The data was simulated from the loglinear cognitive diagnostic model (LCDM;
#' Henson et al., 2009). In total, we simulated
#' 500 respondents taking 15 items, which combined to measure 3 attributes.
#' After simulating the data, an LCDM was estimated using
#' [measr](https://measr.info) (Thompson, 2023). This
#' data contains the probabilities of attribute proficiency from the estimated
#' model, as well as the true attribute classifications that were used to
#' generate the data.
#'
#' @format `dcm_probs` is a list contain true attribute classifications and
#' estimated probabilities of proficiency for the 3 attributes in the simulated
#' data. Each element is itself a list containing a vector of attribute
#' estimates and a vector of true classifications.
#' * `dcm_probs`
#'   * `att1`
#'     * `estimate`: A double vector of length `r length(dcm_probs[[1]][[1]])`
#'       containing the proficiency probabilities for attribute 1.
#'     * `truth`: An integer vector of length `r length(dcm_probs[[1]][[2]])`
#'       containing the true classifications for attribute 1.
#'   * `att2`
#'     * `estimate`: A double vector of length `r length(dcm_probs[[2]][[1]])`
#'       containing the proficiency probabilities for attribute 2.
#'     * `truth`: An integer vector of length `r length(dcm_probs[[2]][[2]])`
#'       containing the true classifications for attribute 2.
#'   * `att3`
#'     * `estimate`: A double vector of length `r length(dcm_probs[[3]][[1]])`
#'       containing the proficiency probabilities for attribute 3.
#'     * `truth`: An integer vector of length `r length(dcm_probs[[3]][[2]])`
#'       containing the true classifications for attribute 3.
#'
#' @references Henson, R. A., Templin, J. L., & Willse, J. T. (2009). Defining
#'   a family of cognitive diagnosis models using log-linear models with latent
#'   variables. *Psychometrika, 74*(2), 191-210.
#'   \doi{10.1007/s11336-008-9089-5}
#' @references Thompson, W. J. (2023). measr: Bayesian psychometric modeling
#'   using Stan. *Journal of Open Source Software, 8*(91).
#'   \doi{10.21105/joss.05742}
"dcm_probs"
