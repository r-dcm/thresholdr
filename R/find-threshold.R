#' Find the optimal probability classification threshold
#'
#' @description
#' `find_threshold()` is an S7 generic that finds the optimal probability
#' classification threshold for a set of probabilities or an estimated
#' diagnostic classification model with one of the following classes:
#'
#' * `numeric`: A vector of probabilities (e.g., the probabilities of
#'   proficiency on a single attribute).
#' * `data.frame` or `matrix`: Probabilities for multiple attributes, with one
#'   column per attribute. All columns must be numeric, so remove any
#'   respondent identifiers first. Column names are used as the attribute
#'   names. Unnamed matrix columns are named `A1`, `A2`, etc.
#' * \pkg{measr}
#'   * `measrdcm`: Models estimated with [measr::dcm_estimate()].
#' * \pkg{GDINA}
#'   * `GDINA`: Models estimated with [GDINA::GDINA()].
#' * \pkg{CDM}
#'   * `din`: Models estimated with [CDM::din()].
#'   * `gdina`: Models estimated with [CDM::gdina()].
#'   * `gdm`: Models estimated with [CDM::gdm()]. All skills must be
#'     dichotomous (e.g., `theta.k = c(0, 1)`).
#'   * `mcdina`: Models estimated with [CDM::mcdina()].
#'
#' @inheritParams optimal_resample
#' @param x A numeric vector of probabilities, a data frame or matrix of
#'   probabilities with one column per attribute, or an estimated model object.
#' @param ... Additional arguments passed to the approximation function defined
#'   by `approx_method`.
#' @param approx_method The approximation method to use. One of `"resample"`,
#'   which will use [optimal_resample()] or `"iterate"`, which will use
#'   [optimal_iterate()].
#' @returns A [tibble][tibble::tibble-package] with one row per threshold and
#'   (if applicable) per attribute. For each attribute, the estimated optimal
#'   threshold is listed first, followed by any comparison thresholds. For a
#'   numeric vector, the result is the same as calling [optimal_resample()] or
#'   [optimal_iterate()] directly, and there is no `attribute` column.
#'
#'   When `approx_method = "iterate"`, the returned object also has the
#'   `"converged"` and `"diagnostics"` attributes described in
#'   [optimal_iterate()]. For multiple attributes, these are combined across
#'   attributes: `"converged"` is `TRUE` only if all attributes converged, and
#'   `"diagnostics"` includes an additional `attribute` column identifying the
#'   attribute for each row.
#' @export
find_threshold <- new_generic(
  "find_threshold",
  "x",
  function(
    x,
    ...,
    approx_method = c("resample", "iterate"),
    optimal_method = "youden"
  ) {
    approx_method <- rlang::arg_match(
      approx_method,
      values = approx_method_choices(),
      multiple = FALSE
    )
    optimal_method <- rlang::arg_match(
      optimal_method,
      values = optimal_method_choices(),
      multiple = FALSE
    )
    S7_dispatch()
  }
)

# methods ----------------------------------------------------------------------
method(find_threshold, class_numeric) <- function(
  x,
  ...,
  approx_method = c("resample", "iterate"),
  optimal_method = "youden"
) {
  # S7 dispatches numeric matrices on their base type, so handle them here
  if (is.matrix(x)) {
    return(
      .find_threshold_columns(
        x,
        ...,
        approx_method = approx_method,
        optimal_method = optimal_method
      )
    )
  }

  check_double(x, min = 0, max = 1, call = rlang::call2("find_threshold"))

  method_func <- get_approx_function(approx_method)

  method_func(estimates = x, optimal_method = optimal_method, ...)
}

## data frames and matrices ---------------------------------------------------
.find_threshold_columns <- function(
  x,
  ...,
  approx_method = c("resample", "iterate"),
  optimal_method = "youden"
) {
  check_attribute_probs(x, call = rlang::call2("find_threshold")) |>
    find_attribute_thresholds(
      approx_method = approx_method,
      optimal_method = optimal_method,
      ...
    )
}

# nolint start: object_name_linter.
method(find_threshold, class_data.frame) <- .find_threshold_columns
# nolint end

## measr -----------------------------------------------------------------------
measrdcm <- new_external_class("measr", "measrdcm")
method(find_threshold, measrdcm) <- function(
  x,
  ...,
  approx_method = c("resample", "iterate"),
  optimal_method = "youden"
) {
  measr::measr_extract(x, "attribute_prob") |>
    dplyr::select(dplyr::all_of(names(
      x@model_spec@qmatrix_meta$attribute_names
    ))) |>
    find_attribute_thresholds(
      approx_method = approx_method,
      optimal_method = optimal_method,
      ...
    )
}

## GDINA -----------------------------------------------------------------------
method(find_threshold, new_S3_class("GDINA")) <- function(
  x,
  ...,
  approx_method = c("resample", "iterate"),
  optimal_method = "youden"
) {
  att_names <- if (is.null(colnames(x$options$Q))) {
    paste0("A", seq_len(ncol(x$options$Q)))
  } else {
    colnames(x$options$Q)
  }

  GDINA::personparm(x, what = "mp") |>
    tibble::as_tibble(.name_repair = \(x) att_names) |>
    find_attribute_thresholds(
      approx_method = approx_method,
      optimal_method = optimal_method,
      ...
    )
}

# CDM --------------------------------------------------------------------------
.find_threshold_cdm <- function(
  x,
  ...,
  approx_method = c("resample", "iterate"),
  optimal_method = "youden"
) {
  cdm_attribute_probs(x, call = rlang::call2("find_threshold")) |>
    find_attribute_thresholds(
      approx_method = approx_method,
      optimal_method = optimal_method,
      ...
    )
}

method(find_threshold, new_S3_class("din")) <- .find_threshold_cdm
method(find_threshold, new_S3_class("gdina")) <- .find_threshold_cdm
method(find_threshold, new_S3_class("mcdina")) <- .find_threshold_cdm
method(find_threshold, new_S3_class("gdm")) <- .find_threshold_cdm
