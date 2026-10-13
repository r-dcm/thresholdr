#' Extract attribute probabilities from a diagnostic model
#'
#' @param x A model estimated with [CDM::din()], [CDM::gdina()],
#'   [CDM::mcdina()], or [CDM::gdm()].
#' @param call The call stack for the error message.
#'
#' @return A matrix with one row per respondent and one column per attribute,
#'   containing the marginal probability that each respondent has mastered each
#'   attribute. Columns are named using the column names of the Q-matrix (or
#'   `theta.k` for [CDM::gdm()] models).
#' @noRd
cdm_attribute_probs <- function(x, call = rlang::caller_env()) {
  if (inherits(x, "gdm")) {
    # gdm models do not retain the Q-matrix names; skills are named after the
    # columns of `theta.k` (F1, F2, ..., unless a named matrix was supplied)
    theta <- as.matrix(x$theta.k)
    n_levels <- apply(theta, 2, \(lv) length(unique(lv)))
    bad_skills <- n_levels[n_levels != 2]
    if (length(bad_skills) > 0) {
      cli::cli_abort(
        c(
          "All skills in a {.cls gdm} model must have exactly two levels.",
          x = paste(
            "Skills without two levels:",
            "{paste0(names(bad_skills), ' (', bad_skills, ' levels)')}."
          ),
          i = "Estimate the model with {.code theta.k = c(0, 1)}."
        ),
        call = call
      )
    }

    # mastery is defined as the upper level of each skill
    mastery <- apply(theta, 2, \(lv) as.numeric(lv == max(lv)))
    att_names <- colnames(theta)
    probs <- x$posterior %*% mastery
  } else if (inherits(x, "mcdina")) {
    # `CDM::IRT.factor.scores()` returns MLE classes for mcdina models, even
    # when `type = "EAP"` is requested, so use the EAP estimates directly
    att_names <- setdiff(colnames(x$q.matrix), c("item", "categ"))
    probs <- as.matrix(x$EAP.class)
  } else {
    att_names <- colnames(x$q.matrix)
    probs <- as.matrix(
      x$pattern[, paste0("post.attr", seq_len(ncol(x$q.matrix)))]
    )
  }

  if (is.null(att_names)) {
    att_names <- paste0("A", seq_len(ncol(probs)))
  }
  colnames(probs) <- att_names
  rownames(probs) <- NULL
  probs
}


#' Find the optimal threshold for each attribute
#'
#' @param probs A data frame or matrix with one column per attribute,
#'   containing the probabilities of proficiency for each respondent. Columns
#'   must be named with the attribute names.
#' @inheritParams find_threshold
#'
#' @return A [tibble][tibble::tibble-package] with one row per threshold per
#'   attribute. See `combine_attribute_thresholds()` for details on the returned
#'   attributes.
#' @noRd
find_attribute_thresholds <- function(
  probs,
  ...,
  approx_method,
  optimal_method
) {
  method_func <- get_approx_function(approx_method)

  # convergence warnings are consolidated across attributes below
  results <- withCallingHandlers(
    apply(
      probs,
      MARGIN = 2,
      FUN = method_func,
      optimal_method = optimal_method,
      ...,
      simplify = FALSE
    ),
    thresholdr_warning_convergence = \(cnd) invokeRestart("muffleWarning")
  ) |>
    combine_attribute_thresholds()

  if (isFALSE(attr(results, "converged"))) {
    warn_attribute_convergence(attr(results, "diagnostics"))
  }

  results
}


#' Combine the thresholds estimated for each attribute
#'
#' @param results A named list of [tibble][tibble::tibble-package] objects
#'   returned by [optimal_resample()] or [optimal_iterate()], one per attribute.
#'
#' @return A [tibble][tibble::tibble-package] with the `results` combined by
#'   row, with an additional `attribute` column. If the `results` include
#'   convergence diagnostics (i.e., from [optimal_iterate()]), the returned
#'   object also has two attributes:
#'   * `"converged"`: A logical indicating whether all monitored statistics for
#'     all attributes have converged.
#'   * `"diagnostics"`: A [tibble][tibble::tibble-package] with the diagnostics
#'     for each attribute, with an additional `attribute` column.
#' @noRd
combine_attribute_thresholds <- function(results) {
  combined <- purrr::list_rbind(results, names_to = "attribute")

  diagnostics <- purrr::map(results, \(x) attr(x, "diagnostics"))
  if (all(purrr::map_lgl(diagnostics, is.null))) {
    return(combined)
  }

  attr(combined, "diagnostics") <- purrr::list_rbind(
    diagnostics,
    names_to = "attribute"
  )
  attr(combined, "converged") <- all(
    purrr::map_lgl(results, \(x) isTRUE(attr(x, "converged")))
  )

  combined
}


#' Warn about attributes that did not converge
#'
#' @param diagnostics A [tibble][tibble::tibble-package] of combined
#'   diagnostics, as returned in the `"diagnostics"` attribute of
#'   `combine_attribute_thresholds()`.
#'
#' @return Called for its side effect of issuing a single warning, created by
#'   [cli::cli_warn()], that lists the statistics that did not converge for
#'   each attribute.
#' @noRd
warn_attribute_convergence <- function(diagnostics) {
  not_converged <- diagnostics |>
    dplyr::filter(!.data$converged) |>
    dplyr::summarize(parameter = list(.data$parameter), .by = "attribute")

  details <- purrr::map2_chr(
    not_converged$attribute,
    not_converged$parameter,
    \(att, par) cli::format_inline("{.val {att}}: {.arg {par}}")
  )
  # escape braces so cli does not re-interpolate the formatted details
  details <- gsub("([{}])", "\\1\\1", details)

  cli::cli_warn(
    c(
      paste(
        "Statistics did not converge (Rhat > 1.01) for",
        "{length(details)} attribute{?s}:"
      ),
      rlang::set_names(details, rep("*", length(details))),
      i = "See {.code attr(x, \"diagnostics\")} for details."
    ),
    class = "thresholdr_warning_convergence"
  )
}
