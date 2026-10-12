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
