# numeric method checks inputs

    Code
      find_threshold(c(0.2, 1.4, 0.7))
    Condition
      Error in `find_threshold()`:
      ! `x` must be a number between 0 and 1, not the number 1.4.
    Code
      find_threshold(c(0.2, NA, 0.7))
    Condition
      Error in `find_threshold()`:
      ! `x` must be a number, not a numeric `NA`.
    Code
      find_threshold(c(0.2, 0.7), approx_method = "bootstrap")
    Condition
      Error in `find_threshold()`:
      ! `approx_method` must be one of "resample" or "iterate", not "bootstrap".
    Code
      find_threshold(c(0.2, 0.7), optimal_method = "accuracy")
    Condition
      Error in `find_threshold()`:
      ! `optimal_method` must be one of "youden", "topleft", "cz", "gmean", "iu", or "minp", not "accuracy".

# data frame and matrix methods check inputs

    Code
      find_threshold(probs)
    Condition
      Error in `find_threshold()`:
      ! All columns of `x` must be numeric probabilities.
      x Non-numeric column: `id`.
      i Remove any respondent identifiers before finding thresholds.
    Code
      find_threshold(probs[, c("att1", "att2")])
    Condition
      Error in `find_threshold()`:
      ! `x$att2` must be a number between 0 and 1, not the number 1.3.
    Code
      find_threshold(as.matrix(probs[, c("att1", "att2")]))
    Condition
      Error in `find_threshold()`:
      ! `x[, "att2"]` must be a number between 0 and 1, not the number 1.3.
    Code
      find_threshold(probs[, 0])
    Condition
      Error in `find_threshold()`:
      ! `x` must have at least one column.
    Code
      find_threshold(data.frame(a = c(0.1, 0.9), a = c(0.2, 0.8), check.names = FALSE))
    Condition
      Error in `find_threshold()`:
      ! Column names of `x` must be unique.
      x Duplicated name: "a".

# CDM gdm method works

    Code
      find_threshold(gdm_poly)
    Condition
      Error in `find_threshold()`:
      ! All skills in a <gdm> model must have exactly two levels.
      x Skills without two levels: F1 (3 levels), F2 (3 levels), and F3 (3 levels).
      i Estimate the model with `theta.k = c(0, 1)`.

