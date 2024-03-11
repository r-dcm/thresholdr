#' Send an error message for an unexpected argument input
#'
#' @param arg The name of the argument.
#' @param must The requirement for input values that is not met.
#' @param not The current state of `argument` that is problematic.
#' @param extra Additional text to add to the error message.
#' @param custom A custom error message to override the defaul message of `must`
#'   + `not` + `extra`.
#' @param call The call stack.
#'
#' @return An error message created by [cli::cli_abort()].
#' @noRd
abort_bad_argument <- function(arg, must, not = NULL, extra = NULL,
                               custom = NULL, call) {
  msg <- glue::glue("`{arg}` must {must}")
  if (!is.null(not)) {
    msg <- glue::glue("{msg}; not {not}")
  }
  if (!is.null(extra)) {
    msg <- c(msg, extra)
  }
  if (!is.null(custom)) {
    msg <- custom
  }

  cli::cli_abort(msg, call = call)
}


#' Check numeric (double) value
#'
#' @param x The input value to be checked.
#' @param lb The acceptable lower bound.
#' @param ub The acceptable upper bound.
#' @param inclusive Logical. Are the `lb` and `ub` inclusive?
#' @param allow_null Logical. Are `NULL` values allowed?
#' @param allow_missing Logical. Are `NA` values allowed?
#' @param exp_length The expected value of `length(x)`. If `NULL`, any length is
#'   accepted. If multiple lengths are acceptable, a vector can be specified
#'   (e.g., `exp_length = c(1, 10)`).
#' @param arg The name of the argument for the error message.
#' @param call The call stack for the error message.
#'
#' @return If all requirements are met, `x` is returned. If requirements are not
#'   met, an informative error message is returned by [abort_bad_argument()].
#' @noRd
check_double <- function(x, lb = -Inf, ub = Inf, inclusive = TRUE,
                         allow_null = FALSE, allow_missing = FALSE,
                         exp_length = NULL,
                         arg = rlang::caller_arg(x),
                         call = rlang::caller_env()) {
  if (is.null(x) && allow_null) return(x)

  if (inclusive) {
    check_lb <- function(.x, .y) .x < .y
    check_ub <- function(.x, .y) .x > .y
  } else if (!inclusive) {
    check_lb <- function(.x, .y) .x <= .y
    check_ub <- function(.x, .y) .x >= .y
  }

  if (!is.numeric(x)) {
    abort_bad_argument(arg = arg, must = "be of type numeric", not = typeof(x),
                       call = call)
  }

  if (!allow_missing && any(is.na(x))) {
    abort_bad_argument(arg = arg, must = "be non-missing", call = call)
  }

  if (!is.null(exp_length) && !(length(x) %in% exp_length)) {
    abort_bad_argument(
      arg = arg,
      must = glue::glue("be of length ",
                        "{knitr::combine_words(exp_length, and = ' or ')}"),
      not = length(x),
      call = call
    )
  }

  if (any(vapply(x, check_lb, logical(1), lb)) ||
        any(vapply(x, check_ub, logical(1), ub))) {
    msg <- if (is.infinite(lb)) {
      glue::glue("be less than {ub}")
    } else if (is.infinite(ub)) {
      glue::glue("be greater than {lb}")
    } else {
      glue::glue("be between {lb} and {ub}")
    }
    abort_bad_argument(arg = arg, must = msg, call = call)
  }

  x
}


#' Check integer value
#'
#' @param x The input value to be checked.
#' @param lb The acceptable lower bound.
#' @param ub The acceptable upper bound.
#' @param inclusive Logical. Are the `lb` and `ub` inclusive?
#' @param allow_null Logical. Are `NULL` values allowed?
#' @param allow_missing Logical. Are `NA` values allowed?
#' @param exp_length The expected value of `length(x)`. If `NULL`, any length is
#'   accepted. If multiple lengths are acceptable, a vector can be specified
#'   (e.g., `exp_length = c(1, 10)`).
#' @param arg The name of the argument for the error message.
#' @param call The call stack for the error message.
#'
#' @return If all requirements are met, `x` is returned. If `x` was supplied as
#'   a double, it is coerced to an integer before being returned. If
#'   requirements are not met, an informative error message is returned by
#'   [abort_bad_argument()].
#' @noRd
check_integer <- function(x, lb = -Inf, ub = Inf, inclusive = TRUE,
                          allow_null = FALSE, allow_missing = FALSE,
                          exp_length = NULL,
                          arg = rlang::caller_arg(x),
                          call = rlang::caller_env()) {
  if (is.null(x) && allow_null) return(x)

  if (inclusive) {
    check_lb <- lb
    check_ub <- ub
  } else if (!inclusive) {
    check_lb <- lb + 1L
    check_ub <- ub - 1L
  }

  if (!is.numeric(x)) {
    abort_bad_argument(arg = arg, must = "be of type numeric", not = typeof(x),
                       call = call)
  }
  x_int <- as.integer(x)

  if (!allow_missing && any(is.na(x_int))) {
    abort_bad_argument(arg = arg, must = "be non-missing", call = call)
  }

  if (!is.null(exp_length) && !(length(x_int) %in% exp_length)) {
    abort_bad_argument(
      arg = arg,
      must = glue::glue("be of length ",
                        "{knitr::combine_words(exp_length, and = ' or ')}"),
      not = length(x),
      call = call
    )
  }

  if (any(x != x_int)) {
    msg <- if (length(x_int) == 1) {
      "be an integer value"
    } else {
      "be integer values"
    }

    abort_bad_argument(arg = arg, must = msg, call = call)
  }

  if (any(x_int < check_lb) || any(x_int > check_ub)) {
    msg <- if (is.infinite(lb)) {
      glue::glue("be less than {ub}")
    } else if (is.infinite(ub)) {
      glue::glue("be greater than {lb}")
    } else {
      glue::glue("be between {lb} and {ub}")
    }
    abort_bad_argument(arg = arg, must = msg, call = call)
  }

  x_int
}


#' Check class probability metric
#'
#' @param x A character vector. Should be the name of a class probability metric
#'  from the yardstick package (e.g., `"roc_auc"`, `"brier_class"`).
#' @param arg The argument name for the error message.
#' @param call The call stack for the error mesasge.
#'
#' @return If `x` is the name of a valid probability metric from yardstick, that
#'   function is returned. Otherwise, an error message is returned with
#'   [cli::cli_abort()].
#' @noRd
check_prob_metric <- function(x,
                              arg = rlang::caller_arg(x),
                              call = rlang::caller_env()) {
  metric_url <- paste0("https://yardstick.tidymodels.org/reference/",
                       "index.html#class-probability-metrics")
  msg <- paste0("{.arg {arg}} must be a probability metric from ",
                "{.pkg yardstick}. ",
                "For all options see the ",
                "{.href [reference list]({metric_url})}.")

  if (!(x %in% getNamespaceExports("yardstick"))) {
    cli::cli_abort(msg, call = call)
  }

  ys_obj <- eval(parse(text = paste0("yardstick::", x)))
  if (!("prob_metric" %in% class(ys_obj))) {
    cli::cli_abort(msg, call = call)
  }

  return(ys_obj)
}
