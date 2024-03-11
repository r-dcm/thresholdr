#' Get an error message for expected bounds
#'
#' @param lb The lower bound.
#' @param ub The upper bound.
#'
#' @return A [glue][glue::glue()] object for printing in error messages.
#' @noRd
bounded_error <- function(lb, ub) {
  if (is.infinite(lb)) {
    glue::glue("be less than {ub}")
  } else if (is.infinite(ub)) {
    glue::glue("be greater than {lb}")
  } else {
    glue::glue("be between {lb} and {ub}")
  }
}


#' Determine the bounds of an argument
#'
#' @param type Type of value to be checked. One of `"dbl"` or `"int"`.
#' @param inclusive Logical. Are the lower and upper bounds inclusive?
#' @param lb The lower bound.
#' @param ub The upper bound.
#'
#' @return A list that includes either functions for checking double bounds, or
#'   boundaries for checking integer bounds.
#' @noRd
check_bounds <- function(type, inclusive, lb = -Inf, ub = Inf) {
  type <- rlang::arg_match(type, values = c("dbl", "int"))

  ret_list <- if (type == "dbl") {
    if (inclusive) {
      list(
        check_lb = function(.x, .y) .x < .y,
        check_ub = function(.x, .y) .x > .y
      )
    } else if (!inclusive) {
      list(
        check_lb = function(.x, .y) .x <= .y,
        check_ub = function(.x, .y) .x >= .y
      )
    }
  } else if (type == "int") {
    if (inclusive) {
      list(
        check_lb = lb,
        check_ub = ub
      )
    } else if (!inclusive) {
      list(
        check_lb = lb + 1L,
        check_ub = ub - 1L
      )
    }
  }

  return(ret_list)
}

#' Check the length of an argument
#'
#' @param x The object to test.
#' @param exp_length The expected length of `x`.
#' @param arg The name of the argument, passed to [abort_bad_argument()].
#' @param call The call stack, passed to [abort_bad_argument()].
#'
#' @return Invisibly returns `x`.
#' @noRd
check_length <- function(x, exp_length, arg, call) {
  if (!is.null(exp_length) && !(length(x) %in% exp_length)) {
    abort_bad_argument(
      arg = arg,
      must = glue::glue("be of length ",
                        "{knitr::combine_words(exp_length, and = ' or ')}"),
      not = length(x),
      call = call
    )
  }

  invisible(x)
}
