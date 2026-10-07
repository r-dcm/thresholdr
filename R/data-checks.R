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
abort_bad_argument <- function(
  arg,
  must,
  not = NULL,
  extra = NULL,
  custom = NULL,
  ...,
  call
) {
  extra_arg <- list(...)

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


#' Check vectors of numeric values
#'
#' @param x The input value to be checked.
#' @param ... Additional arguments passed to [rlang::check_number_decimal()] for
#'   `check_double()` or [rlang::check_number_whole()] for `check_integer()``.
#' @param exp_length The expected value of `length(x)`. If `NULL`, any length is
#'   accepted. If multiple lengths are acceptable, a vector can be specified
#'   (e.g., `exp_length = c(1, 10)`).
#' @inheritParams rlang::check_number_decimal arg call
#'
#' @noRd
check_double <- function(
  x,
  ...,
  exp_length = NULL,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  for (i in seq_along(x)) {
    rlang::check_number_decimal(x[i], ..., arg = arg, call = call)
  }
  if (!length(x)) {
    rlang::check_number_decimal(x, ..., arg = arg, call = call)
  }

  check_length(x = x, exp_length = exp_length, arg = arg, call = call)
}


#' @rdname check_double
#' @noRd
check_integer <- function(
  x,
  ...,
  exp_length = NULL,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  for (i in seq_along(x)) {
    rlang::check_number_whole(x[i], ..., arg = arg, call = call)
  }
  if (!length(x)) {
    rlang::check_number_whole(x, ..., arg = arg, call = call)
  }

  check_length(x = x, exp_length = exp_length, arg = arg, call = call)
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
check_prob_metric <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  metric_url <- paste0(
    "https://yardstick.tidymodels.org/reference/",
    "index.html#class-probability-metrics"
  )
  msg <- paste0(
    "{.arg {arg}} must be a probability metric from ",
    "{.pkg yardstick}. ",
    "For all options see the ",
    "{.href [reference list]({metric_url})}."
  )

  if (!(x %in% getNamespaceExports("yardstick"))) {
    cli::cli_abort(msg, call = call)
  }

  ys_obj <- eval(parse(text = paste0("yardstick::", x)))
  if (!("prob_metric" %in% class(ys_obj))) {
    cli::cli_abort(msg, call = call)
  }

  ys_obj
}
