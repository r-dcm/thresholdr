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
      must = glue::glue(
        "be of length ",
        "{knitr::combine_words(exp_length, and = ' or ')}"
      ),
      not = length(x),
      call = call
    )
  }

  invisible(x)
}
