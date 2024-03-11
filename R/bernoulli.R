#' Random Bernoulli draws
#'
#' Random generation for the Bernoulli distribution with parameter `prob`.
#'
#' @inheritParams stats::Binomial
#'
#' @return An integer vector of length `n`, consisting of `0` or `1`.
#' @source Based on the [rbinom][stats::Binomial] function, with `size = 1`.
#' @seealso [Distributions][stats::Distributions] for other standard
#'   distributions, including [dbinom][stats::dbinom()] for the binomial.
#' @export
#'
#' @examples
#' # Generate random Bernoulli variables
#' rbern(n = 10, prob = 0.5)
rbern <- function(n, prob) {
  stats::rbinom(n, size = 1, prob)
}
