% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bernoulli.R
\name{rbern}
\alias{rbern}
\title{Random Bernoulli draws}
\source{
Based on the \link[stats:Binomial]{rbinom} function, with \code{size = 1}.
}
\usage{
rbern(n, prob)
}
\arguments{
\item{n}{number of observations. If \code{length(n) > 1}, the length
    is taken to be the number required.}

\item{prob}{probability of success on each trial.}
}
\value{
An integer vector of length \code{n}, consisting of \code{0} or \code{1}.
}
\description{
Random generation for the Bernoulli distribution with parameter \code{prob}.
}
\examples{
# Generate random Bernoulli variables
rbern(n = 10, prob = 0.5)
}
\seealso{
\link[stats:Distributions]{Distributions} for other standard
distributions, including \link[stats:Binomial]{dbinom} for the binomial.
}
