% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optimal-methods.R
\name{calc_topleft}
\alias{calc_topleft}
\title{Top-left statistic}
\usage{
calc_topleft(estimates, truth)
}
\arguments{
\item{estimates}{A vector of classification probabilities. Values should
represent the probability of \code{1} in the \code{truth} argument.}

\item{truth}{An integer vector of \code{0} and \code{1} representing the true
classifications.}
}
\value{
A numeric scalar representing the optimal probability threshold.
}
\description{
Calculate the optimal probability classification threshold using the point
closest to the top-left corner of the ROC plot.
}
\details{
The top-left corner of an ROC plot represents perfect sensitivity and
specificity. The top-left statistic is determined by calculating the distance
from each point on the ROC curve (e.g., \code{\link[=create_roc]{create_roc()}}) to the top-left
corner of the plot. The point on the plot that is the closest to the top-left
corner is the optimal threshold. The optimality criterion is then defined as:

\deqn{\text{min}((1 - sensitivity)^2 + (1 - specificity)^2)}
}
\examples{
calc_topleft(estimates = dcm_probs$att1$estimate,
             truth = dcm_probs$att1$truth)

calc_topleft(estimates = dcm_probs$att2$estimate,
             truth = dcm_probs$att2$truth)

calc_topleft(estimates = dcm_probs$att3$estimate,
             truth = dcm_probs$att3$truth)
}
\seealso{
Other optimal threshold methods: 
\code{\link{calc_cz}()},
\code{\link{calc_gmean}()},
\code{\link{calc_youden}()}
}
\concept{optimal threshold methods}
