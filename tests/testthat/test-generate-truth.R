test_that("normal generation works", {
  estimates <- runif(50)
  truth <- generate_truth(estimates)

  expect_equal(length(estimates), length(truth))
  expect_equal(typeof(truth), "integer")
  expect_true(all(truth %in% c(0L, 1L)))

  check_probs <- vapply(1:10000,
                        \(x) {
                          generate_truth(estimates = estimates)
                        },
                        integer(length(estimates))) |>
    rowMeans()

  expect_equal(check_probs, estimates, tolerance = 0.3)
})

test_that("beta-weighted generation works", {
  estimates <- runif(50)
  truth <- generate_beta_wt_truth(estimates)

  expect_equal(length(estimates), length(truth))
  expect_equal(typeof(truth), "integer")
  expect_true(all(truth %in% c(0L, 1L)))

  check_probs <- vapply(1:10000,
                        \(x) {
                          mean(
                            generate_beta_wt_truth(
                              estimates = runif(1000)
                            )
                          )
                        },
                        double(1))
  expect_equal(mean(check_probs), .5, tolerance = 0.1)

  check_probs <- vapply(1:10000,
                        \(x) {
                          mean(
                            generate_beta_wt_truth(
                              estimates = runif(1000),
                              threshold = 0.8, precision = 7
                            )
                          )
                        },
                        double(1))
  expect_equal(mean(check_probs), .2, tolerance = 0.1)
})

test_that("distance-weighted generation works", {
  estimates <- runif(50)
  truth <- generate_distance_wt_truth(estimates)

  expect_equal(length(estimates), length(truth))
  expect_equal(typeof(truth), "integer")
  expect_true(all(truth %in% c(0L, 1L)))

  check_probs <- vapply(1:10000,
                        \(x) {
                          mean(
                            generate_distance_wt_truth(
                              estimates = runif(1000)
                            )
                          )
                        },
                        double(1))
  expect_equal(mean(check_probs), .5, tolerance = 0.1)

  check_probs <- vapply(1:10000,
                        \(x) {
                          mean(
                            generate_distance_wt_truth(
                              estimates = runif(1000),
                              threshold = 0.8
                            )
                          )
                        },
                        double(1))
  expect_equal(mean(check_probs), .25, tolerance = 0.1)
})
