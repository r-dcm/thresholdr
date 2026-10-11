test_that("normal generation works", {
  estimates <- runif(50)
  truth <- generate_truth(estimates)

  expect_equal(length(estimates), length(truth))
  expect_equal(typeof(truth), "integer")
  expect_true(all(truth %in% c(0L, 1L)))

  check_probs <- vapply(
    1:10000,
    \(x) {
      generate_truth(estimates = estimates)
    },
    integer(length(estimates))
  ) |>
    rowMeans()

  expect_equal(check_probs, estimates, tolerance = 0.3)
})

test_that("beta-weighted generation works", {
  estimates <- runif(50)
  truth <- generate_beta_wt_truth(estimates)

  expect_equal(length(estimates), length(truth))
  expect_equal(typeof(truth), "integer")
  expect_true(all(truth %in% c(0L, 1L)))

  check_probs <- vapply(
    1:10000,
    \(x) {
      mean(
        generate_beta_wt_truth(
          estimates = runif(1000)
        )
      )
    },
    double(1)
  )
  expect_equal(mean(check_probs), .5, tolerance = 0.1)

  check_probs <- vapply(
    1:10000,
    \(x) {
      mean(
        generate_beta_wt_truth(
          estimates = runif(1000),
          threshold = 0.8,
          precision = 7
        )
      )
    },
    double(1)
  )

  # with uniform estimates, P(truth = 1) = 1 - mean of the Beta distribution
  expect_equal(
    mean(check_probs),
    1 - median_to_mu(median = 0.8, phi = 7),
    tolerance = 0.1
  )

  # estimates equal to the threshold are classified as `1` half of the time
  check_probs <- vapply(
    c(0.2, 0.7, 0.9),
    \(t) {
      mean(
        generate_beta_wt_truth(
          estimates = rep(t, 100000),
          threshold = t,
          precision = 10
        )
      )
    },
    double(1)
  )
  expect_equal(check_probs, c(.5, .5, .5), tolerance = 0.02)
})

test_that("distance-weighted generation works", {
  estimates <- runif(50)
  truth <- generate_distance_wt_truth(estimates)

  expect_equal(length(estimates), length(truth))
  expect_equal(typeof(truth), "integer")
  expect_true(all(truth %in% c(0L, 1L)))

  check_probs <- vapply(
    1:10000,
    \(x) {
      mean(
        generate_distance_wt_truth(
          estimates = runif(1000)
        )
      )
    },
    double(1)
  )
  expect_equal(mean(check_probs), .5, tolerance = 0.1)

  check_probs <- vapply(
    1:10000,
    \(x) {
      mean(
        generate_distance_wt_truth(
          estimates = runif(1000),
          threshold = 0.8
        )
      )
    },
    double(1)
  )
  expect_equal(mean(check_probs), .25, tolerance = 0.1)
})

test_that("log-odds-weighted generation works", {
  estimates <- runif(50)
  truth <- generate_logodds_wt_truth(estimates)

  expect_equal(length(estimates), length(truth))
  expect_equal(typeof(truth), "integer")
  expect_true(all(truth %in% c(0L, 1L)))

  check_probs <- vapply(
    1:10000,
    \(x) {
      mean(
        generate_logodds_wt_truth(
          estimates = runif(1000)
        )
      )
    },
    double(1)
  )
  expect_equal(mean(check_probs), .5, tolerance = 0.1)

  # estimates equal to the threshold are classified as `1` half of the time
  check_probs <- vapply(
    c(0.2, 0.7, 0.9),
    \(t) {
      mean(generate_logodds_wt_truth(rep(t, 100000), threshold = t))
    },
    double(1)
  )
  expect_equal(check_probs, c(.5, .5, .5), tolerance = 0.02)

  # estimates of 0 and 1 are not changed
  expect_equal(
    generate_logodds_wt_truth(c(0, 1, 0, 1), threshold = 0.8),
    c(0L, 1L, 0L, 1L)
  )
})
