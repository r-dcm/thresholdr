test_that("multiplication works", {
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
