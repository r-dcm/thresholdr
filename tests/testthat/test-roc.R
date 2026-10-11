test_that("roc can be calculated", {
  roc_1 <- create_roc(
    estimates = dcm_probs$att1$estimate,
    truth = dcm_probs$att1$truth
  )
  expect_true("roc_df" %in% class(roc_1))
  expect_equal(colnames(roc_1), c(".threshold", "specificity", "sensitivity"))

  roc_2 <- create_roc(
    estimates = dcm_probs$att2$estimate,
    truth = dcm_probs$att2$truth
  )
  expect_true("roc_df" %in% class(roc_2))
  expect_equal(colnames(roc_2), c(".threshold", "specificity", "sensitivity"))

  roc_3 <- create_roc(
    estimates = dcm_probs$att3$estimate,
    truth = dcm_probs$att3$truth
  )
  expect_true("roc_df" %in% class(roc_3))
  expect_equal(colnames(roc_3), c(".threshold", "specificity", "sensitivity"))

  roc_r <- create_roc(
    estimates = runif(100),
    truth = sample(c(0L, 1L), size = 100, replace = TRUE)
  )
  expect_true("roc_df" %in% class(roc_r))
  expect_equal(colnames(roc_r), c(".threshold", "specificity", "sensitivity"))
})

test_that("internal roc table matches yardstick", {
  for (i in 1:50) {
    n <- sample(c(10, 100, 500), size = 1)
    estimates <- if (i %% 2) runif(n) else round(runif(n), 1)
    truth <- generate_truth(estimates)
    if (length(unique(truth)) < 2) {
      next
    }

    roc <- roc_table(estimates = estimates, truth = truth)
    expect_identical(
      roc[c(".threshold", "specificity", "sensitivity")],
      as.list(create_roc(estimates = estimates, truth = truth))
    )
    expect_identical(
      roc_table_auc(roc),
      yardstick::roc_auc_vec(
        truth = factor(truth, levels = c(0, 1)),
        estimate = estimates,
        event_level = "second"
      )
    )

    # counts for strictly greater than the threshold
    expect_identical(
      roc$tp_gt,
      vapply(roc$.threshold, \(t) sum(truth[estimates > t]), double(1))
    )
    expect_identical(
      roc$fp_gt,
      vapply(roc$.threshold, \(t) sum(1 - truth[estimates > t]), double(1))
    )
  }

  expect_error(
    calc_youden(estimates = c(0.2, 0.8), truth = c(0L, 0L)),
    "must contain both"
  )
})
