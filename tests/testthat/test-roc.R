test_that("roc can be calculated", {
  roc_1 <- create_roc(estimates = dcm_probs$att1$estimate,
                      truth = dcm_probs$att1$truth)
  expect_true("roc_df" %in% class(roc_1))
  expect_equal(colnames(roc_1), c(".threshold", "specificity", "sensitivity"))

  roc_2 <- create_roc(estimates = dcm_probs$att2$estimate,
                      truth = dcm_probs$att2$truth)
  expect_true("roc_df" %in% class(roc_2))
  expect_equal(colnames(roc_2), c(".threshold", "specificity", "sensitivity"))

  roc_3 <- create_roc(estimates = dcm_probs$att3$estimate,
                      truth = dcm_probs$att3$truth)
  expect_true("roc_df" %in% class(roc_3))
  expect_equal(colnames(roc_3), c(".threshold", "specificity", "sensitivity"))

  roc_r <- create_roc(estimates = runif(100),
                      truth = sample(c(0L, 1L), size = 100, replace = TRUE))
  expect_true("roc_df" %in% class(roc_r))
  expect_equal(colnames(roc_r), c(".threshold", "specificity", "sensitivity"))
})
