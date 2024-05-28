test_that("youden works", {
  att1 <- calc_youden(estimates = dcm_probs$att1$estimate,
                      truth = dcm_probs$att1$truth)
  expect_equal(typeof(att1), "double")
  expect_true(0 <= att1 && att1 <= 1)

  att2 <- calc_youden(estimates = dcm_probs$att2$estimate,
                      truth = dcm_probs$att2$truth)
  expect_equal(typeof(att2), "double")
  expect_true(0 <= att2 && att2 <= 1)

  att3 <- calc_youden(estimates = dcm_probs$att3$estimate,
                      truth = dcm_probs$att3$truth)
  expect_equal(typeof(att3), "double")
  expect_true(0 <= att3 && att3 <= 1)
})

test_that("topleft works", {
  att1 <- calc_topleft(estimates = dcm_probs$att1$estimate,
                       truth = dcm_probs$att1$truth)
  expect_equal(typeof(att1), "double")
  expect_true(0 <= att1 && att1 <= 1)

  att2 <- calc_topleft(estimates = dcm_probs$att2$estimate,
                       truth = dcm_probs$att2$truth)
  expect_equal(typeof(att2), "double")
  expect_true(0 <= att2 && att2 <= 1)

  att3 <- calc_topleft(estimates = dcm_probs$att3$estimate,
                       truth = dcm_probs$att3$truth)
  expect_equal(typeof(att3), "double")
  expect_true(0 <= att3 && att3 <= 1)
})

test_that("concordance probability works", {
  att1 <- calc_cz(estimates = dcm_probs$att1$estimate,
                  truth = dcm_probs$att1$truth)
  expect_equal(typeof(att1), "double")
  expect_true(0 <= att1 && att1 <= 1)

  att2 <- calc_cz(estimates = dcm_probs$att2$estimate,
                  truth = dcm_probs$att2$truth)
  expect_equal(typeof(att2), "double")
  expect_true(0 <= att2 && att2 <= 1)

  att3 <- calc_cz(estimates = dcm_probs$att3$estimate,
                  truth = dcm_probs$att3$truth)
  expect_equal(typeof(att3), "double")
  expect_true(0 <= att3 && att3 <= 1)
})

test_that("g-mean works", {
  att1 <- calc_gmean(estimates = dcm_probs$att1$estimate,
                     truth = dcm_probs$att1$truth)
  expect_equal(typeof(att1), "double")
  expect_true(0 <= att1 && att1 <= 1)

  att2 <- calc_gmean(estimates = dcm_probs$att2$estimate,
                     truth = dcm_probs$att2$truth)
  expect_equal(typeof(att2), "double")
  expect_true(0 <= att2 && att2 <= 1)

  att3 <- calc_gmean(estimates = dcm_probs$att3$estimate,
                     truth = dcm_probs$att3$truth)
  expect_equal(typeof(att3), "double")
  expect_true(0 <= att3 && att3 <= 1)
})
