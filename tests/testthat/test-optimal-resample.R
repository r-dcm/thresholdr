test_that("resampling works", {
  rs_youden <- optimal_resample(estimates = dcm_probs$att1$estimate,
                                optimal_method = "youden", samples = 500)
  expect_equal(colnames(rs_youden),
               c(".threshold", "sensitivity", "specificity", "j_index"))
  expect_equal(typeof(rs_youden$.threshold), "double")
  expect_s3_class(rs_youden$sensitivity, "rvar")
  expect_s3_class(rs_youden$specificity, "rvar")
  expect_s3_class(rs_youden$j_index, "rvar")

  expect_equal(length(rs_youden$.threshold), 1L)
  expect_equal(posterior::ndraws(rs_youden$sensitivity), 500L)
  expect_equal(posterior::ndraws(rs_youden$specificity), 500L)
  expect_equal(posterior::ndraws(rs_youden$j_index), 500L)
})

test_that("weighting works", {
  rs_topleft <- optimal_resample(estimates = dcm_probs$att1$estimate,
                                 optimal_method = "topleft", samples = 100,
                                 weight_by = "roc_auc")

  expect_equal(colnames(rs_topleft),
               c(".threshold", "roc_auc", "sensitivity", "specificity",
                 "j_index"))
  expect_equal(typeof(rs_topleft$.threshold), "double")
  expect_s3_class(rs_topleft$roc_auc, "rvar")
  expect_s3_class(rs_topleft$sensitivity, "rvar")
  expect_s3_class(rs_topleft$specificity, "rvar")
  expect_s3_class(rs_topleft$j_index, "rvar")

  expect_equal(length(rs_topleft$.threshold), 1L)
  expect_equal(posterior::ndraws(rs_topleft$roc_auc), 100L)
  expect_equal(posterior::ndraws(rs_topleft$sensitivity), 100L)
  expect_equal(posterior::ndraws(rs_topleft$specificity), 100L)
  expect_equal(posterior::ndraws(rs_topleft$j_index), 100L)
})

test_that("metrics work", {
  rs_topleft <- optimal_resample(estimates = dcm_probs$att1$estimate,
                                 optimal_method = "topleft", samples = 100,
                                 metrics = yardstick::metric_set(
                                   yardstick::precision,
                                   yardstick::f_meas,
                                   yardstick::kap
                                 ))

  expect_equal(colnames(rs_topleft),
               c(".threshold", "precision", "f_meas", "kap"))
  expect_equal(typeof(rs_topleft$.threshold), "double")
  expect_s3_class(rs_topleft$precision, "rvar")
  expect_s3_class(rs_topleft$f_meas, "rvar")
  expect_s3_class(rs_topleft$kap, "rvar")

  expect_equal(length(rs_topleft$.threshold), 1L)
  expect_equal(posterior::ndraws(rs_topleft$precision), 100L)
  expect_equal(posterior::ndraws(rs_topleft$f_meas), 100L)
  expect_equal(posterior::ndraws(rs_topleft$kap), 100L)


  rs_youden <- optimal_resample(estimates = dcm_probs$att1$estimate,
                                optimal_method = "youden", samples = 100,
                                weight_by = "gain_capture",
                                metrics = yardstick::metric_set(
                                  yardstick::recall,
                                  yardstick::ppv
                                ))

  expect_equal(colnames(rs_youden),
               c(".threshold", "gain_capture", "recall", "ppv"))
  expect_equal(typeof(rs_youden$.threshold), "double")
  expect_s3_class(rs_youden$gain_capture, "rvar")
  expect_s3_class(rs_youden$recall, "rvar")
  expect_s3_class(rs_youden$ppv, "rvar")

  expect_equal(length(rs_youden$.threshold), 1L)
  expect_equal(posterior::ndraws(rs_youden$gain_capture), 100L)
  expect_equal(posterior::ndraws(rs_youden$recall), 100L)
  expect_equal(posterior::ndraws(rs_youden$ppv), 100L)
})

test_that("comparison thresholds work", {
  rs_youden <- optimal_resample(estimates = dcm_probs$att1$estimate,
                                optimal_method = "topleft", samples = 100,
                                weight_by = "pr_auc",
                                comp_thresholds = c(0.5, 0.8),
                                metrics = yardstick::metric_set(
                                  yardstick::sens,
                                  yardstick::spec,
                                  yardstick::ppv
                                ))

  expect_equal(colnames(rs_youden),
               c(".threshold", "pr_auc", "sens", "spec", "ppv", "distance"))
  expect_equal(typeof(rs_youden$.threshold), "double")
  expect_s3_class(rs_youden$pr_auc, "rvar")
  expect_s3_class(rs_youden$sens, "rvar")
  expect_s3_class(rs_youden$spec, "rvar")
  expect_s3_class(rs_youden$ppv, "rvar")
  expect_s3_class(rs_youden$distance, "rvar")

  expect_equal(length(rs_youden$.threshold), 3L)
  expect_equal(vapply(rs_youden$pr_auc, posterior::ndraws, integer(1)),
               c(100L, 100L, 100L))
  expect_equal(vapply(rs_youden$pr_auc,
                      \(x) {
                        posterior::E(posterior::rvar_is_na(x))
                      },
                      double(1)),
               c(0, 1, 1))
  expect_equal(vapply(rs_youden$sens, posterior::ndraws, integer(1)),
               c(100L, 100L, 100L))
  expect_equal(vapply(rs_youden$sens,
                      \(x) {
                        posterior::E(posterior::rvar_is_na(x))
                      },
                      double(1)),
               c(0, 0, 0))
  expect_equal(vapply(rs_youden$spec, posterior::ndraws, integer(1)),
               c(100L, 100L, 100L))
  expect_equal(vapply(rs_youden$spec,
                      \(x) {
                        posterior::E(posterior::rvar_is_na(x))
                      },
                      double(1)),
               c(0, 0, 0))
  expect_equal(vapply(rs_youden$ppv, posterior::ndraws, integer(1)),
               c(100L, 100L, 100L))
  expect_equal(vapply(rs_youden$ppv,
                      \(x) {
                        posterior::E(posterior::rvar_is_na(x))
                      },
                      double(1)),
               c(0, 0, 0))
  expect_equal(vapply(rs_youden$distance, posterior::ndraws, integer(1)),
               c(100L, 100L, 100L))
  expect_equal(vapply(rs_youden$distance,
                      \(x) {
                        posterior::E(posterior::rvar_is_na(x))
                      },
                      double(1)),
               c(0, 0, 0))
})
