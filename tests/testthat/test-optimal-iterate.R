test_that("iteration works", {
  it_youden <- optimal_iterate(estimates = dcm_probs$att1$estimate,
                               weight_method = "beta",
                               optimal_method = "youden")

  expect_equal(colnames(it_youden),
               c(".threshold", "sensitivity", "specificity", "j_index"))
  expect_equal(typeof(it_youden$.threshold), "double")
  expect_s3_class(it_youden$sensitivity, "rvar")
  expect_s3_class(it_youden$specificity, "rvar")
  expect_s3_class(it_youden$j_index, "rvar")

  expect_equal(length(it_youden$.threshold), 1L)
  expect_equal(posterior::ndraws(it_youden$sensitivity), 1000L)
  expect_equal(posterior::ndraws(it_youden$specificity), 1000L)
  expect_equal(posterior::ndraws(it_youden$j_index), 1000L)
})

test_that("additional criteria works", {
  it_topleft <- optimal_iterate(estimates = dcm_probs$att1$estimate,
                                weight_method = "beta",
                                optimal_method = "topleft",
                                iter_retain = 250,
                                additional_criterion = "roc_auc")

  expect_equal(colnames(it_topleft),
               c(".threshold", "roc_auc", "sensitivity", "specificity",
                 "j_index"))
  expect_equal(typeof(it_topleft$.threshold), "double")
  expect_s3_class(it_topleft$roc_auc, "rvar")
  expect_s3_class(it_topleft$sensitivity, "rvar")
  expect_s3_class(it_topleft$specificity, "rvar")
  expect_s3_class(it_topleft$j_index, "rvar")

  expect_equal(length(it_topleft$.threshold), 1L)
  expect_equal(posterior::ndraws(it_topleft$roc_auc), 250L)
  expect_equal(posterior::ndraws(it_topleft$sensitivity), 250L)
  expect_equal(posterior::ndraws(it_topleft$specificity), 250L)
  expect_equal(posterior::ndraws(it_topleft$j_index), 250L)
})

test_that("metrics work", {
  it_topleft <- optimal_iterate(estimates = dcm_probs$att1$estimate,
                                optimal_method = "topleft",
                                weight_method = "distance",
                                iter_retain = 100,
                                metrics = yardstick::metric_set(
                                  yardstick::precision,
                                  yardstick::f_meas,
                                  yardstick::kap
                                ))

  expect_equal(colnames(it_topleft),
               c(".threshold", "precision", "f_meas", "kap"))
  expect_equal(typeof(it_topleft$.threshold), "double")
  expect_s3_class(it_topleft$precision, "rvar")
  expect_s3_class(it_topleft$f_meas, "rvar")
  expect_s3_class(it_topleft$kap, "rvar")

  expect_equal(length(it_topleft$.threshold), 1L)
  expect_equal(posterior::ndraws(it_topleft$precision), 100L)
  expect_equal(posterior::ndraws(it_topleft$f_meas), 100L)
  expect_equal(posterior::ndraws(it_topleft$kap), 100L)


  it_youden <- optimal_iterate(estimates = dcm_probs$att1$estimate,
                               optimal_method = "youden",
                               weight_method = "distance",
                               additional_criterion = "gain_capture",
                               iter_retain = 250,
                               metrics = yardstick::metric_set(
                                 yardstick::recall,
                                 yardstick::ppv
                               ))

  expect_equal(colnames(it_youden),
               c(".threshold", "gain_capture", "recall", "ppv"))
  expect_equal(typeof(it_youden$.threshold), "double")
  expect_s3_class(it_youden$gain_capture, "rvar")
  expect_s3_class(it_youden$recall, "rvar")
  expect_s3_class(it_youden$ppv, "rvar")

  expect_equal(length(it_youden$.threshold), 1L)
  expect_equal(posterior::ndraws(it_youden$gain_capture), 250L)
  expect_equal(posterior::ndraws(it_youden$recall), 250L)
  expect_equal(posterior::ndraws(it_youden$ppv), 250L)
})

test_that("comparison thresholds work", {
  it_youden <- optimal_iterate(estimates = dcm_probs$att1$estimate,
                               optimal_method = "topleft",
                               weight_method = "distance",
                               additional_criterion = "pr_auc",
                               comp_thresholds = c(0.5, 0.8),
                               iter_retain = 200,
                               metrics = yardstick::metric_set(
                                 yardstick::sens,
                                 yardstick::spec,
                                 yardstick::ppv
                               ))

  expect_equal(colnames(it_youden),
               c(".threshold", "pr_auc", "sens", "spec", "ppv", "distance"))
  expect_equal(typeof(it_youden$.threshold), "double")
  expect_s3_class(it_youden$pr_auc, "rvar")
  expect_s3_class(it_youden$sens, "rvar")
  expect_s3_class(it_youden$spec, "rvar")
  expect_s3_class(it_youden$ppv, "rvar")
  expect_s3_class(it_youden$distance, "rvar")

  expect_equal(length(it_youden$.threshold), 3L)
  expect_equal(vapply(it_youden$pr_auc, posterior::ndraws, integer(1)),
               c(200L, 200L, 200L))
  expect_equal(
    vapply(it_youden$pr_auc,
           \(x) posterior::E(posterior::rvar_is_na(x)),
           double(1)),
    c(0, 1, 1)
  )
  expect_equal(vapply(it_youden$sens, posterior::ndraws, integer(1)),
               c(200L, 200L, 200L))
  expect_equal(
    vapply(it_youden$sens,
           \(x) posterior::E(posterior::rvar_is_na(x)),
           double(1)),
    c(0, 0, 0)
  )
  expect_equal(vapply(it_youden$spec, posterior::ndraws, integer(1)),
               c(200L, 200L, 200L))
  expect_equal(
    vapply(it_youden$spec,
           \(x) posterior::E(posterior::rvar_is_na(x)),
           double(1)),
    c(0, 0, 0)
  )
  expect_equal(vapply(it_youden$ppv, posterior::ndraws, integer(1)),
               c(200L, 200L, 200L))
  expect_equal(
    vapply(it_youden$ppv,
           \(x) posterior::E(posterior::rvar_is_na(x)),
           double(1)),
    c(0, 0, 0)
  )
  expect_equal(vapply(it_youden$distance, posterior::ndraws, integer(1)),
               c(200L, 200L, 200L))
  expect_equal(
    vapply(it_youden$distance,
           \(x) posterior::E(posterior::rvar_is_na(x)),
           double(1)),
    c(0, 0, 0)
  )
})
