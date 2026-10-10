test_that("measrdcm method works", {
  model_spec <- measr::dcm_specify(
    qmatrix = dcmdata::pie_ft_qmatrix,
    identifier = "task"
  )
  measr_model <- measr::dcm_estimate(
    dcm_spec = model_spec,
    data = dcmdata::pie_ft_data,
    identifier = "student",
    method = "optim",
    seed = 63277
  )

  # defaults -------------------------------------------------------------------
  thld <- find_threshold(measr_model)
  expect_s3_class(thld, "tbl_df")
  expect_equal(nrow(thld), ncol(dcmdata::pie_ft_qmatrix) - 1)
  expect_equal(
    colnames(thld),
    c(
      "attribute",
      "threshold",
      "sensitivity",
      "specificity",
      "j_index",
      "distance"
    )
  )
  expect_equal(thld$attribute, colnames(dcmdata::pie_ft_qmatrix)[-1])
  expect_type(thld$threshold, "double")
  expect_s3_class(thld$sensitivity, "rvar")
  expect_s3_class(thld$specificity, "rvar")
  expect_s3_class(thld$j_index, "rvar")
  expect_s3_class(thld$distance, "rvar")

  # add comparison thresholds and new metrics ----------------------------------
  thld <- find_threshold(
    measr_model,
    approx_method = "iterate",
    optimal_method = "cz",
    comp_thresholds = c(.5, .8),
    metrics = metric_set(yardstick::ppv, yardstick::recall)
  )
})

test_that("GDINA method works", {
  gdina_model <- GDINA::GDINA(
    dat = GDINA::sim10GDINA$simdat,
    Q = GDINA::sim10GDINA$simQ,
    model = "GDINA",
    verbose = 0
  )
})
