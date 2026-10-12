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
  thld <- find_threshold(measr_model, samples = 100)
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
  expect_s3_class(thld, "tbl_df")
  expect_equal(nrow(thld), (ncol(dcmdata::pie_ft_qmatrix) - 1) * 3)
  expect_equal(
    colnames(thld),
    c(
      "attribute",
      "threshold",
      "ppv",
      "recall"
    )
  )
  expect_equal(
    thld$attribute,
    rep(colnames(dcmdata::pie_ft_qmatrix)[-1], each = 3)
  )
  expect_type(thld$threshold, "double")
  expect_s3_class(thld$ppv, "rvar")
  expect_s3_class(thld$recall, "rvar")

  # for each attribute: estimated threshold first, then comparison thresholds
  for (att_thresholds in split(thld$threshold, thld$attribute)) {
    expect_false(att_thresholds[1] %in% c(.5, .8))
    expect_equal(att_thresholds[-1], c(.5, .8))
  }
})

test_that("GDINA method works", {
  gdina_model <- GDINA::GDINA(
    dat = GDINA::sim10GDINA$simdat,
    Q = GDINA::sim10GDINA$simQ,
    model = "GDINA",
    verbose = 0
  )
  att_names <- c("A1", "A2", "A3")

  # defaults -------------------------------------------------------------------
  thld <- suppressWarnings(find_threshold(
    gdina_model,
    approx_method = "iterate",
    iter_burin = 20,
    iter_retain = 25
  ))
  expect_s3_class(thld, "tbl_df")
  expect_equal(nrow(thld), 3L)
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
  expect_equal(thld$attribute, att_names)
  expect_type(thld$threshold, "double")
  expect_true(all(thld$threshold >= 0 & thld$threshold <= 1))
  expect_s3_class(thld$sensitivity, "rvar")
  expect_s3_class(thld$specificity, "rvar")
  expect_s3_class(thld$j_index, "rvar")
  expect_s3_class(thld$distance, "rvar")
  expect_equal(posterior::ndraws(thld$sensitivity), 100L)

  # add comparison thresholds and new metrics ----------------------------------
  gdina_model <- GDINA::GDINA(
    dat = GDINA::realdata_ECPE$dat,
    Q = rlang::set_names(GDINA::realdata_ECPE$Q, c("morph", "coehs", "lexic")),
    model = "GDINA",
    verbose = 0
  )
  att_names <- c("morph", "coehs", "lexic")
  thld <- suppressWarnings(find_threshold(
    gdina_model,
    approx_method = "resample",
    optimal_method = "minp",
    samples = 100,
    comp_thresholds = c(.4, .7),
    metrics = metric_set(
      yardstick::kap,
      yardstick::f_meas,
      yardstick::precision
    )
  ))
  expect_s3_class(thld, "tbl_df")
  expect_equal(nrow(thld), 9L)
  expect_equal(
    colnames(thld),
    c("attribute", "threshold", "kap", "f_meas", "precision")
  )
  expect_equal(thld$attribute, rep(att_names, each = 3))
  expect_type(thld$threshold, "double")
  expect_s3_class(thld$kap, "rvar")
  expect_s3_class(thld$f_meas, "rvar")
  expect_s3_class(thld$precision, "rvar")
  expect_equal(posterior::ndraws(thld$f_meas), 100L)

  # for each attribute: estimated threshold first, then comparison thresholds
  for (att_thresholds in split(thld$threshold, thld$attribute)) {
    expect_false(att_thresholds[1] %in% c(.4, .7))
    expect_equal(att_thresholds[-1], c(.4, .7))
  }
})
