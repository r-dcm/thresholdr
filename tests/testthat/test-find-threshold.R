test_that("numeric method works", {
  est <- dcm_probs$att1$estimate

  # defaults -------------------------------------------------------------------
  thld <- find_threshold(est, samples = 100)
  expect_s3_class(thld, "tbl_df")
  expect_equal(nrow(thld), 1L)
  expect_equal(
    colnames(thld),
    c("threshold", "sensitivity", "specificity", "j_index", "distance")
  )
  expect_type(thld$threshold, "double")
  expect_true(thld$threshold >= 0 && thld$threshold <= 1)
  expect_s3_class(thld$sensitivity, "rvar")
  expect_s3_class(thld$specificity, "rvar")
  expect_s3_class(thld$j_index, "rvar")
  expect_s3_class(thld$distance, "rvar")
  expect_equal(posterior::ndraws(thld$sensitivity), 100L)

  # add comparison thresholds and new metrics ----------------------------------
  thld <- suppressWarnings(find_threshold(
    est,
    approx_method = "iterate",
    optimal_method = "cz",
    chains = 2,
    iter_burnin = 10,
    iter_retain = 15,
    comp_thresholds = c(.5, .8),
    metrics = metric_set(yardstick::ppv, yardstick::recall)
  ))
  expect_s3_class(thld, "tbl_df")
  expect_equal(nrow(thld), 3L)
  expect_equal(colnames(thld), c("threshold", "ppv", "recall"))
  expect_false(thld$threshold[1] %in% c(.5, .8))
  expect_equal(thld$threshold[-1], c(.5, .8))
  expect_s3_class(thld$ppv, "rvar")
  expect_s3_class(thld$recall, "rvar")
  expect_equal(posterior::ndraws(thld$ppv), 30L)

  # iteration diagnostics are retained
  expect_type(attr(thld, "converged"), "logical")
  expect_s3_class(attr(thld, "diagnostics"), "tbl_df")
})

test_that("numeric method matches the approximation functions", {
  est <- dcm_probs$att2$estimate

  withr::local_seed(1213)
  generic <- find_threshold(
    est,
    optimal_method = "gmean",
    samples = 50,
    comp_thresholds = .5
  )
  withr::local_seed(1213)
  direct <- optimal_resample(
    est,
    optimal_method = "gmean",
    samples = 50,
    comp_thresholds = .5
  )
  expect_identical(generic, direct)

  withr::local_seed(1213)
  generic <- suppressWarnings(find_threshold(
    est,
    approx_method = "iterate",
    optimal_method = "topleft",
    weighting_method = "logodds",
    chains = 2,
    iter_burnin = 10,
    iter_retain = 15
  ))
  withr::local_seed(1213)
  direct <- suppressWarnings(optimal_iterate(
    est,
    optimal_method = "topleft",
    weighting_method = "logodds",
    chains = 2,
    iter_burnin = 10,
    iter_retain = 15
  ))
  expect_identical(generic, direct)
})

test_that("numeric method checks inputs", {
  expect_snapshot(error = TRUE, {
    find_threshold(c(0.2, 1.4, 0.7))
    find_threshold(c(0.2, NA, 0.7))
    find_threshold(c(0.2, 0.7), approx_method = "bootstrap")
    find_threshold(c(0.2, 0.7), optimal_method = "accuracy")
  })
})

test_that("measrdcm method works", {
  skip_if_not_installed("measr")
  skip_if_not_installed("dcmdata")

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
  expect_null(attr(thld, "diagnostics"))
  expect_null(attr(thld, "converged"))
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

  # iteration diagnostics are combined across attributes
  diagnostics <- attr(thld, "diagnostics")
  expect_s3_class(diagnostics, "tbl_df")
  expect_equal(
    colnames(diagnostics),
    c("attribute", "parameter", "rhat", "ess_bulk", "ess_tail", "converged")
  )
  expect_equal(diagnostics$attribute, colnames(dcmdata::pie_ft_qmatrix)[-1])
  expect_equal(attr(thld, "converged"), all(diagnostics$converged))
})

test_that("GDINA method works", {
  skip_if_not_installed("GDINA")

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
    iter_burnin = 20,
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
  diagnostics <- attr(thld, "diagnostics")
  expect_s3_class(diagnostics, "tbl_df")
  expect_equal(diagnostics$attribute, att_names)
  expect_equal(diagnostics$parameter, rep("threshold", 3))
  expect_equal(attr(thld, "converged"), all(diagnostics$converged))

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

test_that("CDM methods work", {
  skip_if_not_installed("CDM")

  q_matrix <- CDM::sim.qmatrix
  colnames(q_matrix) <- c("add", "sub", "mult")

  din_model <- CDM::din(CDM::sim.dina, q.matrix = q_matrix, progress = FALSE)
  gdina_model <- suppressWarnings(
    CDM::gdina(CDM::sim.dina, q.matrix = q_matrix, progress = FALSE)
  )
  # mcdina() prints timing information even when `progress = FALSE`
  utils::capture.output(
    mcdina_model <- CDM::mcdina(
      CDM::data.cdm01$data,
      q.matrix = CDM::data.cdm01$q.matrix,
      progress = FALSE
    )
  )

  # defaults -------------------------------------------------------------------
  for (model in list(din_model, gdina_model)) {
    thld <- find_threshold(model, samples = 50, optimal_method = "iu")
    expect_s3_class(thld, "tbl_df")
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
    expect_equal(thld$attribute, c("add", "sub", "mult"))
    expect_type(thld$threshold, "double")
    expect_true(all(thld$threshold >= 0 & thld$threshold <= 1))
    expect_s3_class(thld$sensitivity, "rvar")
    expect_s3_class(thld$specificity, "rvar")
    expect_s3_class(thld$j_index, "rvar")
    expect_s3_class(thld$distance, "rvar")
    expect_equal(posterior::ndraws(thld$sensitivity), 50L)
  }

  # add comparison thresholds and new metrics ----------------------------------
  thld <- suppressWarnings(
    find_threshold(
      mcdina_model,
      approx_method = "iterate",
      optimal_method = "gmean",
      weighting_method = "distance",
      chains = 2,
      iter_burnin = 10,
      iter_retain = 15,
      comp_thresholds = c(.5, .8),
      metrics = metric_set(yardstick::ppv, yardstick::recall)
    )
  )
  expect_s3_class(thld, "tbl_df")
  expect_equal(nrow(thld), 9L)
  expect_equal(colnames(thld), c("attribute", "threshold", "ppv", "recall"))
  expect_equal(thld$attribute, rep(c("A1", "A2", "A3"), each = 3))
  expect_s3_class(thld$ppv, "rvar")
  expect_equal(posterior::ndraws(thld$ppv), 30L)
  diagnostics <- attr(thld, "diagnostics")
  expect_equal(diagnostics$attribute, c("A1", "A2", "A3"))
  expect_equal(attr(thld, "converged"), all(diagnostics$converged))

  for (att_thresholds in split(thld$threshold, thld$attribute)) {
    expect_false(att_thresholds[1] %in% c(.5, .8))
    expect_equal(att_thresholds[-1], c(.5, .8))
  }
})

test_that("CDM gdm method works", {
  skip_if_not_installed("CDM")

  # names are taken from the columns of `theta.k`
  theta_k <- as.matrix(expand.grid(add = 0:1, sub = 0:1, mult = 0:1))
  utils::capture.output(
    gdm_model <- CDM::gdm(
      CDM::sim.dina,
      theta.k = theta_k,
      Qmatrix = as.matrix(CDM::sim.qmatrix),
      skillspace = "full",
      progress = FALSE
    )
  )

  gdm_probs <- cdm_attribute_probs(gdm_model)
  expect_equal(dim(gdm_probs), c(nrow(CDM::sim.dina), 3L))
  expect_equal(colnames(gdm_probs), c("add", "sub", "mult"))
  expect_equal(
    unname(gdm_probs),
    unname(as.matrix(gdm_model$person[, paste0("EAP.", colnames(theta_k))]))
  )

  thld <- find_threshold(gdm_model, samples = 50)
  expect_s3_class(thld, "tbl_df")
  expect_equal(thld$attribute, c("add", "sub", "mult"))
  expect_true(all(thld$threshold >= 0 & thld$threshold <= 1))
  expect_equal(posterior::ndraws(thld$sensitivity), 50L)

  # default skill names
  utils::capture.output(
    gdm_model <- CDM::gdm(
      CDM::sim.dina,
      theta.k = c(0, 1),
      Qmatrix = as.matrix(CDM::sim.qmatrix),
      progress = FALSE
    )
  )
  expect_equal(colnames(cdm_attribute_probs(gdm_model)), c("F1", "F2", "F3"))

  # only binary skills are supported
  utils::capture.output(
    gdm_poly <- CDM::gdm(
      CDM::sim.dina,
      theta.k = c(0, 1, 2),
      Qmatrix = as.matrix(CDM::sim.qmatrix),
      progress = FALSE
    )
  )
  expect_snapshot(error = TRUE, find_threshold(gdm_poly))
})
