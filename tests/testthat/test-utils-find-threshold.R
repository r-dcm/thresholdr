test_that("attribute probabilities can be extracted from CDM models", {
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

  # attribute probabilities ----------------------------------------------------
  din_probs <- cdm_attribute_probs(din_model)
  expect_equal(dim(din_probs), c(nrow(CDM::sim.dina), 3L))
  expect_equal(colnames(din_probs), c("add", "sub", "mult"))
  expect_equal(
    unname(din_probs),
    unname(as.matrix(CDM::IRT.factor.scores(din_model, type = "EAP")))
  )

  gdina_probs <- cdm_attribute_probs(gdina_model)
  expect_equal(dim(gdina_probs), c(nrow(CDM::sim.dina), 3L))
  expect_equal(colnames(gdina_probs), c("add", "sub", "mult"))
  expect_equal(
    unname(gdina_probs),
    unname(as.matrix(CDM::IRT.factor.scores(gdina_model, type = "EAP")))
  )

  mcdina_probs <- cdm_attribute_probs(mcdina_model)
  expect_equal(dim(mcdina_probs), c(nrow(CDM::data.cdm01$data), 3L))
  expect_equal(colnames(mcdina_probs), c("A1", "A2", "A3"))
  expect_equal(
    unname(mcdina_probs),
    unname(mcdina_model$posterior %*% mcdina_model$attribute.patt.splitted)
  )
})

test_that("attribute thresholds are combined", {
  est <- list(
    att1 = dcm_probs$att1$estimate,
    att2 = dcm_probs$att2$estimate
  )

  # resampling has no diagnostics ----------------------------------------------
  rs <- lapply(est, optimal_resample, optimal_method = "youden", samples = 20)
  combined <- combine_attribute_thresholds(rs)
  expect_s3_class(combined, "tbl_df")
  expect_equal(combined$attribute, c("att1", "att2"))
  expect_equal(combined$threshold, c(rs$att1$threshold, rs$att2$threshold))
  expect_null(attr(combined, "diagnostics"))
  expect_null(attr(combined, "converged"))

  # iteration diagnostics are combined -----------------------------------------
  it <- suppressWarnings(lapply(
    est,
    optimal_iterate,
    optimal_method = "youden",
    additional_criterion = "roc_auc",
    chains = 2,
    iter_burnin = 5,
    iter_retain = 10
  ))
  combined <- combine_attribute_thresholds(it)
  diagnostics <- attr(combined, "diagnostics")
  expect_s3_class(diagnostics, "tbl_df")
  expect_equal(
    colnames(diagnostics),
    c("attribute", "parameter", "rhat", "ess_bulk", "ess_tail", "converged")
  )
  expect_equal(diagnostics$attribute, rep(c("att1", "att2"), each = 2))
  expect_equal(diagnostics$parameter, rep(c("threshold", "roc_auc"), 2))
  expect_equal(
    diagnostics$rhat,
    c(attr(it$att1, "diagnostics")$rhat, attr(it$att2, "diagnostics")$rhat)
  )

  # converged only if every attribute converged --------------------------------
  attr(it$att1, "converged") <- TRUE
  attr(it$att2, "converged") <- TRUE
  expect_true(attr(combine_attribute_thresholds(it), "converged"))

  attr(it$att2, "converged") <- FALSE
  expect_false(attr(combine_attribute_thresholds(it), "converged"))
})

test_that("convergence warnings name the attributes", {
  diagnostics <- tibble::tibble(
    attribute = rep(c("add", "sub", "mult"), each = 2),
    parameter = rep(c("threshold", "roc_auc"), 3),
    rhat = c(1.05, 1.02, 1.00, 1.001, 1.20, 1.00),
    ess_bulk = 100,
    ess_tail = 100,
    converged = rhat <= 1.01
  )
  expect_snapshot(warn_attribute_convergence(diagnostics))
  expect_snapshot(warn_attribute_convergence(diagnostics[5:6, ]))
  expect_warning(
    warn_attribute_convergence(diagnostics),
    class = "thresholdr_warning_convergence"
  )
})

test_that("convergence warnings are consolidated across attributes", {
  # each attribute warns and fails to converge
  local_mocked_bindings(
    optimal_iterate = function(estimates, ...) {
      cli::cli_warn("Not converged.", class = "thresholdr_warning_convergence")
      structure(
        tibble::tibble(threshold = 0.5),
        diagnostics = tibble::tibble(
          parameter = "threshold",
          rhat = 1.1,
          ess_bulk = 100,
          ess_tail = 100,
          converged = FALSE
        ),
        converged = FALSE
      )
    }
  )
  probs <- cbind(
    att1 = dcm_probs$att1$estimate,
    att2 = dcm_probs$att2$estimate
  )

  warnings <- list()
  result <- withCallingHandlers(
    find_attribute_thresholds(
      probs,
      approx_method = "iterate",
      optimal_method = "youden"
    ),
    warning = function(cnd) {
      warnings[[length(warnings) + 1]] <<- cnd
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warnings, 1)
  expect_s3_class(warnings[[1]], "thresholdr_warning_convergence")
  expect_match(conditionMessage(warnings[[1]]), "2 attributes")
  expect_match(conditionMessage(warnings[[1]]), "att1")
  expect_match(conditionMessage(warnings[[1]]), "att2")
  expect_false(attr(result, "converged"))

  # other warnings are not muffled
  local_mocked_bindings(
    optimal_iterate = function(estimates, ...) {
      cli::cli_warn("Something else.")
      structure(
        tibble::tibble(threshold = 0.5),
        diagnostics = tibble::tibble(
          parameter = "threshold",
          rhat = 1.0,
          ess_bulk = 100,
          ess_tail = 100,
          converged = TRUE
        ),
        converged = TRUE
      )
    }
  )
  expect_warning(
    find_attribute_thresholds(
      probs[, "att1", drop = FALSE],
      approx_method = "iterate",
      optimal_method = "youden"
    ),
    "Something else."
  )
})
