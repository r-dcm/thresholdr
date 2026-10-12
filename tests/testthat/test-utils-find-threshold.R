test_that("attribute probabilities can be extracted from CDM models", {
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
