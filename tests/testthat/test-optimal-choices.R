test_that("optimal choices are consistent", {
  thresholdr_exp <- getNamespaceExports("thresholdr")
  opts <- thresholdr_exp[which(grepl("calc_", thresholdr_exp))]
  opts <- gsub("calc_", "", opts)

  expect_equal(sort(optimal_method_choices()), sort(opts))

  for (i in seq_along(opts)) {
    func <- get_optimal_function(method = opts[i])

    expect_equal(class(func), "function")
    expect_equal(typeof(func), "closure")
  }

  err <- rlang::catch_cnd(get_optimal_function(method = "larry"))
  expect_s3_class(err, "rlang_error")
  expect_match(
    err$message,
    glue::glue(
      "must be one of ",
      "{knitr::combine_words(paste0('\"', optimal_method_choices(), '\"'),
                                    and = ' or ')}, ",
      "not \"larry\""
    )
  )
})

test_that("weighting choices are consistent", {
  thresholdr_exp <- getNamespaceExports("thresholdr")
  opts <- thresholdr_exp[which(grepl("wt_truth", thresholdr_exp))]
  opts <- gsub("generate_(.*)_wt_truth", "\\1", opts)

  expect_equal(sort(weighting_method_choices()), sort(opts))

  for (i in seq_along(opts)) {
    func <- get_weighting_function(method = opts[i])

    expect_equal(class(func), "function")
    expect_equal(typeof(func), "closure")
  }

  err <- rlang::catch_cnd(get_weighting_function(method = "larry"))
  expect_s3_class(err, "rlang_error")
  expect_match(
    err$message,
    glue::glue(
      "must be one of ",
      "{knitr::combine_words(paste0('\"', weighting_method_choices(), '\"'),
                                    and = ' or ')}, ",
      "not \"larry\""
    )
  )
})

test_that("approximation choices are consistent", {
  thresholdr_exp <- getNamespaceExports("thresholdr")
  opts <- thresholdr_exp[which(grepl("^optimal_", thresholdr_exp))]
  opts <- gsub("optimal_", "", opts)

  expect_equal(sort(approx_method_choices()), sort(opts))
  expect_equal(
    eval(formals(find_threshold)$approx_method),
    approx_method_choices()
  )

  for (i in seq_along(opts)) {
    func <- get_approx_function(method = opts[i])

    expect_equal(class(func), "function")
    expect_equal(typeof(func), "closure")
  }
  expect_identical(get_approx_function("resample"), optimal_resample)
  expect_identical(get_approx_function("iterate"), optimal_iterate)

  err <- rlang::catch_cnd(get_approx_function(method = "larry"))
  expect_s3_class(err, "rlang_error")
  expect_match(
    err$message,
    glue::glue(
      "must be one of ",
      "{knitr::combine_words(paste0('\"', approx_method_choices(), '\"'),
                                    and = ' or ')}, ",
      "not \"larry\""
    )
  )
})
