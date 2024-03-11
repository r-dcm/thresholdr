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
