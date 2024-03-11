test_that("abort_bad_argument", {
  err <- rlang::catch_cnd(abort_bad_argument("size", must = "be an integer",
                                             call = rlang::caller_env()))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "be an integer")

  err <- rlang::catch_cnd(abort_bad_argument("size", must = "be an integer",
                                             not = "character",
                                             call = rlang::caller_env()))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "be an integer; not character")

  err <- rlang::catch_cnd(abort_bad_argument("size", must = "be an integer",
                                             extra = "please",
                                             call = rlang::caller_env()))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "be an integer")
  expect_match(err$body, "please")

  err <- rlang::catch_cnd(abort_bad_argument("size", must = "be an integer",
                                             not = "character",
                                             extra = "required",
                                             call = rlang::caller_env()))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "be an integer; not character")
  expect_match(err$body, "required")

  err <- rlang::catch_cnd(abort_bad_argument("size", must = "be an integer",
                                             not = "character",
                                             extra = "required",
                                             custom = "A new error",
                                             call = rlang::caller_env()))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "A new error")
})

test_that("check_double", {
  num <- "1989"
  err <- rlang::catch_cnd(check_double(num))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "`num` must be of type numeric; not character")

  taylor <- as.POSIXlt("1989-12-13")
  err <- rlang::catch_cnd(check_double(taylor))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "`taylor` must be of type numeric; not list")

  err <- rlang::catch_cnd(check_double(c(c(1, 2), 3), exp_length = 1))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "length 1")

  err <- rlang::catch_cnd(check_double(c(c(1, 2), 3), exp_length = c(1, 5)))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "length 1 or 5")

  err <- rlang::catch_cnd(check_double(NA_real_))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "non-missing")

  err <- rlang::catch_cnd(check_double(-1, lb = 0L))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "greater than 0")

  err <- rlang::catch_cnd(check_double(1, ub = 0L))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "less than 0")

  err <- rlang::catch_cnd(check_double(4L, lb = 0L, ub = 3L))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "between 0 and 3")

  err <- rlang::catch_cnd(check_double(0, lb = 0, inclusive = FALSE))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "greater than 0")

  expect_equal(check_double(0.98), 0.98)
  expect_equal(check_double(0.1), 0.1)
  expect_equal(check_double(c(0.5, 9.2)), c(0.5, 9.2))
  expect_equal(check_double(0, lb = 0, inclusive = TRUE), 0L)
  expect_equal(check_double(0.975, lb = 0, ub = 1), 0.975)
  expect_equal(check_double(NULL, allow_null = TRUE), NULL)
})

test_that("check_integer", {
  num <- "a"
  err <- rlang::catch_cnd(check_integer(num))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "`num` must be of type numeric; not character")

  taylor <- "swift"
  err <- rlang::catch_cnd(check_integer(taylor))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "`taylor` must be of type numeric; not character")

  err <- rlang::catch_cnd(check_integer(1:2, exp_length = 1))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "length 1")

  blue <- 1:2
  err <- rlang::catch_cnd(check_integer(blue, exp_length = c(1, 5)))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "`blue` must be of length 1 or 5")

  err <- rlang::catch_cnd(check_integer(NA_integer_))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "non-missing")

  err <- rlang::catch_cnd(check_integer(NULL))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "numeric; not NULL")

  err <- rlang::catch_cnd(check_integer(c(1, 2, 3.2)))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "be integer values")

  err <- rlang::catch_cnd(check_integer(19.89))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "be an integer value")

  err <- rlang::catch_cnd(check_integer(-1, lb = 0L))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "greater than 0")

  err <- rlang::catch_cnd(check_integer(1, ub = 0L))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "less than 0")

  err <- rlang::catch_cnd(check_integer(4L, lb = 0L, ub = 3L))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "between 0 and 3")

  err <- rlang::catch_cnd(check_integer(0, lb = 0, inclusive = FALSE))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "greater than 0")

  expect_equal(check_integer(5), 5L)
  expect_equal(check_integer(5L), 5L)
  expect_equal(check_integer(0, lb = 0, inclusive = TRUE), 0L)
  expect_equal(check_integer(6, lb = 0), 6L)
  expect_equal(check_integer(2:5), 2:5)
  expect_equal(check_integer(NULL, allow_null = TRUE), NULL)
})

test_that("check_prob_metric", {
  err <- rlang::catch_cnd(check_prob_metric("taylor"))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "must be a probability metric")

  err <- rlang::catch_cnd(check_prob_metric("precision"))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "must be a probability metric")

  expect_s3_class(check_prob_metric("roc_auc"),
                  c("prob_metric", "function"))
  expect_s3_class(check_prob_metric("brier_class"),
                  c("prob_metric", "function"))
})
