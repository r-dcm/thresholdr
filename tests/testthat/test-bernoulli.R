test_that("random generation works", {
  expect_equal(class(rbern(10, prob = 0.5)), "integer")

  expect_true(all(rbern(100, prob = 0.5) %in% c(0L, 1L)))

  iter <- 1L
  repeat {
    n <- sample(seq_len(100), size = 1)
    expect_equal(length(rbern(n = n, prob = runif(1))), n)

    iter <- iter + 1
    if (iter > 4) {
      break
    }
  }

  iter <- 1L
  repeat {
    prob <- runif(n = 1, min = 0.1, max = 0.9)
    expect_equal(mean(rbern(n = 50000, prob = prob)), prob,
                 tolerance = 0.03)

    iter <- iter + 1
    if (iter > 4) {
      break
    }
  }
})
