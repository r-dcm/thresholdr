test_that("Beta conversion works", {
  muphi <- shapes_to_muphi(6, 4)
  expect_equal(muphi$mu, 0.6)
  expect_equal(muphi$phi, 10)

  muphi <- shapes_to_muphi(60, 40)
  expect_equal(muphi$mu, 0.6)
  expect_equal(muphi$phi, 100)

  shapes <- muphi_to_shapes(0.5, 2)
  expect_equal(shapes$shape1, 1)
  expect_equal(shapes$shape2, 1)

  shapes <- muphi_to_shapes(0.2, 10)
  expect_equal(shapes$shape1, 2)
  expect_equal(shapes$shape2, 8)
})

test_that("Beta median conversion works", {
  expect_equal(median_to_mu(0.5, 2), 0.5)

  for (med in c(0.05, 0.3, 0.8, 0.95)) {
    for (phi in c(2, 10, 50)) {
      mu <- median_to_mu(med, phi)
      shapes <- muphi_to_shapes(mu, phi)
      expect_equal(
        stats::qbeta(0.5, shapes$shape1, shapes$shape2),
        med,
        tolerance = 1e-6
      )
    }
  }
})

test_that("Bounding values works", {
  vals <- runif(100, min = -0.5, max = 1.5)
  new_vals <- bound(vals, lb = 0, ub = 1)

  expect_equal(length(vals), length(new_vals))
  expect_gte(min(new_vals), 1e-5)
  expect_lte(max(new_vals), 0.99999)
})
