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

test_that("Bounding values works", {
  vals <- runif(100, min = -0.5, max = 1.5)
  new_vals <- bound(vals, lb = 0, ub = 1)

  expect_equal(length(vals), length(new_vals))
  expect_gte(min(new_vals), 1e-5)
  expect_lte(max(new_vals), 0.99999)
})
