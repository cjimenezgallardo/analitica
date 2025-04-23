test_that("descripYG funciona sin vi", {
  df <- data.frame(x = rnorm(100))
  resultado <- descripYG(df, vd = x)
  expect_true("promedio" %in% names(resultado))
})
