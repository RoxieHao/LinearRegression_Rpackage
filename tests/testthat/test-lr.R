test_that("lr works", {
  X = iris[,c(2,3)]
  Y = iris$Sepal.Length
  model1 = lr(X,Y,TRUE)
  model2 = lm(Sepal.Length ~ Sepal.Width+Petal.Length, data = iris)
  expect_equal(model1$coefficients,summary(model2)$coefficients)
})
