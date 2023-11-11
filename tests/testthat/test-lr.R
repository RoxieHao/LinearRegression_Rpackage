test_that("lr works", {
  X = iris[,c(2,3)]
  Y = iris$Sepal.Length
  model1 = lr(X,Y,TRUE)
  model2 = lm(Sepal.Length ~ Sepal.Width+Petal.Length, data = iris)
  expect_equal(model1$coefficients,summary(model2)$coefficients)

  a = iris[,c(2,3,4)]
  b = iris$Sepal.Length
  model3 = lr(a,b,FALSE)
  model4 = lm(Sepal.Length ~ -1+ Sepal.Width+Petal.Length+Petal.Width, data = iris)
  expect_equal(model3$coefficients,summary(model4)$coefficients)
})
