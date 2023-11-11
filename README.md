
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LinearRegression

<!-- badges: start -->

[![R-CMD-check](https://github.com/RoxieHao/LinearRegression_Rpackage/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RoxieHao/LinearRegression_Rpackage/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of LinearRegression is to fit linear models, including both
simple linear regressions and multiple linear regressions.

## Installation

You can install the development version of LinearRegression from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("RoxieHao/LinearRegression_Rpackage")
```

## Example

This is a basic example which shows you how to fit a linear model:

``` r
library(LinearRegression)
## basic example code
```

To fit the model, first input the dependent variable, covariates and
whether include an intercept in the model, then we can get the fitted
model.

``` r
X = iris[,c(2,3)]
Y = iris$Sepal.Length
model1 = lr(X,Y,TRUE)
```
