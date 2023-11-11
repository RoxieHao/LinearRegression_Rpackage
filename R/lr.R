#'Fitting Linear Regressions
#'
#'This function is used to fit linear models, including both simple linear
#'regressions and multiple linear regressions. The output includes the estimated
#'coefficient, standard error, t values, and P values for each variable, and
#'R squared value, adjusted R squared value, Residual standard error, F-statistic
#'and p value for the whole model.
#'
#'@param X a matrix or a dataframe contains all covariates of interest as columns
#'and all observations. If you want your model contains an intercept, the first column
#'of X should be fulled with 1.
#'@param Y the dependent variable. It is a matrix with dimension n*1 (n means
#'number of observations).
#'@param I a logical operator for the intercept. If I = TRUE, the model contains
#'the intercept, or the intercept is removed.
#'
#'@return \item{coefficients}{a vector for each covariate including estimated
#'coefficients, standard error, t value, p value and significance}
#'@return \item{Residual standard error}{the standard error of residual}
#'@return \item{R-squared value}{the percentage of varience can be explained by
#'the model. Higher percentage means a better fitting of model}
#'@return \item{F statistic}{the test statistic for the analysis of variance
#'approach to test the significance of the model or the components in the model}
#'
#'@examples
#'X = iris[,c(2,3)]
#'Y = iris$Sepal.Length
#'lr(X,Y,TRUE)
#'@export
#'

lr <- function(X, Y, I) {
  n = as.numeric(length(Y))
  p = as.numeric(ncol(X))

  ## represent X and Y
  if (I == TRUE) {
    X = as.matrix(cbind(1, X))
    Y = as.matrix(Y)
    p = p + 1
  } else{
    X = as.matrix(X)
    Y = as.matrix(Y)
  }

  ## calculate beta_hat
  (beta_hat = solve(t(X) %*% X) %*% t(X) %*% Y)

  ## calculate Y_hat
  H = X %*% solve(t(X) %*% X) %*% t(X)
  (Y_hat = H %*% Y)

  ## calculate residual
  epsilon_hat = Y - Y_hat

  ## calculate t statistics
  # estimated sigma^2
  (sigma_2 = t(epsilon_hat) %*% epsilon_hat / (n - p))
  # variance of betahat
  vb = diag(solve(t(X) %*% X))
  var_beta_hat = vb * c(sigma_2)
  # se of betahat
  (se_beta_hat = sqrt(var_beta_hat))
  t_stat = c(beta_hat / se_beta_hat)

  ## p_value for t_statistics
  p_1 = stats::pt(q = abs(t_stat), df = n - p)
  p_value = c(2 * (1 - p_1))

  ## R_2
  Y_bar = mean(Y)
  if(all(X[,1] == 1)) {
    SSY = sum((Y - Y_bar) ^ 2) ## multiple linear regression
  }else{
    SSY = sum(Y  ^ 2)  ## multiple linear regression without intercept
  }
  (SSE = sum(epsilon_hat ^ 2))
  (SSR = SSY - SSE)
  (R_2 = SSR / SSY)

  ## F statistic
  if(all(X[,1] == 1)) {
    df1 = (p - 1) ## multiple linear regression
  }else{
      df1 = p  ## multiple linear regression without intercept
    }

  df2 = (n - p)
  MSR = SSR / df1
  MSE = SSE / df2
  F_stat = MSR / MSE
  F_p = stats::pf(F_stat, df1, df2, lower.tail = FALSE)


  # create the overall table for coefficients
  coefficient_table = data.frame(
    beta_hat,
    se_beta_hat,
    t_stat,
    p_value
  )
  coefficient_table = as.matrix(coefficient_table)
  colnames(coefficient_table) = c("Estimate","Std. Error", "t value", "Pr(>|t|)")
  if(all(X[,1] == 1)) {
    row.names(coefficient_table)[1] = "(Intercept)" ## multiple linear regression
  }

  note1 <-paste("R_squared value: ", round(R_2, digits = 4))
  note2 <-paste(
    "F-statistic: ",
    round(F_stat, digits = 4),
    " on ",
    df1,
    " and ",
    df2,
    " DF, p-value: ",
    F_p
  )

  result <- list(
    coefficients =coefficient_table,
    R_squared_value = note1,
    F_statistics = note2
  )

  return(result)
}

