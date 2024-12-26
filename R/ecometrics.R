#' Fitting a simple or multiple linear regression
#'
#' @param data A data frame containing the variables to use
#' @param y The dependent variable
#' @param x Set of independent variables
#'
#' @return A tibble of the coefficients, standard errors, t-statistics and p-value
#' @author Mutua Kilai
#' @export
#'
#' @examples
#' ols_model(data = eduperform, "pi", c("hs", "ps"))
#'
#'

ols_model <- function(data, y, x)
{
  data[x] <- lapply(data[x], function(xx) {
    if (is.character(xx)) as.factor(xx) else xx
  })
  formula_str <- paste(y, "~", paste(x, collapse = " + "))
  formula <- stats::as.formula(formula_str)
  model <- summary(stats::lm(formula, data = data))
  gg <- data.frame(model$coefficients)
  gg <- tibble::as_tibble(gg, rownames = "term")
  colnames(gg) <- c("term","coefficient", "SE", "t.stat","p.value")
  return(gg)
}


#' Model Summary Statistics
#'
#' @param data A data frame containing the variables to use
#' @param y The dependent variable
#' @param x The independent variables
#'
#' @return A tibble containing model summary stats: R-Squared, Adjusted R-Squared, AIC and BIC
#' @author Mutua Kilai
#' @export
#'
#' @examples
#' ols_model_stats(data = eduperform, "pi", c("hs", "ps"))
#'
#'
#'
ols_model_stats <- function(data,y,x)
{
  data[x] <- lapply(data[x], function(xx) {
    if (is.character(xx)) as.factor(xx) else xx
  })
  formula_str <- paste(y, "~", paste(x, collapse = " + "))
  formula <- stats::as.formula(formula_str)
  model <- stats::lm(formula, data = data)
  rs <- summary(model)$r.squared
  ars <- summary(model)$adj.r.squared
  aic <- stats::AIC(model)
  bic <- stats::BIC(model)
  out <- data.frame(rs, ars,aic,bic)
  out <- tibble::as_tibble(out)
  colnames(out) <- c("R.Squared", "Adj.R.Squared", "AIC", "BIC")
  return(out)
}

#' F-statistic attributes
#'
#' @param data A data frame containing the variables to use
#' @param y The dependent variable
#' @param x Set of independent variables
#'
#' @return A tibble containing the number of observations, F-Statistic, degrees of freedom and p-value
#' @author Mutua Kilai
#' @export
#'
#' @examples
#' ols_model_sig(data = eduperform, "pi", c("hs", "ps"))
#'
#'
#'
ols_model_sig <- function(data,y,x)
{
  data[x] <- lapply(data[x], function(xx) {
    if (is.character(xx)) as.factor(xx) else xx
  })
  formula_str <- paste(y, "~", paste(x, collapse = " + "))
  formula <- stats::as.formula(formula_str)
  model <- stats::lm(formula, data = data)
  f_statistic <- summary(model)$fstatistic[1]
  df1 <- summary(model)$fstatistic[2] # Numerator df
  df2 <- summary(model)$fstatistic[3] # Denominator df
  f_p_value <- stats::pf(f_statistic, df1, df2, lower.tail = FALSE)
  nobs <- nobs(model)
  df <- data.frame(nobs,f_statistic, df1, df2, f_p_value)
  df <- tibble::as_tibble(df)
  colnames(df) <- c("nobs", "F.Stat", "df1", "df2", "p.value")
  return(df)
}


#' Checking Overall Model Significance
#'
#' @param data A data frame containing the variables to use
#' @param y The dependent variable
#' @param x A set of independent variables
#'
#' @return p-value with a statement on whether the model is significant or not
#' @author Mutua Kilai
#' @export
#'
#' @examples
#' check_model_sig(data = eduperform, "pi", c("hs", "ps"))
#'
#'
check_model_sig <- function(data,y,x)
{
  data[x] <- lapply(data[x], function(xx) {
    if (is.character(xx)) as.factor(xx) else xx
  })
  formula_str <- paste(y, "~", paste(x, collapse = " + "))
  formula <- stats::as.formula(formula_str)
  model <- stats::lm(formula, data = data)
  f_statistic <- summary(model)$fstatistic[1]
  df1 <- summary(model)$fstatistic[2] # Numerator df
  df2 <- summary(model)$fstatistic[3] # Denominator df
  f_p_value <- stats::pf(f_statistic, df1, df2, lower.tail = FALSE)
  if(f_p_value < 0.05)
  {
    ot <- insight::print_color(
      sprintf(
        "Ok: The fitted model is significant (%s).\n",
        insight::format_p(f_p_value)
      ),
      "blue"
    )
  } else
  {
    ot <- insight::print_color(
      sprintf(
        "Warning: The fitted model is not significant (%s).\n",
        insight::format_p(f_p_value)
      ),
      "red"
    )
  }
  invisible(ot)
}


#' Confidence Intervals of Model Parameters
#'
#' @param data A data frame containing the variables to use
#' @param y The dependent variable
#' @param x A set of independent variables
#' @param level level of significance can be 0.95, 0.90 etc. default is 0.95
#'
#' @return tibble containing the lower and upper confidence intervals
#' @author Mutua Kilai
#' @export
#'
#' @examples
#' get_confint(data = eduperform, "pi", c("hs", "ps"))
#'
#'
get_confint <- function(data, y,x, level=0.95)
{
  data[x] <- lapply(data[x], function(xx) {
    if (is.character(xx)) as.factor(xx) else xx
  })
  formula_str <- paste(y, "~", paste(x, collapse = " + "))
  formula <- stats::as.formula(formula_str)
  model <- stats::lm(formula, data = data)
  gg <- data.frame(stats::confint(model, level = level))
  gg <- tibble::as_tibble(gg, rownames = "term")
  colnames(gg) <- c("coefficient", "Lower.CI", "Upper.CI")
  return(gg)
}


#' Obtaining only significant predictors from a model
#'
#' @param data A data frame containing the variables to use
#' @param y The dependent variable
#' @param x A set of independent variables
#' @param alpha desired alpha level. default is 0.05
#'
#' @return A tibble containing the significant predictors
#' @author Mutua Kilai
#' @export
#'
#' @examples
#' get_significant_predictors(data = eduperform, "pi", c("hs", "ps"))
#'
#'
#'
get_significant_predictors <- function(data, y, x, alpha = 0.05)
{
  data[x] <- lapply(data[x], function(xx) {
    if (is.character(xx)) as.factor(xx) else xx
  })
  formula_str <- paste(y, "~", paste(x, collapse = " + "))
  formula <- stats::as.formula(formula_str)
  model <- stats::lm(formula, data = data)
  model_summary <- summary(model)
  coefficients <- model_summary$coefficients
  significant_predictors <- tibble::as_tibble(coefficients[coefficients[, 4] < alpha, ], rownames = "term")
  colnames(significant_predictors) <- c("term","coefficient", "SE", "t.stat", "p.value")
  return(significant_predictors)
}



#' Get variance of the model coefficients
#'
#' @param data A data frame containing the variables to use
#' @param y The dependent variable
#' @param x A set of independent variables
#'
#' @return Tibble containing the variances
#' @author Mutua Kilai
#' @export
#'
#' @examples
#' get_coefficients_variance(data = eduperform, "pi", c("hs", "ps"))
#'
#'
get_coefficients_variance <- function(data, y, x) {
  data[x] <- lapply(data[x], function(xx) {
    if (is.character(xx)) as.factor(xx) else xx
  })
  formula_str <- paste(y, "~", paste(x, collapse = " + "))
  formula <- stats::as.formula(formula_str)
  model <- stats::lm(formula, data = data)
  vcov_matrix <- stats::vcov(model)
  coefficient_variances <- diag(vcov_matrix)
  variance_tibble <- tibble::tibble(
    Coefficient = names(coefficient_variances),
    Variance = coefficient_variances
  )
  return(variance_tibble)
}

#' Prediction from new observations
#'
#' @param model an lm object
#' @param new_data data frame containing the new set of predictors
#' @param level confidence level, default 0.95
#'
#' @return A tibble containing the predicted value and the upper and lower CI
#' @author Mutua Kilai
#' @export
#'
#' @examples
#' model <- lm(pi ~ hs + ps, data = eduperform)
#' newdata <- data.frame(hs =c(2,3,4),ps = c(23,24,12))
#' predict_dep_var(model, new_data = newdata, level = 0.95)
#'
#'
predict_dep_var <- function(model, new_data, level = 0.95)
{
  predi <- stats::predict(model, new_data,interval = "confidence", level = level)
  predictions_tibble <-tibble::tibble(
    Predicted = predi[, 1],
    Lower_CI = predi[, 2],
    Upper_CI = predi[, 3]
  )
  return(predictions_tibble)
}


#' Checking normality of residuals
#'
#' @param model A lm model object
#'
#' @return The p-value of the test statistic.
#' @author Mutua Kilai
#' @export
#'
#' @examples
#' model <- lm(pi ~ hs + ps, data = eduperform)
#' normality_assumption(model)
#'
normality_assumption <- function(model)
{
  if (!inherits(model, "lm")) stop("model is not an object of class 'lm'")
  res <- model$residuals
  p_value <- moments::jarque.test(res)$p.value
  if(p_value > 0.05)
  {
    otr <- insight::print_color(
      sprintf(
        "Ok: The residuals are normally distributed (%s).\n",
        insight::format_p(p_value)
      ),
      "blue"
    )
  } else
  {
    otr <- insight::print_color(
      sprintf(
        "Warning: The residuals are not normally distributed (%s).\n",
        insight::format_p(p_value)
      ),
      "red"
    )
  }
  invisible(otr)
}


#' Checking heteroscedasticity assumption
#'
#' @param model A lm model object
#'
#' @return The p-value of the test statistic.
#' @author Mutua Kilai
#' @export
#'
#' @examples
#' model <- lm(pi ~ hs + ps, data = eduperform)
#' heteroscedasticity_assumption(model)
#'
heteroscedasticity_assumption <- function(model)
{
  if (!inherits(model, "lm")) stop("model is not an object of class 'lm'")
  p_value <- lmtest::bptest(model)$p.value
  if(p_value > 0.05)
  {
    oth <- insight::print_color(
      sprintf(
        "Ok: Homoscedasticity(Constant error variance) (%s).\n",
        insight::format_p(p_value)
      ),
      "blue"
    )
  } else
  {
    oth <- insight::print_color(
      sprintf(
        "Warning: Heteroscedaticity(non-conatnt error variance) (%s).\n",
        insight::format_p(p_value)
      ),
      "red"
    )
  }
  invisible(oth)
}



#' Multicollinearity Assumption
#'
#' @param model A lm object
#'
#' @return A tibble containing the VIF and Tolerance values
#' @author Mutua Kilai
#' @export
#'
#' @examples
#' model <- lm(pi ~ hs + ps, data = eduperform)
#' multicollinearity_assumption(model)
#'
#'
multicollinearity_assumption <- function(model)
{
  if (!inherits(model, "lm")) stop("model is not an object of class 'lm'")
  ou <- car::vif(model)
  lk <- data.frame(ou)
  colnames(lk) <- "VIF"
  lk$Tolerance <- 1/(lk$VIF)
  return(lk)
}


#' Check model for residual independence
#' @description
#' Checks model for independence of residuals
#'
#'
#' @param model A lm object
#'
#' @return returns the p-value for the test
#' @author Mutua Kilai
#' @export
#'
#' @examples
#' model <- lm(pi ~ hs + ps, data = eduperform)
#' autocorrelation_assumption(model)
autocorrelation_assumption <- function(model)
{
  p_value <- car::durbinWatsonTest(model)$p
  if(p_value > 0.05)
  {
    ota <- insight::print_color(
      sprintf(
        "Ok: Residuals are not autocorrelated (%s).\n",
        insight::format_p(p_value)
      ),
      "blue"
    )
  } else
  {
    ota <- insight::print_color(
      sprintf(
        "Warning: Residuals are correlated (%s).\n",
        insight::format_p(p_value)
      ),
      "red"
    )
  }
  invisible(ota)
}


#' Check Series for Weak Stationarity
#'
#' @param x A numeric vector or time series object
#'
#' @return p-value of the test
#' @author Mutua Kilai
#' @export
#'
#' @examples
#' data(keconomy)
#' attach(keconomy)
#' check_stationarity(UR)
#'
check_stationarity <- function(x)
{

  p_value <- tseries::adf.test(x)$p.value
  if(p_value < 0.05)
  {
    sta <- insight::print_color(
      sprintf(
        "Ok: The series is stationary (%s).\n",
        insight::format_p(p_value)
      ),
      "blue"
    )
  } else
  {
    sta <- insight::print_color(
      sprintf(
        "Warning: The series is not stationary (%s).\n",
        insight::format_p(p_value)
      ),
      "red"
    )
  }
  invisible(sta)
}


#' Choosing Best Model Based on AIC, BIC and Adjusted R Squared
#'
#' @param models a list of models
#' @param criterion The criterion to select optimal model. Default AIC
#'
#' @return list of the results and best model
#' @author Mutua Kilai
#' @export
#'
#' @examples
#' data(eduperform)
#' model1 <- lm(pi ~ hs, data = eduperform)
#' model2 <- lm(pi ~ hs + ps, data = eduperform)
#' model3 <- lm(pi ~ hs + ps + sh, data = eduperform)
#' models <- list(model1, model2, model3)
#'
#' select_optimal_model(models, criterion= "AIC")
#'
select_optimal_model <- function(models, criterion ="AIC")
{
  if (!is.list(models)) {
    stop("The 'models' argument must be a list of linear models.")
  }

  valid_criteria <- c("AIC", "BIC", "AdjR2")
  if (!(criterion %in% valid_criteria)) {
    stop(paste("Invalid criterion. Choose one of:", paste(valid_criteria, collapse = ", ")))
  }

  scores <- sapply(models, function(model) {
    if (criterion == "AIC") {
      return(stats::AIC(model))
    } else if (criterion == "BIC") {
      return(stats::BIC(model))
    } else if (criterion == "AdjR2") {
      return(summary(model)$adj.r.squared)
    }
  })

  if (criterion %in% c("AIC", "BIC")) {
    best_index <- which.min(scores)
  } else if (criterion == "AdjR2") {
    best_index <- which.max(scores)
  }

  results <- data.frame(
    Model = paste0("Model", seq_along(models)),
    Score = scores
  )

  if (criterion %in% c("AIC", "BIC")) {
    results <- results[order(results$Score), ]
  } else if (criterion == "AdjR2") {
    results <- results[order(-results$Score), ]
  }

  list(
    results = results,
    best_model = models[[best_index]]
  )
}


#' Plots ACF of a univariate time series
#'
#' @param x numeric vector
#' @param lag.max maximum lag to calculate the acf
#'
#' @return a plot of the acf vs lag
#' @author Mutua Kilai
#' @export
#'
#' @examples
#' data(keconomy)
#' attach(keconomy)
#' ACF_PLOT(UR)
#'
#'
#'
ACF_PLOT <- function(x, lag.max = NULL)
{
  acf_res <- stats::acf(x, plot = FALSE)
  n <- length(x)
  conf_limit <- 2 / sqrt(n)
  acf_df <- data.frame(
    Lag <- acf_res$lag,
    ACF <- acf_res$acf
  )
  ggplot2::ggplot(acf_df, ggplot2::aes(x = Lag, y = ACF)) +
    ggplot2::geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
    ggplot2::geom_hline(yintercept = conf_limit, color = "blue", linetype = "dotted",linewidth = 1.5) +
    ggplot2::geom_hline(yintercept = -conf_limit, color = "blue", linetype = "dotted", linewidth = 1.5)+
    ggplot2::geom_segment(ggplot2::aes(xend = Lag, yend = 0), color = "blue") +
    ggplot2::geom_point(color = "red") +
    ggplot2::theme_minimal()
}

#' Plots PACF of a univariate time series
#'
#' @param x a numeric vector
#' @param lag.max maximum lag to calculate pacf
#'
#' @return a plot of the pacf vs lag
#' @author Mutua Kilai
#' @export
#'
#' @examples
#' data(keconomy)
#' attach(keconomy)
#' PACF_PLOT(UR)
#'
#'
#'
PACF_PLOT <- function(x, lag.max = NULL)
{
  pacf_res <- stats::pacf(x, plot = FALSE)
  n <- length(x)
  conf_limit <- 2 / sqrt(n)
  pacf_df <- data.frame(
    Lag <- pacf_res$lag,
    PACF <- pacf_res$acf
  )
  ggplot2::ggplot(pacf_df, ggplot2::aes(x = Lag, y = PACF)) +
    ggplot2::geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
    ggplot2::geom_hline(yintercept = conf_limit, color = "blue", linetype = "dotted",linewidth = 1.5) +
    ggplot2::geom_hline(yintercept = -conf_limit, color = "blue", linetype = "dotted", linewidth = 1.5)+
    ggplot2::geom_segment(ggplot2::aes(xend = Lag, yend = 0), color = "blue") +
    ggplot2::geom_point(color = "red") +
    ggplot2::theme_minimal()
}



#' Fit ARIMA models to univariate data
#'
#' @param data a univariate class object or a vector
#' @param p AR order
#' @param d differencing order
#' @param q MA order
#'
#' @returns A tibble containing the estimate, SE and p-value
#' @export
#'
#' @examples
#' data(keconomy)
#' attach(keconomy)
#' fit_arima(UR, p=2,d=0,q=3)
#'
fit_arima <- function(data, p, d, q) {
  fit <- forecast::Arima(data, order = c(p, d, q))
  estimates <- stats::coef(fit)
  se <- sqrt(diag(stats::vcov(fit)))
  p_values <- 2 * (1 - stats::pnorm(abs(estimates / se)))
  results <- tibble::tibble(
    Parameter = names(estimates),
    Estimate = estimates,
    Std_Error = se,
    P_Value = p_values
  )

  return(results)
}

#' Select Optimal Model based on BIC
#'
#' @param data A univariate ts object
#' @param max_p Maximum AR order
#' @param max_d Maximum differencing order
#' @param max_q Maximum MA order
#'
#' @returns A list containing the optimal model results and the BIC value
#' @export
#'
#' @examples
#' data(keconomy)
#' attach(keconomy)
#' best_arima(UR, max_p = 5, max_d = 2, max_q = 5)
#'
best_arima <- function(data, max_p = 5, max_d = 2, max_q = 5) {
  best_model <- NULL
  best_bic <- Inf

  for (p in 0:max_p) {
    for (d in 0:max_d) {
      for (q in 0:max_q) {
        tryCatch({
          model <- forecast::Arima(data, order = c(p, d, q))
          bic <- stats::BIC(model)

          if (bic < best_bic) {
            best_model <- model
            best_bic <- bic
          }
        }, error = function(e) {
          NULL
        })
      }
    }
  }

  list(
    Best_Model = best_model,
    Best_BIC = best_bic
  )
}
