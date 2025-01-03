% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ecometrics.R
\name{predict_dep_var}
\alias{predict_dep_var}
\title{Prediction from new observations}
\usage{
predict_dep_var(model, new_data, level = 0.95)
}
\arguments{
\item{model}{an lm object}

\item{new_data}{data frame containing the new set of predictors}

\item{level}{confidence level, default 0.95}
}
\value{
A tibble containing the predicted value and the upper and lower CI
}
\description{
Prediction from new observations
}
\examples{
model <- lm(pi ~ hs + ps, data = eduperform)
newdata <- data.frame(hs =c(2,3,4),ps = c(23,24,12))
predict_dep_var(model, new_data = newdata, level = 0.95)


}
\author{
Mutua Kilai
}
