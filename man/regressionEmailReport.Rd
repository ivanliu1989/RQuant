% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/regressionEvaluation.R
\name{regressionEmailReport}
\alias{regressionEmailReport}
\title{Email a Regression Evaluation Report}
\usage{
regressionEmailReport(evaluationResults, toAddress = "your.name@gmail.com",
  additionalMessage = "", plotSize = 10)
}
\arguments{
\item{evaluationResults}{The result from running \code{regressionEvaluation}.}

\item{toAddress}{Character vector, containing the email address(es) to send the report to, when completed.}

\item{additionalMessage}{A string that will appear in the subject of the email}

\item{plotSize}{Numeric scalar containing the side length of the resulting square png files, in pixels.}
}
\value{
The result of the call to \code{sendmail}
}
\description{
Given the result of \code{regressionEvaluation} email a standardised report to one or more recipients.

The emailed report contains a residual vs fitted plot,
a residual vs order plot,
a histogram of actual, predictions and residuals' distributions,
a prediction intervals plot,
and a plot of cumulative % qualified versus cumulative % targetted.
If the model is provided, some basic descriptions about the model will also be reported.
If the model provided is a glm, \code{glm.check.assumptions} function will be used to check the assumptions of the model(e.g. linearity).
}
\examples{
set.seed(123)
data(mtcars)
fit <- glm(mpg ~ cyl + disp + hp + drat, data=as.data.frame(mtcars), family = gaussian())
actuals <- mtcars$mpg
probs <- predict(fit, mtcars)
res <- regressionEvaluation(pred = probs,
obs = actuals,
model = fit,
sample_size=1e5,
seed = 1234)

regressionEmailReport(res,"ivan@xxx.com")

}
\seealso{
\code{\link{RQuantSendMail}}, \code{\link{regressionEvaluation}}
}

