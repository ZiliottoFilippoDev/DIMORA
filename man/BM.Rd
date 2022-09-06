\name{BM}
\alias{BM}
\title{Standard Bass model}
\description{Function that estimates the standard Bass model. Fitted values for cumulative and instantaneous data are displayed (if \code{display = T}). Out-of-sample prediction is performed based on estimated parameters.}


\usage{
BM(series, method = "nls", prelimestimates = c(sum(series) + 100, 0.01, 0.1),
   oos = round(length(series)*0.25), alpha = 0.05 ,display = T)
}

\arguments{
\item{series}{the instantaneous observed data.}
\item{method}{the estimation method, 'nlm' or 'optim' (see \code{Details}).}
\item{prelimestimates}{a vector containing the starting values used by the algorithm to estimate the parameters. If no values are specified, the default ones are:
\itemize{\item{market potential: \eqn{m = \sum_{t}(series)+100};} \item{innovation coefficient: \eqn{p = 0.01};} \item{imitation coefficient: \eqn{q = 0.1}. }}}
\item{alpha}{the significance level for the confidence intervals.}
\item{oos}{positive integer value: number of predictions after the last observed one. Default setting to 25\% of the length of the data.}
\item{display}{if \code{TRUE} returns the fitted values for cumulative and instantaneous observed data. If 'oos' is specified, it also returns the predicted fit values.}
}


\details{
The \code{optim} method provides only the parameter estimates. It does not provide the standard error and the p-value estimates.
}



\value{
\code{BM} returns an object of class "\code{Dimora}".

The function \code{summary} is used to obtain and print a summary table of the results. The generic accessor functions \code{coefficients}, \code{fitted} and \code{residuals} extract various useful features of the value returned by \code{BM}.

An object of class "\code{Dimora}" is a list containing at least the following components:

\item{model}{the model formula used.}
\item{type}{the model frame used.}
\item{Estimate}{a summary table of estimates.}
\item{coefficients}{a named vector of coefficients.}
\item{Rsquared}{the statistical measure R-squared.}
\item{RSS}{the residual sum of squares.}
\item{residuals}{the residuals (observed cumulative data - fitted cumulative data).}
\item{fitted}{the cumulative fitted values.}
\item{data}{the cumulative observed series.}
\item{call}{the matched call.}

}


\author{
 \itemize{

   \item{Zanghi Federico: }{\email{federico.zanghi.11@gmail.com}
    \itemize{
        \item{\url{https://www.linkedin.com/in/federico-zanghi-75512b165/}}
        \item{\url{https://github.com/federicozanghi}}
              }}
   \item{Savio Andrea: }{\email{svandr97@gmail.com}
    \itemize{
       \item{\url{https://www.linkedin.com/in/andrea-savio-51861b191/}}
             }}
   \item{Ziliotto Filippo: }{\email{filippo.ziliotto1996@gmail.com}
    \itemize{
        \item{\url{https://www.linkedin.com/in/FilippoZiliottoDev/}}
              }}
   }
}

\references{
Guidolin, M. (2023). Innovation Diffusion Models: Theory and Practice, First Edition. John Wiley & Sons Ltd.

Bass, F.M. (1969). A new product growth for model consumer durables. Management science, 15 (5), 215-227.
}


\seealso{

The Dimora models: \code{\link{GBM}}, \code{\link{GGM}}, \code{\link{UCRCD}}.

\code{\link{summary.Dimora}} for summaries.

\code{\link{plot.Dimora}} for graphics and residuals analysis.

\code{\link{predict.Dimora}} for prediction.

\code{\link{make.instantaneous}} to create instantaneous series from the cumulative one.

}

\examples{
data(DBdimora)
iphone <- DBdimora$iPhone[7:52]

## Example 1
M1 <- BM(iphone)
summary(M1)
plot.Dimora(M1)
plot.Dimora(M1, oos=25)
# 25 predictions


## Example 2
M2 <- BM(iphone, prelimestimates = c(2000, 0.001, 0.1), method = "optim", oos = 100)
summary(M2)

}


