
\name{GGM}
\alias{GGM}
\title{Guseo-Guidolin model}
\description{Function that estimates the dynamic market potential model. Fitted values for cumulative and instantaneous data are displayed (if \code{display = T}). Out-of-sample prediction is performed based on estimated parameters. Function \code{m(t)} is defined, as default, as the standard Guseo-Guidolin model, but it can also be defined as a general function with some constraints (see \code{Details} and \code{exmaples}).
}


\usage{
GGM(series, prelimestimates = NULL, mt = 'base', alpha = 0.05,
       oos = round(length(series)*0.25), display = T)
}

\arguments{
\item{series}{the instantaneous observed data.}
\item{prelimestimates}{a vector containing the preliminary estimates of the parameters (see \code{Details} and \code{Examples}).}
\item{mt}{function that define the dynamic market potential.}
\item{alpha}{the significance level for the confidence intervals.}
\item{oos}{positive integer value: number of predictions after the last observed one. Default setting to 25\% of the length of the data.}
\item{display}{if \code{TRUE} returns the fitted values for cumulative and instantaneous observed data. If 'oos' is specified, it also returns the predicted fit values.}
}




\details{
The \code{GGM} function allows to define the parameter \code{m(t)} in two ways:

\itemize{
 \item{use the standard GGM model. Note: define the
       \code{prelimestimates = c(K, qc, pc, qs, ps)} (see Example 1 below).}
 \item{define function \code{m(t)} according to modeling needs. Note: function \code{m(t)} must be positive and is essential a c.d.f.; default values for prelimestimates are based on the BM function (see Example 2 below).}}
}



\value{
\code{GGM} returns an object of class "\code{Dimora}".
The function \code{summary} is used to obtain and print a summary table of the results. The generic accessor functions \code{coefficients}, \code{fitted} and \code{residuals} extract various useful features of the value returned by \code{GGM}.

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

   \item{Bessi Alessandro: }{\email{alessandrobessi92@gmail.com}
    \itemize{
        \item{\url{https://www.linkedin.com/in/alessandro-bessi-419812194/}}
              }}
   }
}


\references{
Guidolin, M. (2023). Innovation Diffusion Models: Theory and Practice, First Edition. John Wiley & Sons Ltd.

Guseo, R., & Guidolin, M. (2009). Modelling a dynamic market potential: A class of automata networks for diffusion of innovations. Technological Forecasting and Social Change, 76(6), 806-820.
}


\seealso{

The Dimora models: \code{\link{BM}}, \code{\link{GBM}}, \code{\link{UCRCD}}.

\code{\link{summary.Dimora}} for summaries.

\code{\link{plot.Dimora}} for graphics and residuals analysis.

\code{\link{predict.Dimora}} for prediction.

\code{\link{make.instantaneous}} to create instantaneous series from the cumulative one.

}

\examples{
data(DBdimora)
iphone <- DBdimora$iPhone[7:52]

## Example 1
M6 <- GGM(iphone, prelimestimates=c(1823, 0.001, 0.1, 0.001, 0.1), oos=100)
# 1823 refers to the estimated parameter m of the standard Bass model on iphone
summary(M6)
plot.Dimora(M6, oos=25)
# 25 predictions

## Example 2
M7 <- GGM(iphone, mt = function(x) pchisq(x,10))
summary(M7)
}


