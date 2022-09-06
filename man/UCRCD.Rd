\name{UCRCD}
\alias{UCRCD}
\title{UCRCD model}
\description{
Function that estimates the Unbalanced Competition Regime Change Diachronic model. Fitted values for cumulative and instantaneous data are displayed (if \code{display = T}).
}



\usage{
UCRCD(series1, series2, display=T, alpha=0.05,
      delta=0.01, gamma=0.01, par="double",
      m1  = BM(series1,display = F)$Estimate[1,1],
      m2  = BM(series2,display = F)$Estimate[1,1],
      p1c = BM(series1,display = F)$Estimate[2,1],
      q1c = BM(series1,display = F)$Estimate[3,1],
      p2  = BM(series2,display = F)$Estimate[2,1],
      q2  = BM(series2,display = F)$Estimate[3,1])
}

\arguments{
\item{series1}{the istantaneous observed data of the first product.}
\item{series2}{the istantaneous observed data of the second product.}
\item{alpha}{the significance level for confidence intervals.}

\item{par}{default is \code{'double'}. Instead \code{'unique'}, meaning the costraint delta equal to gamma has been selected.}
\item{display}{if \code{TRUE} returns the predicted values for cumulative and instantaneous observed data.}
\item{delta}{preliminary estimate of delta.}
\item{gamma}{preliminary estimate of gamma.}
\item{m1}{preliminary estimate of the first product's market potential under competition}
\item{m2}{preliminary estimate of the second product's market potential }
\item{p1c}{preliminary estimate of the first product's innovation coefficient under competition}
\item{p2}{preliminary estimate of the second product's innovation coefficient}
\item{q1c}{preliminary estimate of the first product's imitation coefficient under competition}
\item{q2}{preliminary estimate of the second product's imitation coefficient}
}

\details{
The model estimates several parameters that refer both to the stand-alone phase and the competition phase. The description of the parameters is reported below: (wom = "Word Of Mouth")
\itemize{
\item{\code{ma :}        }{Market Potential 1 (stand-alone phase)}
\item{\code{p1a :}       }{Innovation coefficient 1 (stand-alone phase)}
\item{\code{q1a :}       }{Imitation coefficient 1 (stand-alone phase)}
\item{\code{mc :}        }{Market Potential 1 (competition phase)}
\item{\code{p1c :}       }{Innovation coefficient 1 (competition phase)}
\item{\code{q1c+delta :} }{Within-product wom 1 (competition phase)}
\item{\code{q1c :}       }{Cross-product wom 1 (competition phase)}
\item{\code{p2 :}       }{Innovation coefficient 2}
\item{\code{q2 :}       }{Within-product wom 2}
\item{\code{q2-gamma :} }{Cross-product wom 2}
}



}

\value{
\code{UCRCD} returns an object of class "\code{Dimora}".

The function \code{summary} is used to obtain and print a summary table of the results. The generic accessor functions \code{coefficients}, \code{fitted} and \code{residuals} extract various useful features of the value returned by \code{UCRCD}.

An object of class "\code{Dimora}" is a list containing at least the following components:

\item{model}{the model formula used.}
\item{type}{the model frame used.}
\item{Estimate}{a summary table of estimates.}
\item{coefficients}{a named vector of coefficients.}
\item{Rsquared}{the statistical measure R-squared (on the istantaneous data).}
\item{RSS}{the residual sum of squares (on the istantaneous data).}
\item{residuals}{the residuals (observed cumulative data - fitted cumulative data).}
\item{fitted}{the cumulative fitted values.}
\item{data}{the cumlative observed data.}
\item{call}{the matched call.}

The UCRCD model is estimated on the instantaneous data. For this reason, the UCRCD function also returns the following elements:

\item{data.i}{the instantaneous observed data.}
\item{fitted.i}{the instantaneous fitted values.}
\item{residuals.i}{the residuals (observed instantaneous data - fitted instantaneous data).}

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

Guseo, R., & Mortarino, C. (2014). Within-brand and cross-brand word-of-mouth for sequential multi-innovation diffusions. IMA Journal of Management Mathematics, 25(3), 287-311.
}


\seealso{

The Dimora models: \code{\link{BM}}, \code{\link{GBM}}, \code{\link{GGM}}.

\code{\link{summary.Dimora}} for summaries.

\code{\link{plot.Dimora}} for graphics and residuals analysis.

\code{\link{predict.Dimora}} for prediction.

\code{\link{make.instantaneous}} to create instantaneous series from the cumulative one.

}

\examples{
data(DBdimora)
Gd <- DBdimora$Gdenmark[1:36]
Rd <- DBdimora$Rdenmark[13:36]
Ca <- DBdimora$Caustralia
Ra <- DBdimora$Raustralia[26:56]

## Example 1
M8 <- UCRCD(Gd,Rd)
summary(M8)
plot.Dimora(M8,type="fit")

## Example 2
M9 <- UCRCD(Ca,Ra,par = "unique",display=FALSE)
summary(M9)
plot.Dimora(M9)

}
