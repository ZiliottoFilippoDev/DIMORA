\name{GBM}
\alias{GBM}
\title{Generalized Bass model}
\description{Function that estimates the Generalized Bass model with Exponential, Rectangular, or Mixed shock. Fitted values for cumulative and instantaneous data are displayed (if \code{display = T}). Out-of-sample prediction is performed based on estimated parameters.}

\usage{
GBM(series, shock = c("exp", "rett", "mixed"), nshock,
                    prelimestimates, alpha = 0.05,
                    oos = round(length(series)*0.25), display = T)
}

\arguments{
\item{series}{the instantaneous observed data.}
\item{shock}{the parameters which define the shocks. The available options are:
\itemize{
\item{\code{"exp" = Exponential};} \item{ \code{"rett" = Rectangular};} \item{\code{"mixed" = Mixed} (only when nshock = 2)}.}}
\item{nshock}{the number of shocks (from 1 to 3).}
\item{prelimestimates}{a vector containing the preliminary estimates of the parameters (see \code{Details} and \code{Examples}).}
\item{alpha}{the significance level for confidence intervals.}
\item{oos}{positive integer value: number of predictions after the last observed one. Default setting to 25\% of the length of the data.}
\item{display}{if \code{TRUE} returns the fitted values for cumulative and instantaneous observed data. If 'oos' is specified, it also returns the predicted fit values.}

}


\details{Each type of shock is characterized by specific parameters.

The analyst has to set both the preliminary estimates of the parameters \code{m, p, q} and the ones related to the shock(s) \code{a, b, c}.
The parameters related to each shock have to be defined as follows:

\itemize{

  \item{Exponential:}{

     \itemize{\item{\code{a =}  starting time of the shock}
              \item{\code{b =}  memory of the effect (typically negative, suggesting an exponentially decaying behavior)}
              \item{\code{c =} intensity of the shock (may be either positive or negative)}
              }
    In case of more than one shock, preliminary estimates need to be specified as follows:
    \code{prelimestimates = c(m, p, q,  a1, b1, c1,   a2, b2, c2,   a3, b3, c3)}
             }

  \item{Rectangular:}{

     \itemize{\item{\code{a =}  starting time of the shock}
              \item{\code{b =}  ending time of the shock}
              \item{\code{c =}  intensity of the shock (may be either positive or negative)}
              }
        In case of more than one shock, preliminary estimates need to be specified as follows: \code{prelimestimates = c(m, p, q,  a1, b1, c1,   a2, b2, c2,   a3, b3, c3)}
             }


  \item{Mixed: when the series has one exponential and one rectangular shock (you always have to specify the exponential shock before the rectangular one, even if the exponential one occurs later)}{

     \itemize{\item{\code{a1 =}  starting time of the exponential shock}
              \item{\code{b1 =}  memory of the effect (typically negative, suggesting an exponentially decaying behavior)}
              \item{\code{c1 =}  intensity of the exponential shock (may be either positive or negative)}
              }
     \itemize{\item{\code{a2 =}  starting time of the rectangular shock}
              \item{\code{b2 =}  ending time of the rectangular shock}
              \item{\code{c2 =}  intensity of the rectangular shock (may be either positive or negative)}
              }
             }

 }


}

\value{
\code{GBM} returns an object of class "\code{Dimora}".

The function \code{summary} is used to obtain and print a summary table of the results. The generic accessor functions \code{coefficients}, \code{fitted} and \code{residuals} extract various useful features of the value returned by \code{GBM}.

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

Bass, F.M., Krishnan, T.V., & Jain, D.C. (1994). Why the Bass model fits without decision variables. Marketing science, 13 (3), 203-223.
}




\seealso{

The Dimora models: \code{\link{BM}}, \code{\link{GGM}}, \code{\link{UCRCD}}.

\code{\link{summary.Dimora}} for summaries.

\code{\link{plot.Dimora}} for graphics and residuals analysis.

\code{\link{predict.Dimora}} for prediction.

\code{\link{make.instantaneous}} to create instantaneous series from the cumulative one.
}


\examples{
data(DBdimora)
iphone<- DBdimora$iPhone[7:52]
imac<- DBdimora$iMac[1:52]

## Example 1: exponential shock
M3 <- GBM(iphone, shock = "exp", nshock = 1,
          prelimestimates = c(BM(iphone, display=FALSE)$Estimate[1,1],
                              BM(iphone, display=FALSE)$Estimate[2,1],
                              BM(iphone, display=FALSE)$Estimate[3,1],
                              17,-0.1,0.1))
summary(M3)
plot.Dimora(M3, oos=25)
# 25 predictions

## Example 2: rectangular shock
M4 <- GBM(imac,shock = "rett",nshock = 1,
           prelimestimates = c(BM(imac, display=FALSE)$Estimate[1,1],
                               BM(imac, display=FALSE)$Estimate[2,1],
                               BM(imac, display=FALSE)$Estimate[3,1],
                               20,30,0.1), oos=20)
summary(M4)

## Example 3: mixed shock
## The prelimestimates of m, p, q are their relative values estimated through M4.

M5 <- GBM(imac,shock = "mixed",nshock = 2,
           prelimestimates = c(M4$Estimate[1,1],
                               M4$Estimate[2,1],
                               M4$Estimate[3,1],
                               6,-0.1,0.1, 20,30,0.1), oos=0)
summary(M5)




}

