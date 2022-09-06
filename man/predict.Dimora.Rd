\name{predict.Dimora}
\alias{predict.Dimora}
\title{Predict method for Dimora objects

}
\description{
  Prediction of test data using any model of the package.
}

\usage{
\method{predict}{Dimora}(object, ..., newx)
}

\arguments{
  \item{object}{an object of class \code{Dimora}, as that
    created by \code{BM}, \code{GBM} or \code{GGM}.}
  \item{newx}{a number or a vector of numeric values rapresenting the time window (see \code{Example} below).}
  \item{...}{additional arguments affecting the predictions produced.}
}

\value{
  The method returns a vector of predicted values.}

\details{It works only for univariate models; specifically, for the models \code{\link{BM}}, \code{\link{GBM}}, \code{\link{GGM}}, it returns a vector of the cumulative predicted values.}


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


\seealso{

The Dimora models: \code{\link{BM}}, \code{\link{GBM}}, \code{\link{GGM}}, \code{\link{UCRCD}}.

\code{\link{summary.Dimora}} for summaries.

\code{\link{plot.Dimora}} for graphics and residuals analysis.

\code{\link{make.instantaneous}} to create instantaneous series from the cumulative one.

}

\examples{
data(DBdimora)
iphone <- DBdimora$iPhone[7:52]
M1 <- BM(iphone)

# Predict the values of the observed series (lenght(iphone)=46)
predict.Dimora(M1, newx = c(1:46))


# Predict the values of the observed series and other 34 future values
predict.Dimora(M1, newx = c(1:80))

}



