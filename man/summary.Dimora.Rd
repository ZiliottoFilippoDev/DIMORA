\name{summary.Dimora}
\alias{summary.Dimora}
\title{Summary method for Dimora objects}

\description{
  Summary method for the \code{Dimora} objects.
}

\usage{
\method{summary}{Dimora}(object,...)
}


\arguments{
  \item{object}{an object of class \code{Dimora}, as that
    created by \code{BM}, \code{GBM}, \code{GGM} or \code{UCRCD}.}
  \item{...}{additional arguments affecting the summary produced.}
}

\value{
  \code{summary} is a generic function used to produce summaries of the results of various model fitting functions. The function invokes particular methods which depend on the class of the first argument; in this case, the class is \code{Dimora}.}


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

\code{\link{plot.Dimora}} for graphics and residuals analysis.

\code{\link{predict.Dimora}} for prediction.

\code{\link{make.instantaneous}} to create instantaneous series from the cumulative one.

}


\examples{
data(DBdimora)
iphone <- DBdimora$iPhone[7:52]

M1 <- BM(iphone)
summary(M1)

}
