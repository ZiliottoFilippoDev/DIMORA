\name{plot.Dimora}
\alias{plot.Dimora}
\title{Plot method for Dimora models}
\description{
  Plot method for the DIMORA package models.
}

\usage{
\method{plot}{Dimora}(x,..., type = c('all','res','fit'), oos = 0)
}

\arguments{
  \item{x}{an object of class \code{Dimora}.}

  \item{type}{specifies the type of plot. Default setting is \code{'all'}:
  \itemize{ \item{\code{'fit'} returns the cumulative and instantaneous fit of the model;} \item{\code{'res'} returns the 'Residuals VS Time' graphic and the ACF of the residuals of the model;} \item{\code{'all'} returns the two types before combined in an unique graphical window.}}}
  \item{oos}{positive integer value: number of predictions after the last observed one. If specified, it also determines the xlim for the plots. It only works for univariate models.}
  \item{...}{other graphics parameters.}
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


\seealso{
The Dimora models: \code{\link{BM}}, \code{\link{GBM}}, \code{\link{GGM}}, \code{\link{UCRCD}}.

\code{\link{summary.Dimora}} for summaries.

\code{\link{predict.Dimora}} for prediction.

\code{\link{make.instantaneous}} to create instantaneous series from the cumulative one.


}

\examples{
data(DBdimora)
iphone <- DBdimora$iPhone[7:52]
Gd <- DBdimora$Gdenmark[1:36]
Rd <- DBdimora$Rdenmark[13:36]

## Plot with univariate model
M1 <- BM(iphone,display = FALSE)
plot.Dimora(M1,type='all',oos = 20)

## Plot with multivariate model
M8 <- UCRCD(Gd,Rd)
plot.Dimora(M8,type='fit')

}

