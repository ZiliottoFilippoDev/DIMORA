\name{make.instantaneous}
\alias{make.instantaneous}
\title{Function that transforms your data}
\description{Function that transforms cumulative data into instantaneous; this function can be useful because all the model functions in this package require the istantaneous data as input.}

\usage{
make.instantaneous(cumulate.data)
}

\arguments{
\item{cumulate.data}{ the cumulative data.}
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



\seealso{

The Dimora models: \code{\link{BM}}, \code{\link{GBM}}, \code{\link{GGM}}, \code{\link{UCRCD}}.

\code{\link{summary.Dimora}} for summaries.

\code{\link{plot.Dimora}} for graphics and residuals analysis.

\code{\link{predict.Dimora}} for prediction.

}


\examples{
cumulate.data = c(1,2,3,6,12)
data.inst = make.instantaneous(cumulate.data)
}
