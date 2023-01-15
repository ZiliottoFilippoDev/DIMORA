\name{DIMORA}
\alias{DIMORA}
\title{DIMORA Package}
\description{The main functions of the package are \code{BM, GBM, GGM}, and \code{UCRCD}. See the full documentation for more details.
At the end of this page there are links to the functions.

Note 1: the names of the model functions have been changed! \code{BM} refers to the previous \code{BASS.standard}, \code{GMB} to the \code{BASS.generalized}, and \code{GGM} to the \code{GG.model}.

Note 2: the SARMAX.refinement has been deleted from the previous version. Function \code{Arima()} of the  \code{forecast} library works to implement the SARMAX refinement.

Note 3: there is a new DIMORA dataset (DBdimora) which refers to the examples reported on the help files of the functions.}


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

The Dimora models: \code{\link{BM}}, \code{\link{GBM}}, \code{\link{GGM}}, \code{UCRCD}.

\code{\link{summary.Dimora}} for summaries.

\code{\link{plot.Dimora}} for graphics and residuals analysis.

\code{\link{predict.Dimora}} for prediction.

\code{\link{make.instantaneous}} to create instantaneous series from the cumulative one.

}



