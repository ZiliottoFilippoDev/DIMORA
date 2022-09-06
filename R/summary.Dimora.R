summary.Dimora <- function(object,...){
  if(is.list(object$residuals)==FALSE){
    cat("Call: (",object$type,")\n\n  ")
    print(object$call)
    cat("\n")
    cat("Residuals:\n ")
    print(summary(object$residuals))
    cat("\n")
    assegna.stelle <- function(object){
      if(object<0.001) return("***")
      else if(object>=0.001 & object<0.01) return("**")
      else if(object>=0.01 & object<0.05) return("*")
      else if(object>=0.05 & object<0.1) return(".")
      else return(" ")
    }
    stelle <- sapply(object$Estimate$`p-value`,assegna.stelle)
    fitfit <- cbind(object$Estimate,stelle)
    colnames(fitfit) <- c(colnames(object$Estimate)," ")
    cat("Coefficients:\n ")
    print(fitfit)
    cat("---\n Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1")
    cat("\n\n")
    cat(" Residual standard error ", sd(object$residuals), " on ", length(object$residuals)-nrow(fitfit), " degrees of freedom\n")
    cat(" Multiple R-squared:  ",object$Rsquared," Residual squared sum: ", object$RSS)

    return(invisible(object))
  }

  if(is.list(object$residuals)==TRUE){
    cat("Call: (",object$type,")\n\n  ")
    print(object$call)
    cat("\n")
    cat("Residuals:\n ")
    print(summary(object$Res_tot))
    cat("\n")
    assegna.stelle <- function(object){
      if(object<0.001) return("***")
      else if(object>=0.001 & object<0.01) return("**")
      else if(object>=0.01 & object<0.05) return("*")
      else if(object>=0.05 & object<0.1) return(".")
      else return(" ")
    }
    stelle <- sapply(object$Estimate$`p-value`,assegna.stelle)
    fitfit <- cbind(object$Estimate,stelle)
    colnames(fitfit) <- c(colnames(object$Estimate)," ")
    cat("Coefficients:\n ")
    print(fitfit)
    cat("---\n Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1")
    cat("\n\n")
    cat(" Residual standard error ", sd(object$Res_tot), " on ", length(object$Res_tot)-nrow(fitfit), " degrees of freedom\n")
    cat(" Multiple R-squared:  ",object$Rsquared," Residual squared sum: ", object$RSS)

    return(invisible(object))

  }
}
