summary.Dimora <- function(object,...){
  if(is.list(object$residuals)==FALSE){
    cat("Call: (",object$type,")\n\n  ")
    print(object$call)
    cat("\n")
    cat("Residuals:\n ")
    print(summary(object$residuals))
    cat("\n")
    assign.stars <- function(object){
      if(object<0.001) return("***")
      else if(object>=0.001 & object<0.01) return(" **")
      else if(object>=0.01 & object<0.05) return("  *")
      else if(object>=0.05 & object<0.1) return("  .")
      else return(" ")
    }
    stars <- sapply(object$Estimate$`p-value`,assign.stars)
    fitfit <- cbind(object$Estimate,stars)
    colnames(fitfit) <- c(colnames(object$Estimate)," ")
    cat("Coefficients:\n ")
    print(fitfit)
    cat("---\n Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1")
    cat("\n\n")
    cat(" Residual standard error ",
        round(sqrt(sum(object$residuals^2)/(length(object$residuals)-nrow(fitfit))), digits=6),
        " on ", length(object$residuals)-nrow(fitfit), " degrees of freedom\n")
    cat(" Multiple R-squared:  ", round(object$Rsquared, digits=6),
        " Residual sum of squares: ", round(object$RSS, digits=6))
    cat("\n")
    return(invisible(object))
  }

  if(is.list(object$residuals)==TRUE){

    cat("Call: (",object$type,")\n\n  ")
    print(object$call)
    cat("\n")
    cat("Residuals Series 1:\n ")
    print(summary(object$residuals.i[[1]]))
    cat("\n")
    cat("Residuals Series 2:\n ")
    print(summary(object$residuals.i[[2]]))
    cat("\n")
    assign.stars <- function(object){
      if(object<0.001) return("***")
      else if(object>=0.001 & object<0.01) return(" **")
      else if(object>=0.01 & object<0.05) return("  *")
      else if(object>=0.05 & object<0.1) return("  .")
      else return(" ")
    }
    stars <- sapply(object$Estimate$`p-value`,assign.stars)
    fitfit <- cbind(object$Estimate,stars)
    colnames(fitfit) <- c(colnames(object$Estimate)," ")
    cat("Coefficients:\n ")
    print(fitfit)
    cat("---\n Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1")
    cat("\n\n")
    cat(" Residual standard error Series 1:",
        round(sqrt(sum(object$residuals.i[[1]]^2)/(length(object$residuals.i[[1]])-nrow(fitfit))), digits=6),
        " on ", length(object$residuals.i[[1]])-nrow(fitfit), " degrees of freedom\n")
    cat(" Residual standard error Series 2:",
        round(sqrt(sum(object$residuals.i[[2]]^2)/(length(object$residuals.i[[2]])-nrow(fitfit))), digits=6),
        " on ", length(object$residuals.i[[2]])-nrow(fitfit), " degrees of freedom\n")
    cat(" Multiple R-squared:  ",round(object$Rsquared, digits=6),
        " Residual sum of squares: ", round(object$RSS, digits=6))
    cat("\n")
    return(invisible(object))

  }
}

