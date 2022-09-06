predict.Dimora <- function(object,...,newx){
  object$model(newx,object$Estimate$Estimate)
}
