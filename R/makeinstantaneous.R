make.instantaneous <- function(cumulate.data){
  firstdiff <- function(x) {
    shifted <- c(0,x[1:(length(x)-1)])
    x-shifted
  }
  return(firstdiff(cumulate.data))
}
