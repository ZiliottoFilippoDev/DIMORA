plot.Dimora <- function(x,...,type=c('all','res','fit'), oos=0){

  #checking which model we have
  product=1
  if (x$type == 'UCRCD Model'){product=2}

  ## 1 PRODUCT ##
  if(product==1){
    if(oos==0) {
      xlim <- NULL
      xmax <- length(x$data)
      maxx <- length(x$data)}
    else {
      xmax <- length(x$data) + oos
      maxx <- max(length(x$data),xmax)
      xlim <- c(0,maxx)}

    data <- make.instantaneous(x$data)
    estimates <- make.instantaneous(predict(x,newx=1:maxx))

    data_cum <- x$data
    estimates_cum <- predict(x,newx=1:maxx)
    ylim_cum=c(0,max(estimates_cum))


    if(missing(type)){type='all'}
    if(type=='fit'){
      par(mfrow=c(1,2))
      limit <- NULL
      plot(data_cum,xlim = xlim,main="Cumulative",ylim=ylim_cum,cex.axis=.9,
           ylab='z(t)',xlab="t",type='b',pch=19,cex=0.8,...)
      lines(estimates_cum,lty=1,lwd=2,col='red')
      legend('topleft', legend=c("Observed", "Predicted"), col=c("black", 'red'), lty=c(0,1),pch=c(19,NA), cex=0.8)
      plot(data,xlim = xlim,main="Instantaneous",ylim=limit,cex.axis=.9,
           ylab="z'(t)",xlab="t",type='b',pch=19,cex=0.8,...)
      lines(estimates,lty=1,lwd=2,col='red')
      legend('topleft', legend=c("Observed", "Predicted"), col=c("black", 'red'), lty=c(0,1),pch=c(19,NA), cex=0.8)
    }

    else if(type=='res'){
      par(mfrow=c(1,2))
      plot(residuals(x)~I(1:length(residuals(x))),main="Residuals vs Time",ylab="Res cumulative",xlab="t",type='h',...)
      points(residuals(x)~I(1:length(residuals(x))),xlim = xlim,main="", ylab="",xlab="",type='p',pch=19,cex=0.8,...)
      abline(h=0,lty=2,col="gray")
      #legend('topright', legend=c("Residuals"), col=c("black"), lty=c(0),pch=c(19), cex=0.6)

      acf_res <- acf(residuals(x),  plot=FALSE)
      plot(acf_res, main='ACF Residuals',type='h',ylab='ACF cumulative')
      points(acf_res$lag,acf_res$acf, type='p',col='black',pch=19,cex=0.8)

    }
    else if(type=='all'){

      #Instantaneous/Cumulative/Residuals/ACF(residuals)
      par(mfrow=c(2,2))
      plot(data_cum,xlim = xlim,main="Cumulative",cex.axis=.9,ylim=ylim_cum,
           ylab='z(t)',xlab="t",type='b',pch=19,cex=0.6,...)
      lines(estimates_cum,lty=1,lwd=2,col='red')
      legend('topleft', legend=c("Observed", "Predicted"), col=c("black", 'red'), lty=c(0,1),pch=c(19,NA), cex=0.6)

      plot(data,xlim = xlim,main="Instantaneous",cex.axis=.9,ylim=NULL,
           ylab="z'(t)",xlab="t",type='b',pch=19,cex=0.6,...)
      lines(estimates,lty=1,lwd=2,col='red')
      legend('topleft', legend=c("Observed", "Predicted"), col=c("black", 'red'), lty=c(0,1),pch=c(19,NA), cex=0.6)

      plot(residuals(x)~I(1:length(residuals(x))),main="Residuals vs Time",ylab="Res cumulative",xlab="t",type='h',...)
      points(residuals(x)~I(1:length(residuals(x))),xlim = xlim,main="", ylab="",xlab="",type='p',pch=19,cex=0.8,...)
      abline(h=0,lty=2,col="gray")


      acf_res <- acf(residuals(x),  plot=FALSE)
      plot(acf_res, main='ACF Residuals',type='h', ylab='ACF cumulative')
      points(acf_res$lag,acf_res$acf, type='p',col='black',pch=19,cex=0.8)
    }

  }

  ## 2 PRODUCT ##
  if(product==2){
    xlim <- NULL
    len1 <- length(x$data[[1]])
    len2 <- length(x$data[[2]])
    zeros <- rep(NA,abs(len2 - len1))

    data1 <- x$data.i[[1]]
    data2 <- x$data.i[[2]]
    estimates1 <- x$fitted.i[[1]]
    estimates2 <- x$fitted.i[[2]]

    data1_cum <- x$data[[1]]
    data2_cum <- x$data[[2]]
    estimates1_cum <- x$fitted[[1]]
    estimates2_cum <- x$fitted[[2]]

    if(missing(type)){type='all'}
    min_ <- min(c(min(data1),min(data2)))
    max_ <- max(c(max(data1),max(data2)))

    if(type=='fit'){
      par(mfrow=c(1,2))
      plot(data1_cum,main="Cumulative",ylab='z(t)',xlab="t",cex.axis=.9,
           type='b',pch=19,cex=0.6,...)
      lines(estimates1_cum,lty=1,lwd=2,col='red')
      lines(c(zeros,estimates2_cum),lty=1,lwd=2,col='#5C2E91')
      points(c(zeros,data2_cum),main="Product 2",cex.axis=.9,col='green',
             type='b',pch=19,cex=0.6,...)
      legend('topleft',legend=c('Observed 1','Observed 2','Predicted 1','Predicted 2'),
             col=c('black','green','red','#5C2E91'),pch=c(19,19,NA,NA),lty=c(NA,NA,1,1), cex=.7)

      plot(data1,main="Instantaneous",ylab="z'(t)",xlab="t",cex.axis=.9,
           type='b',pch=19,cex=0.6,ylim=c(min_,max_),...)
      lines(estimates1,lty=1,lwd=2,col='red')
      lines(c(zeros,estimates2),lty=1,lwd=2,col='#5C2E91')
      points(c(zeros,data2),type='b',pch=19,cex=0.6,col='green',...)

      legend('topleft',legend=c('Observed 1','Observed 2','Predicted 1','Predicted 2'),
             col=c('black','green','red','#5C2E91'),pch=c(19,19,NA,NA),lty=c(NA,NA,1,1), cex=.7)
    }

    else if(type=='res'){
      par(mfrow=c(2,2))
      plot(x$residuals.i[[1]]~I(1:length(x$residuals[[1]])),main="Residuals 1 vs Time",
           ylab="Res instantaneous",xlab="t",type='h',...)
      points(x$residuals.i[[1]]~I(1:length(x$residuals[[1]])),xlim = xlim,main="",
             ylab="",xlab="",type='p',pch=19,cex=0.8,...)
      abline(h=0,lty=2,col="gray")
      plot(x$residuals.i[[2]]~I(1:length(x$residuals[[2]])),main="Residuals 2 vs Time",ylab="Res instantaneous",xlab="t",type='h',...)
      points(x$residuals.i[[2]]~I(1:length(x$residuals[[2]])),xlim = xlim,main="",
             ylab="",xlab="",type='p',pch=19,cex=0.8,...)
      abline(h=0,lty=2,col="gray")

      acf_res1 <- acf(x$residuals.i[[1]],  plot=FALSE)
      plot(acf_res1, main='ACF Residuals 1',type='h', ylab='ACF instantenous')
      points(acf_res1$lag,acf_res1$acf, type='p',col='black',pch=19,cex=0.8)

      acf_res2 <- acf(x$residuals.i[[2]],  plot=FALSE)
      plot(acf_res2, main='ACF Residuals 2',type='h', ylab='ACF instantenous')
      points(acf_res2$lag,acf_res2$acf, type='p',col='black',pch=19,cex=0.8)

    }

    else if(type=='all'){
      par(mfrow=c(3,2))

      plot(data1_cum,main="Cumulative",ylab='z(t)',xlab="t",cex.axis=.9,
           type='b',pch=19,cex=0.6,...)
      lines(estimates1_cum,lty=1,lwd=2,col='red')
      lines(c(zeros,estimates2_cum),lty=1,lwd=2,col='#5C2E91')
      points(c(zeros,data2_cum),ylab='',xlab="",col='green',
             type='b',pch=19,cex=0.6,...)
      legend('topleft',legend=c('Observed 1','Observed 2','Predicted 1','Predicted 2'),
             col=c('black','green','red','#5C2E91'),pch=c(19,19,NA,NA),lty=c(NA,NA,1,1), cex=.7)

      plot(data1,main="Instantaneous",ylab="z'(t)",xlab="t",cex.axis=.9,
           type='b',pch=19,cex=0.6,ylim=c(min_,max_),...)
      lines(estimates1,lty=1,lwd=2,col='red')
      lines(c(zeros,estimates2),lty=1,lwd=2,col='#5C2E91')
      points(c(zeros,data2),ylab='',xlab="",col='green',
             type='b',pch=19,cex=0.6,...)
      legend('topleft',legend=c('Observed 1','Observed 2','Predicted 1','Predicted 2'),
             col=c('black','green','red','#5C2E91'),pch=c(19,19,NA,NA),lty=c(NA,NA,1,1), cex=.7)

      plot(x$residuals.i[[1]]~I(1:length(x$residuals[[1]])),main="Residuals 1 vs Time",
           ylab="Res instantaneous",xlab="t",type='h',...)
      points(x$residuals.i[[1]]~I(1:length(x$residuals[[1]])),xlim = xlim,main="",
             ylab="",xlab="",type='p',pch=19,cex=0.8,...)
      abline(h=0,lty=2,col="gray")

      plot(x$residuals.i[[2]]~I(1:length(x$residuals[[2]])),main="Residuals 2 vs Time",ylab="Res instantaneous",xlab="t",type='h',...)
      points(x$residuals.i[[2]]~I(1:length(x$residuals[[2]])),xlim = xlim,main="",
             ylab="",xlab="",type='p',pch=19,cex=0.8,...)
      abline(h=0,lty=2,col="gray")

      acf_res1 <- acf(x$residuals.i[[1]],  plot=FALSE)
      plot(acf_res1, main='ACF Residuals 1',type='h', ylab='ACF instantenous')
      points(acf_res1$lag,acf_res1$acf, type='p',col='black',pch=19,cex=0.8)

      acf_res2 <- acf(x$residuals.i[[2]],  plot=FALSE)
      plot(acf_res2, main='ACF Residuals 2',type='h', ylab='ACF instantenous')
      points(acf_res2$lag,acf_res2$acf, type='p',col='black',pch=19,cex=0.8)

    }
  }

}


