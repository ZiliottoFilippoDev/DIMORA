UCRCD = function( series1,
                     series2,
                     display = T,
                     alpha = 0.05,
                     delta = 0.01,
                     gamma = 0.01,
                     par = "double",
                     m1  = BM(series1, display = F)$Estimate[1, 1],
                     m2  = BM(series2, display = F)$Estimate[1, 1],
                     p1c = BM(series1, display = F)$Estimate[2, 1],
                     q1c = BM(series1, display = F)$Estimate[3, 1],
                     p2  = BM(series2, display = F)$Estimate[2, 1],
                     q2  = BM(series2, display = F)$Estimate[3, 1])
{
  c2i <- length(series1) - length(series2)
  c2 = c2i + 1
  
  tot <- c(series1, series2)
  data1 <- cumsum(series1)
  data2 <- cumsum(series2)
  end <- length(series1)
  
  if (c2i > 0) {
    step <- c2i
    s1 <- series1[1:step]
    series1 <- series1[(c2):end]
    t <- c(1:step)
    s2 <- c(rep(0, step))
    Z1 <- cumsum(s1)
    Z2 <- cumsum(s2)
    
    BMs1 <- BM(s1, display = F)
    model1 <- BMs1$model
    m1 <- BMs1$Estimate[1, 1]
    p1a <- BMs1$Estimate[2, 1]
    q1a <- BMs1$Estimate[3, 1]
    
    m1_ <- BMs1$Estimate[1, ]
    p1a_ <- BMs1$Estimate[2, ]
    q1a_ <- BMs1$Estimate[3, ]
    
    pred_BM1 <- make.instantaneous(fitted(BMs1))
    
    o_bass <- matrix()
    o_bass <- cbind(t, s1, s2, Z1, Z2)
    p_bass <- matrix()
    p_bass <- cbind(t, pred_BM1, s2)
    o_bass <- as.data.frame(o_bass)
    p_bass <- as.data.frame(p_bass)
    colnames(o_bass) <- c("t", "s1", "s2", "Z1", "Z2")
    colnames(p_bass) <- c("t", "pred_1", "pred_2")
  }
  
  
  #######################################################
  
  if (par == "unique") {
    parms <- list(
      mc = (m1 + m2) * 2,
      p1c = p1c,
      p2 = p2,
      q1c = q1c,
      q2 = q2,
      delta = delta
    )
    
    t <- seq(c2, end, by = 1)
    
    Z1 <- cumsum(series1)
    Z2 <- cumsum(series2)
    data <- matrix()
    data <- cbind(t, series1, series2, Z1, Z2)
    data <- as.data.frame(data)
    colnames(data) <- c("t", "s1", "s2", "Z1", "Z2")
    expdf = melt(
      data[1:3],
      id.var = "t",
      variable.name = "product",
      value.name = "response"
    )
    
    model <- function(t, parms) {
      Z = Z1 + Z2
      mc = parms$mc
      p1c = parms$p1c
      p2 = parms$p2
      q1c = parms$q1c
      q2 = parms$q2
      delta = parms$delta
      z1 = mc * (p1c + (q1c + delta) * Z1 / mc + q1c * Z2 / mc) * (1 - Z /
                                                                     mc)
      z2 = mc * (p2 + (q2 - delta) * Z1 / mc + q2 * Z2 / mc) * (1 - Z /
                                                                     mc)
      return(list(z1 = z1, z2 = z2, t = t))
    }
    
    res.model <- function(parms) {
      estimates = model(t = t, parms)
      data <- data.frame(t = estimates$t,
                         z1.s = estimates$z1,
                         z2.s = estimates$z2)
      preddf = melt(
        data,
        id.var = "t",
        variable.name = "product",
        value.name = "response"
      )
      residual <- preddf$response - expdf$response
      return (residual)
    }
    
    fitval1 = nls.lm(
      par = parms,
      fn = res.model,
      control = nls.lm.control(maxiter = 1000, maxfev = 10000)
    )
    summary <- summary(fitval1)
    sssss <- signif(summary$coefficients, digits = 3)
    aa <- data.frame(summary$coefficients[, c(1, 2)], 0, 0, 0)
    for (i in 1:NROW(aa)) {
      aa[i, c(3, 4)] <- aa[i, 1] + c(-1, 1) * qnorm(1 -
                                                      alpha / 2) * aa[i, 2]
    }
    aa[, 5] <- sssss[, 4]
    names(aa) <- c("Estimate", "Std.Error", "Lower", "Upper",
                   "p-value")
    
    parest = as.list(coef(fitval1))
    mc = parest$mc
    p1c = parest$p1c
    q1c = parest$q1c
    p2 = parest$p2
    q2 = parest$q2
    delta = parest$delta
    
    if (c2i > 0) {
      Estimate <- c(m1, p1a, q1a, mc, p1c, q1c + delta, q1c, p2, q2, q2 - delta)
      names(Estimate) <-
        c("ma",
          "p1a",
          "q1a",
          "mc",
          "p1c",
          "q1c+delta",
          "q1c",
          "p2",
          "q2",
          "q2-delta")
      
      Estimate1 <- rbind(m1_, p1a_, q1a_, aa)
      rownames(Estimate1) <- c("ma",
                               "p1a",
                               "q1a",
                               "mc",
                               "p1c",
                               "p2",
                               "q1c",
                               "q2",
                               "delta")
      estimates = model(t = t, parest)
      z_prime <-
        data.frame(t = estimates$t,
                   pred_1 = estimates$z1,
                   pred_2 = estimates$z2)
      
      
      data$t <- c(c2:end)
      data_o <- matrix()
      data_o <- rbind(o_bass, data)
      data <- data_o
      z_prime$t <- c(c2:end)
      data_p <- matrix()
      data_p <- rbind(p_bass, z_prime)
      z_prime <- data_p
    }
    
    if (c2i == 0) {
      Estimate <- c(mc, p1c, q1c + delta, q1c, p2, q2, q2 - delta)
      names(Estimate) <- c("mc", "p1c", "q1c+delta", "q1c", "p2",
                           "q2", "q2-delta")
      Estimate1 <- aa
      rownames(Estimate1) <- c("ma",
                               "p1a",
                               "q1a",
                               "mc",
                               "p1c",
                               "p2",
                               "q1c",
                               "q2",
                               "delta")
      estimates = model(t = t, parest)
      z_prime <-
        data.frame(t = estimates$t,
                   pred_1 = estimates$z1,
                   pred_2 = estimates$z2)
      
      data$t <- c(c2:end)
      z_prime$t <- c(c2:end)
    }
    
    obs = melt(
      data[, c(1:3)],
      id.var = c("t"),
      variable.name = "product",
      value.name = "consumption"
    )
    pred = melt(
      z_prime,
      id.var = c("t"),
      variable.name = "product",
      value.name = "consumption"
    )
    res = obs$consumption - pred$consumption
  }
  
  ##################################################################################
  
  if (par == "double") {
    parms <- list(
      mc = (m1 + m2) * 2,
      p1c = p1c,
      p2 = p2,
      q1c = q1c,
      q2 = q2,
      delta = delta,
      gamma = gamma
    )
    
    t <- seq(c2, end, by = 1)
    
    Z1 <- cumsum(series1)
    Z2 <- cumsum(series2)
    data <- matrix()
    data <- cbind(t, series1, series2, Z1, Z2)
    data <- as.data.frame(data)
    colnames(data) <- c("t", "s1", "s2", "Z1", "Z2")
    expdf = melt(
      data[1:3],
      id.var = "t",
      variable.name = "product",
      value.name = "response"
    )
    
    model <- function(t, parms) {
      Z = Z1 + Z2
      mc = parms$mc
      p1c = parms$p1c
      p2 = parms$p2
      q1c = parms$q1c
      q2 = parms$q2
      delta = parms$delta
      gamma = parms$gamma
      z1 = mc * (p1c + (q1c + delta) * Z1 / mc + q1c * Z2 / mc) * (1 - Z /
                                                                     mc)
      z2 = mc * (p2 + (q2 - gamma) * Z1 / mc + q2 * Z2 / mc) * (1 - Z /
                                                                     mc)
      return(list(z1 = z1, z2 = z2, t = t))
    }
    
    res.model <- function(parms) {
      estimates = model(t = t, parms)
      data <- data.frame(t = estimates$t,
                         z1.s = estimates$z1,
                         z2.s = estimates$z2)
      preddf = melt(
        data,
        id.var = "t",
        variable.name = "product",
        value.name = "response"
      )
      residual <- preddf$response - expdf$response
      return (residual)
    }
    
    fitval1 = nls.lm(
      par = parms,
      fn = res.model,
      control = nls.lm.control(maxiter = 1000, maxfev = 10000)
    )
    summary <- summary(fitval1)
    sssss <- signif(summary$coefficients, digits = 3)
    aa <- data.frame(summary$coefficients[, c(1, 2)], 0, 0, 0)
    for (i in 1:NROW(aa)) {
      aa[i, c(3, 4)] <- aa[i, 1] + c(-1, 1) * qnorm(1 -
                                                      alpha / 2) * aa[i, 2]
    }
    aa[, 5] <- sssss[, 4]
    names(aa) <- c("Estimate", "Std.Error", "Lower", "Upper",
                   "p-value")
    
    parest = as.list(coef(fitval1))
    mc = parest$mc
    p1c = parest$p1c
    q1c = parest$q1c
    p2 = parest$p2
    q2 = parest$q2
    delta = parest$delta
    gamma = parest$gamma
    
    if (c2i > 0) {
      Estimate <- c(m1, p1a, q1a, mc, p1c, q1c + delta, q1c, p2, q2, q2 - gamma)
      names(Estimate) <-
        c("ma",
          "p1a",
          "q1a",
          "mc",
          "p1c",
          "q1c+delta",
          "q1c",
          "p2",
          "q2",
          "q2-gamma")
      Estimate1 <- rbind(m1_, p1a_, q1a_, aa)
      rownames(Estimate1) <- c("ma",
                               "p1a",
                               "q1a",
                               "mc",
                               "p1c",
                               "p2",
                               "q1c",
                               "q2",
                               "delta",
                               "gamma")
      estimates = model(t = t, parest)
      z_prime <-
        data.frame(t = estimates$t,
                   pred_1 = estimates$z1,
                   pred_2 = estimates$z2)
      
      data$t <- c(c2:end)
      data_o <- matrix()
      data_o <- rbind(o_bass, data)
      data <- data_o
      z_prime$t <- c(c2:end)
      data_p <- matrix()
      data_p <- rbind(p_bass, z_prime)
      z_prime <- data_p
    }
    
    if (c2i == 0) {
      Estimate <- c(mc, p1c, q1c + delta, q1c, p2, q2, q2 - gamma)
      names(Estimate) <- c("mc", "p1c", "q1c+delta", "q1c", "p2",
                           "q2", "q2-gamma")
      Estimate1 <- aa
      rownames(Estimate1) <- c("ma",
                               "p1a",
                               "q1a",
                               "mc",
                               "p1c",
                               "p2",
                               "q1c",
                               "q2",
                               "delta",
                               "gamma")
      estimates = model(t = t, parest)
      z_prime <-
        data.frame(t = estimates$t,
                   pred_1 = estimates$z1,
                   pred_2 = estimates$z2)
      
      data$t <- c(c2:end)
      z_prime$t <- c(c2:end)
    }
    
    obs = melt(
      data[, c(1:3)],
      id.var = c("t"),
      variable.name = "product",
      value.name = "consumption"
    )
    pred = melt(
      z_prime,
      id.var = c("t"),
      variable.name = "product",
      value.name = "consumption"
    )
    res = obs$consumption - pred$consumption
  }
  
  ################################################
  
  if (c2i > 0) {
    data <-
      list(obs$consumption[1:end], obs$consumption[(end + c2):(2 * end)])
    fitted <-
      list(pred$consumption[1:end], pred$consumption[(end + c2):(2 * end)])
    residu <- list(obs$consumption[1:end] - pred$consumption[1:end],
                   obs$consumption[(end + c2):(2 * end)] - pred$consumption[(end +
                                                                               c2):(2 * end)])
  }
  
  if (c2i == 0) {
    data <-
      list(obs$consumption[1:end], obs$consumption[(end + c2):(2 * end)])
    fitted <-
      list(pred$consumption[1:end], pred$consumption[(end + c2):(2 * end)])
    residu <- list(obs$consumption[1:end] - pred$consumption[1:end],
                   obs$consumption[(end + c2):(2 * end)] - pred$consumption[(end +
                                                                               c2):(2 * end)])
    
  }
  
  tss <- sum((obs$consumption - mean(obs$consumption)) ^ 2)
  rss <- sum((obs$consumption - pred$consumption) ^ 2)
  r.squared <- 1 - rss / tss
  
  ss1 <- obs$consumption[1:end]
  ss2 <- obs$consumption[(end + c2):(2 * end)]
  cc1 <- cumsum(ss1)
  cc2 <- cumsum(ss2)
  
  pp1 <- pred$consumption[1:end]
  pp2 <- pred$consumption[(end + c2):(2 * end)]
  gg1 <- cumsum(pp1)
  gg2 <- cumsum(pp2)
  
  t <- c(1:end)
  t2 <- c(c2:end)
  
  
  if (display == T) {
    par(mfrow = c(1, 2))
    plot(
      t,
      cumsum(ss1),
      main = "Cumulative",
      
      ylim = c(0, sum(ss1) + sum(ss1) * 50 / 100),
      ylab = "z(t)",
      type = 'b',
      pch = 19,
      cex = 0.8
    )
    
    legend(
      "topright",
      legend = c("Observed 1", "Observed 2",
                 "Predicted 1", "Predicted 2"),
      pch = c(19, 19, NA, NA),
      lty = c(NA, NA, 1, 1),
      lwd = 2,
      col = c(1, 3, 2, "#5C2E91"),
      cex = 0.8
    )
    
    lines(t2, cumsum(pp2), col = "#5C2E91", lwd = 2)
    lines(t, cumsum(pp1), col = 2, lwd = 2)
    lines(
      t2,
      cumsum(ss2),
      pch = 19,
      type = "b",
      col = 3,
      cex = 0.8
    )
    
    M <- max(ss1, ss2, pp1, pp2)
    MM <- M * 1.30
    plot(
      t,
      ss1,
      main = "Instantaneous",
      pch = 19,
      type = "b",
      cex = 0.8 ,
      ylab = "z'(t)",
      ylim = c(0, MM)
    )
    lines(t2, pp2, col = "#5C2E91", lwd = 2)
    lines(t, pp1, col = 2, lwd = 2)
    lines(
      t2,
      ss2,
      pch = 19,
      type = "b",
      col = 3 ,
      cex = 0.8
    )
    
    legend(
      "topright",
      legend = c("Observed 1", "Observed 2",
                 "Predicted 1", "Predicted 2"),
      pch = c(19, 19, NA, NA),
      lty = c(NA, NA, 1, 1),
      lwd = 2,
      col = c(1, 3, 2, "#5C2E91"),
      cex = 0.8
    )
    
    par(mfrow = c(1, 1))
    
  }
  
  FITTED <- list(cumsum(pp1), cumsum(pp2))
  DATA <- list(cumsum(ss1), cumsum(ss2))
  RESIDUALS <- list(cumsum(pp1) - cumsum(ss1), cumsum(pp2) - cumsum(ss2))
    
    cl <- match.call()
  ao <- list(
    model = model,
    type = "UCRCD Model",
    Estimate = Estimate1,
    coefficients = Estimate,
    Rsquared = r.squared,
    RSS = rss,
    residuals = RESIDUALS,
    residuals.i = residu,
    fitted = FITTED,
    fitted.i = fitted,
    data = DATA,
    data.i = data,
    call = cl
  )
  
  class(ao) <- "Dimora"
  invisible(ao)
}
