BM = function (series,
               method = "nls",
               prelimestimates = c(sum(series) + 100, 0.01, 0.1),
               oos = round(length(series)*0.25),
               alpha = 0.05,
               display = T)
{
  x <- NULL
  t <- seq(1, length(series), by = 1)
  s <- series
  c <- cumsum(s)
  ff <- function(t, m, p, q) {
    m * (1 - exp(-(p + q) * t)) / (1 + q / p * exp(-(p + q) * t))
  }
  ff1 <- function(t, par) {
    c - ff(t, par[1], par[2], par[3])
  }
  ff2 <- function(t, par) {
    ff(t, par[1], par[2], par[3])
  }


  zprime <- function(t, m,p,q) {

    m * (p + q * (ff(t, m, p, q) / m)) * (1 - (ff(t, m, p,
                                                  q) / m))
  }


  zprime.return <- function(t, par) {
    m <- par[1]
    p <- par[2]
    q <- par[3]
    m * (1 - exp(-(p + q) * t)) / (1 + q / p * exp(-(p + q) *
                                                     t))
  }

  if (method == "nls") {
    stime <-
      nls.lm(
        par = prelimestimates,
        fn = ff1,
        t = t,
        control = nls.lm.control(maxiter = 200)
      )$par
    sssss <- signif(summary(
      nls.lm(
        par = prelimestimates,
        fn = ff1,
        t = t,
        control = nls.lm.control(maxiter = 200)
      )
    )$coefficients, digits = 3)
    aa <- data.frame(summary(
      nls.lm(
        par = prelimestimates,
        fn = ff1,
        t = t,
        control = nls.lm.control(maxiter = 200)
      )
    )$coefficients[, c(1, 2)], 0, 0,
    0)
    names(aa) <- c("Estimate", "Std.Error", "Lower",
                   "Upper", "p-value")
    row.names(aa) <- c("m", "p", "q")
    for (i in 1:NROW(aa)) {
      aa[i, c(3, 4)] <- aa[i, 1] + c(-1, 1) * qnorm(1 -
                                                      alpha / 2) * aa[i, 2]
    }
    aa[, 5] <- sssss[, 4]
    res <-
      nls.lm(
        par = prelimestimates,
        fn = ff1,
        t = t,
        control = nls.lm.control(maxiter = 200)
      )$fvec

  }


  else if (method == "optim") {
    mass <- sum(s) + 1000
    ff3 <- function(par) {
      ff(t, par[1], par[2], par[3])
    }
    max <- sum(s) + 10000

    stima_optim <- function(c) {
      f <- function(p) {
        sum((c - ff3(p)) ^ 2)
      }
      optim(
        par = prelimestimates,
        fn = f,
        method = "L-BFGS-B",
        lower = c(1e-10, 1e-10, 1e-10),
        upper = c(mass,
                  1, 1)
      )$par
    }

    stime <- stima_optim(c)
    aa <- stime
    res <- c - ff2(t, aa)

    table <- matrix(data=c(rep("-", 12)), ncol=4)
    aa <- data.frame(aa, table)
    names(aa) <- c("Estimate", "Std.Error", "Lower",
                   "Upper", "p-value")
    row.names(aa) <- c("m", "p", "q")

  }


  if (display == T) {
    par(mfrow = c(1, 2))
    plot(
      t,
      c,
      main = "Cumulative",
      xlim = c(0,
               max(t) + oos),
      ylim = c(0, sum(s) + sum(s) * 50 / 100),
      ylab = "z(t)",type='b',pch=19,cex=0.8
    )

    legend("topright",
           legend = c("Observed",
                      "Predicted"),
           pch = c(19,NA),
           lty = c(NA,1),
           lwd=2,
           col = c(1,2),
           cex=0.8
    )

    curve(
      ff2(x, stime),
      add = T,
      lty=1,
      lwd=2,
      col = 2,
      xlim = c(0, max(t) +
                 oos)
    )

    plot(t, series,
         main = "Instantaneous",
         xlim = c(0,
                  max(t) + oos),
         ylim = c(0, max(series) * 150 / 100),
         ylab = "z'(t)",
         type='b',
         pch=19,
         cex=0.8)

    legend("topright",
           legend = c("Observed",
                      "Predicted"),
           pch = c(19,NA),
           lty = c(NA,1),
           lwd=2,
           col = c(1,2),
           cex=0.8
    )

    curve(grad(function(t) ff2(t, stime), x, method = "simple"),
          add=T,
          lty=1,
          lwd=2,
          col = 2,
          xlim = c(0, max(t) +
                     oos))

    par(mfrow = c(1, 1))

  }


  s.hat <- ff2(t, stime)
  tss <- sum((c - mean(s)) ^ 2)
  rss <- sum((c - s.hat) ^ 2)
  r.squared <- 1 - rss / tss

  coefi <- aa$Estimate
  cl <- match.call()
  names(coefi) <- rownames(aa)

  ao <- list(
    model = zprime.return,
    type = "Standard Bass Model",
    Estimate = aa,
    coefficients = coefi,
    Rsquared = r.squared,
    RSS = rss,
    residuals = res,
    fitted = s.hat,
    data = cumsum(series),
    call = cl
  )
  class(ao) <- "Dimora"
  invisible(ao)


}
