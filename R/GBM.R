
GBM =  function (series,
                 shock = c("exp", "rett", "mixed"),
                 nshock,
                 prelimestimates,
                 alpha = 0.05,
                 oos = round(length(series)*0.25),
                 display = T)
{

  cl <- match.call()
  bass <- c( BM(series, display = F)$Estimate[1, 1],
             BM(series, display = F)$Estimate[2, 1],
             BM(series, display = F)$Estimate[3, 1])

  nameinter <- c("Exponential", "Rectangular", "Mixed")
  namemean <- c("exp", "rett", "mixed")
  a <-
    b <-
    c <-
    a1 <- b1 <- c1 <- a2 <- b2 <- c2 <- a3 <- b3 <- c3 <- x <- NULL
  t <- seq(1, length(series), by = 1)
  s <- series
  c <- cumsum(series)
  if (nshock == 1) {
    if (shock == "exp" | shock == "rett") {

      if (shock == "exp") {
        intx <- function(t, a1, b1, c1) {
          (t + c1 * (1 / b1) * (exp(b1 * (t - a1)) - 1) *
             (t >= a1))
        }
        #cat("################## Exponential shock ################## \n")
        xt <- function(t, a1, b1, c1) {
          1 + (c1 * exp(b1 * (t - a1))) * (t >= a1)
        }
      }
      else if (shock == "rett") {
        intx <- function(t, a1, b1, c1) {
          (t + c1 * (t - a1) * (a1 <= t) * (t <= b1) +
             c1 * (b1 - a1) * (b1 < t))
        }
        #cat("################## Rectangular shock  ################## \n")
        xt <- function(t, a1, b1, c1) {
          1 + c1 * (t >= a1) * (t <= b1)
        }
      }
    }
    else if (shock == "mixed") {
      cat("--- Sorry you need 2 shocks in this case ---")}

    ff <- function(t, m, p, q, a1, b1, c1) {
      m * (1 - exp(-(p + q) * intx(t, a1, b1, c1))) / (1 +
                                                         (q / p) * exp(-(p + q) * intx(t, a1, b1, c1)))
    }
    ff1 <- function(t, par) {
      c - ff(t, par[1], par[2], par[3], par[4], par[5],
             par[6])
    }
    ff2 <- function(t, par) {
      ff(t, par[1], par[2], par[3], par[4], par[5], par[6])
    }
    zprime <- function(t, m, p, q, a1, b1, c1) {
      m * (p + q * (ff(t, m, p, q, a1, b1, c1) / m)) * (1 - (ff(t, m, p, q, a1, b1, c1) /
                                                               m)) * xt(t, a1, b1, c1)
    }
    zprime.return <- function(t, par) {
      m <- par[1]
      p <- par[2]
      q <- par[3]
      a1 <- par[4]
      b1 <- par[5]
      c1 <- par[6]

      m * (1 - exp(-(p + q) * intx(t, a1, b1, c1))) / (1 +
                                                         (q / p) * exp(-(p + q) * intx(t, a1, b1, c1)))

    }



    estimates <-
      nls.lm(
        par = prelimestimates,
        fn = ff1,
        t = t,
        control = nls.lm.control(maxiter = 1024)
      )$par
    aa <- data.frame(summary(
      nls.lm(
        par = prelimestimates,
        fn = ff1,
        t = t,
        control = nls.lm.control(maxiter = 1024)
      )
    )$coefficients[, c(1, 2)], 0, 0,
    0)
    names(aa) <- c("Estimate", "Std.Error", "Lower", "Upper",
                   "p-value")
    row.names(aa) <- c("m  ", "p  ", "q  ", "a1  ", "b1  ",
                       "c1  ")
    for (i in 1:NROW(aa)) {
      aa[i, c(3, 4)] <- aa[i, 1] + c(-1, 1) * qnorm(1 -
                                                      alpha / 2) * aa[i, 2]
    }
    sssss <- signif(summary(
      nls.lm(
        par = prelimestimates,
        fn = ff1,
        t = t,
        control = nls.lm.control(maxiter = 1024)
      )
    )$coefficients, digits = 3)
    aa[, 5] <- sssss[, 4]
    resid <-
      nls.lm(
        par = prelimestimates,
        fn = ff1,
        t = t,
        control = nls.lm.control(maxiter = 1024)
      )$fvec

    if (display == T) {
      par(mfrow = c(1, 2))
      plot(
        t,
        c,
        main = "Cumulative",
        xlim = c(0,
                 max(t) + oos),
        ylim = c(0,optimize(ff2, interval=c(0, max(t) + oos),
                            maximum=TRUE, par=estimates)$objective*1.2),
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
        ff2(x, estimates),
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
           ylim = c(0,max(max(series),
                          max(make.instantaneous(ff2(1: (max(t)+oos), estimates))))*1.2),

           #ylim = c(0, max(series) * 150 / 100),
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

      curve(grad(function(t) ff2(t, estimates), x, method = "simple"),
            add=T,
            lty=1,
            lwd=2,
            col = 2,
            xlim = c(0, max(t) +
                       oos))


      par(mfrow = c(1, 1))

    }



    s.hat <- ff2(t, estimates)
    tss <- sum((c - mean(c)) ^ 2)
    rss <- sum((c - s.hat) ^ 2)
    r.squared <- 1 - rss / tss

    tipo <-
      paste("Generalized Bass model with 1 ", nameinter[which(namemean == shock)], " shock")


    coefi <- aa$Estimate
    names(coefi) <- rownames(aa)
    ao <-
      list(
        model = zprime.return,
        type = tipo,
        Estimate = aa,
        coefficients = coefi,
        Rsquared = r.squared,
        RSS = rss,
        residuals = resid,
        fitted = s.hat,
        data = cumsum(series),
        call = cl
      )
    class(ao) <- "Dimora"
    invisible(ao)
  }
  else if (nshock == 2) {
    if (shock == "exp") {
      intx1 <- function(t, a1, b1, c1, a2, b2, c2) {
        t + c1 * (1 / b1) * (exp(b1 * (t - a1)) - 1) *
          (t >= a1) + c2 * (1 / b2) * (exp(b2 * (t - a2)) -
                                         1) * (t >= a2)
      }
      ##cat("################## Exponential shock ################## \n")
      xt1 <- function(t, a1, b1, c1, a2, b2, c2) {
        1 + (c1 * exp(b1 * (t - a1))) * (t >= a1) + (c2 *
                                                       exp(b2 * (t - a2))) * (t >= a2)
      }
    }
    else if (shock == "rett") {
      intx1 <- function(t, a1, b1, c1, a2, b2, c2) {
        t + c1 * (t - a1) * (a1 <= t) * (t <= b1) + c1 *
          (b1 - a1) * (b1 < t) + c2 * (t - a2) * (a2 <=
                                                    t) * (t <= b2) + c2 * (b2 - a2) * (b2 < t)
      }
      #cat("################## Rectangular shock  ################## \n")
      xt1 <- function(t, a1, b1, c1, a2, b2, c2) {
        1 + c1 * (t >= a1) * (t <= b1) + c2 * (t >= a2) *
          (t <= b2)
      }
    }
    else if (shock == "mixed") {
      intx1 <- function(t, a1, b1, c1, a2, b2, c2) {
        t + (c1 / b1) * (exp(b1 * (t - a1)) - 1) * (a1 <=
                                                      t) + c2 * (t - a2) * (a2 <= t) * (t <= b2) +
          c2 * (b2 - a2) * (b2 < t)
      }
      #cat("################## Mixed shock ################## \n")
      xt1 <- function(t, a1, b1, c1, a2, b2, c2) {
        1 + (c1 * exp(b1 * (t - a1))) * (t >= a1) + c2 *
          (t >= a2) * (t <= b2)
      }
    }


    ff0 <- function(t, m, p, q, a1, b1, c1, a2, b2, c2) {
      m * (1 - exp(-(p + q) * intx1(t, a1, b1, c1, a2,
                                    b2, c2))) / (1 + (q / p) * exp(-(p + q) * intx1(t,
                                                                                    a1, b1, c1, a2, b2, c2)))
    }
    ff1 <- function(t, par) {
      c - ff0(t, par[1], par[2], par[3], par[4], par[5],
              par[6], par[7], par[8], par[9])
    }
    ff2 <- function(t, par) {
      ff0(t, par[1], par[2], par[3], par[4], par[5], par[6],
          par[7], par[8], par[9])
    }
    zprime1 <- function(t, m, p, q, a1, b1, c1, a2, b2, c2) {
      m * (p + q * (ff0(t, m, p, q, a1, b1, c1, a2, b2,
                        c2) / m)) * (1 - (ff0(t, m, p, q, a1, b1, c1, a2,
                                              b2, c2) / m)) * xt1(t, a1, b1, c1, a2, b2, c2)
    }
    zprime1.return <- function(t, par) {
      m <- par[1]
      p <- par[2]
      q <- par[3]
      a1 <- par[4]
      b1 <- par[5]
      c1 <- par[6]
      a2 <- par[7]
      b2 <- par[8]
      c2 <- par[9]

      m * (1 - exp(-(p + q) * intx1(t, a1, b1, c1, a2, b2, c2))) /
        (1 + (q / p) * exp(-(p + q) * intx1(t,a1, b1, c1, a2, b2, c2)))

    }



    estimates <-
      nls.lm(
        par = prelimestimates,
        fn = ff1,
        t = t,
        control = nls.lm.control(maxiter = 1024)
      )$par

    aa <- data.frame(summary(

      nls.lm(
        par = prelimestimates,
        fn = ff1,
        t = t,
        control = nls.lm.control(maxiter = 1024)
      )
    )$coefficients[, c(1, 2)], 0, 0,
    0)

    names(aa) <- c("Estimate", "Std.Error", "Lower", "Upper",
                   "p-value")
    row.names(aa) <- c("m  ", "p  ", "q  ", "a1  ", "b1  ",
                       "c1  ", "a2  ", "b2  ", "c2  ")
    for (i in 1:NROW(aa)) {
      aa[i, c(3, 4)] <- aa[i, 1] + c(-1, 1) * qnorm(1 -
                                                      alpha / 2) * aa[i, 2]
    }
    sssss <- signif(summary(
      nls.lm(
        par = prelimestimates,
        fn = ff1,
        t = t,
        control = nls.lm.control(maxiter = 1024)
      )
    )$coefficients, digits = 3)

    aa[, 5] <- sssss[, 4]
    resid <-
      nls.lm(
        par = prelimestimates,
        fn = ff1,
        t = t,
        control = nls.lm.control(maxiter = 1024)
      )$fvec

    if (display == T) {
      par(mfrow = c(1, 2))
      plot(
        t,
        c,
        main = "Cumulative",
        xlim = c(0,
                 max(t) + oos),
        ylim = c(0,optimize(ff2, interval=c(0, max(t) + oos),
                            maximum=TRUE, par=estimates)$objective*1.2),
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
        ff2(x, estimates),
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
           ylim = c(0,max(max(series),
                          max(make.instantaneous(ff2(1:(max(t)+oos), estimates))))*1.2),
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

      curve(grad(function(t) ff2(t, estimates), x, method = "simple"),
            add=T,
            lty=1,
            lwd=2,
            col = 2,
            xlim = c(0, max(t) +
                       oos))


      par(mfrow = c(1, 1))

    }



    s.hat <- ff2(t, estimates)
    tss <- sum((c - mean(c)) ^ 2)
    rss <- sum((c - s.hat) ^ 2)
    r.squared <- 1 - rss / tss

    tipo <-
      paste("Generalized Bass model with 2 ", nameinter[which(namemean == shock)], " shock")
    coefi <- aa$Estimate
    names(coefi) <- rownames(aa)
    ao <-
      list(
        model = zprime1.return,
        type = tipo,
        Estimate = aa,
        coefficients = coefi,
        Rsquared = r.squared,
        RSS = rss,
        residuals = resid,
        fitted = s.hat,
        data = cumsum(series),
        call = cl
      )
    class(ao) <- "Dimora"
    invisible(ao)
  }
  else if (nshock == 3) {
    if (shock == "exp" | shock == "rett") {
      if (shock == "exp") {
        intx2 <- function(t, a1, b1, c1, a2, b2, c2,
                          a3, b3, c3) {
          t + c1 * (1 / b1) * (exp(b1 * (t - a1)) - 1) *
            (t >= a1) + c2 * (1 / b2) * (exp(b2 * (t -
                                                     a2)) - 1) * (t >= a2) + c3 * (1 /
                                                                                     b3) * (exp(b3 *
                                                                                                  (t - a3)) - 1) * (t >= a3)
        }
        #cat("################## Esponential shock ################## \n")
        xt2 <- function(t, a1, b1, c1, a2, b2, c2, a3,
                        b3, c3) {
          1 + (c1 * exp(b1 * (t - a1))) * (t >= a1) +
            (c2 * exp(b2 * (t - a2))) * (t >= a2) + (c3 *
                                                       exp(b3 * (t - a3))) * (t >= a3)
        }
      }
      else if (shock == "rett") {
        intx2 <- function(t, a1, b1, c1, a2, b2, c2,
                          a3, b3, c3) {
          (
            t + c1 * (t - a1) * (a1 <= t) * (t <= b1) +
              c1 * (b1 - a1) * (b1 < t) + c2 * (t - a2) *
              (a2 <= t) * (t <= b2) + c2 * (b2 - a2) *
              (b2 < t) + c3 * (t - a3) * (a3 <= t) * (t <=
                                                        b3) + c3 * (b3 - a3) * (b3 < t)
          )
        }
        #cat("################## Rectangular shock  ################## \n")
        xt2 <- function(t, a1, b1, c1, a2, b2, c2, a3,
                        b3, c3) {
          1 + c1 * (t >= a1) * (t <= b1) + c2 * (t >=
                                                   a2) * (t <= b2) + c3 * (t >= a3) * (t <=
                                                                                         b3)
        }
      }

      ff00 <- function(t,
                       m,
                       p,
                       q,
                       a1,
                       b1,
                       c1,
                       a2,
                       b2,
                       c2,
                       a3,
                       b3,
                       c3) {
        m * (1 - exp(-(p + q) * intx2(t, a1, b1, c1,a2, b2, c2, a3, b3, c3))) /
          (1 + (q /p) * exp(-(p + q) * intx2(t, a1, b1, c1, a2, b2, c2, a3, b3, c3)))
      }
      ff1 <- function(t, par) {
        c - ff00(t,
                 par[1],
                 par[2],
                 par[3],
                 par[4],
                 par[5],
                 par[6],
                 par[7],
                 par[8],
                 par[9],
                 par[10],
                 par[11],
                 par[12])
      }
      ff2 <- function(t, par) {
        ff00(t,
             par[1],
             par[2],
             par[3],
             par[4],
             par[5],
             par[6],
             par[7],
             par[8],
             par[9],
             par[10],
             par[11],
             par[12])
      }
      zprime2 <- function(t,
                          m,
                          p,
                          q,
                          a1,
                          b1,
                          c1,
                          a2,
                          b2,
                          c2,
                          a3,
                          b3,
                          c3) {
        m * (p + q * (ff00(
          t, m, p, q, a1, b1, c1, a2,
          b2, c2, a3, b3, c3
        ) / m)) * (1 - (ff00(
          t, m,
          p, q, a1, b1, c1, a2, b2, c2, a3, b3, c3
        ) / m)) *
          xt2(t, a1, b1, c1, a2, b2, c2, a3, b3, c3)
      }
      zprime2.return <- function(t, par) {
        m  <- par[1]
        p  <- par[2]
        q <- par[3]
        a1 <- par[4]
        b1 <- par[5]
        c1 <- par[6]
        a2 <- par[7]
        b2 <- par[8]
        c2 <- par[9]
        a3 <- par[10]
        b3 <- par[11]
        c3 <- par[12]

        m * (1 - exp(-(p + q) * intx2(t, a1, b1, c1, a2, b2, c2, a3, b3, c3))) /
          (1 + (q /p) * exp(-(p + q) * intx2(t, a1, b1, c1, a2, b2, c2, a3, b3,c3)))

      }



      estimates <- nls.lm(
        par = prelimestimates,
        fn = ff1,
        t = t,
        control = nls.lm.control(maxiter = 1024)
      )$par
      aa <- data.frame(summary(
        nls.lm(
          par = prelimestimates,
          fn = ff1,
          t = t,
          control = nls.lm.control(maxiter = 1024)
        )
      )$coefficients[, c(1, 2)], 0,
      0, 0)
      names(aa) <- c("Estimate", "Std.Error", "Lower",
                     "Upper", "p-value")
      row.names(aa) <- c("m  ",
                         "p  ",
                         "q  ",
                         "a1  ",
                         "b1  ",
                         "c1  ",
                         "a2  ",
                         "b2  ",
                         "c2  ",
                         "a3  ",
                         "b3  ",
                         "c3  ")
      for (i in 1:NROW(aa)) {
        aa[i, c(3, 4)] <- aa[i, 1] + c(-1, 1) * qnorm(1 -
                                                        alpha / 2) * aa[i, 2]
      }
      sssss <- signif(summary(
        nls.lm(
          par = prelimestimates,
          fn = ff1,
          t = t,
          control = nls.lm.control(maxiter = 1024)
        )
      )$coefficients, digits = 3)
      aa[, 5] <- sssss[, 4]
      resid <- nls.lm(
        par = prelimestimates,
        fn = ff1,
        t = t,
        control = nls.lm.control(maxiter = 1024)
      )$fvec

      if (display == T) {
        par(mfrow = c(1, 2))
        plot(
          t,
          c,
          main = "Cumulative",
          xlim = c(0,
                   max(t) + oos),
          ylim = c(0,optimize(ff2, interval=c(0, max(t) + oos),
                              maximum=TRUE, par=estimates)$objective*1.2),
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
          ff2(x, estimates),
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
             ylim = c(0,max(max(series),
                            max(make.instantaneous(ff2(1: (max(t)+oos), estimates))))*1.2),
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

        curve(grad(function(t) ff2(t, estimates), x, method = "simple"),
              add=T,
              lty=1,
              lwd=2,
              col = 2,
              xlim = c(0, max(t) +
                         oos))


        par(mfrow = c(1, 1))

      }


      s.hat <- ff2(t, estimates)
      tss <- sum((c - mean(c)) ^ 2)
      rss <- sum((c - s.hat) ^ 2)
      r.squared <- (tss - rss) / tss
      tipo <-
        paste("Generalized Bass model with 3 ", nameinter[which(namemean == shock)], " shock")
      coefi <- aa$Estimate
      names(coefi) <- rownames(aa)
      ao <-
        list(
          model = zprime2.return,
          type = tipo,
          Estimate = aa,
          coefficients = coefi,
          Rsquared = r.squared,
          RSS = rss,
          residuals = resid,
          fitted = s.hat,
          data = cumsum(series),
          call = cl
        )
      class(ao) <- "Dimora"
      invisible(ao)
    }
    else if (shock == "mixed") {
      cat("Sorry but we have not implemented a model with 3 mixed shocks yet.")
    }
  }
  else {
    cat("Sorry but we have not implemented a model with more than 3 shocks yet.")
  }
}
