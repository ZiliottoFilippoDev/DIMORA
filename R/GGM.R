GGM =
  function (series,
            prelimestimates = NULL,
            mt = "base",
            alpha = 0.05,
            oos = round(length(series)*0.25),
            display = T)
  {
    x <- NULL
    t <- seq(1, length(series), by = 1)
    s <- series
    c <- cumsum(series)
    if (is.function(mt) == T) {
      ff <- function(t, K, ps, qs) {
        K * mt(t) * (1 - exp(-(ps + qs) * t)) / (1 + (qs / ps) *
                                                   exp(-(ps + qs) * t))
      }
      ff1 <- function(t, par) {
        c - ff(t, par[1], par[2], par[3])
      }
      ff2 <- function(t, par) {
        ff(t, par[1], par[2], par[3])
      }

      if (is.null(prelimestimates) == TRUE) {
        prelimestimates <- BM(series = s, display = F)$Estimate[,
                                                                           1]
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
      )$coefficients[,
                     c(1, 2)], 0, 0, 0)
      names(aa) <- c("Estimate", "Std.Error", "Lower", "Upper",
                     "p-value")
      row.names(aa) <- c("K ", "ps ", "qs ")
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
      )$coefficients,
      digits = 3)
      aa[, 5] <- sssss[, 4]
    }
    else {
      mt <- function(t, K, pc, qc) {
        K * sqrt((1 - exp(-(pc + qc) * t)) / (1 + (qc / pc) *
                                                exp(-(pc + qc) * t)))
      }
      ff0 <- function(t, K, pc, qc, ps, qs) {
        mt(t, K, pc, qc) * (1 - exp(-(ps + qs) * t)) / (1 +
                                                          (qs / ps) * exp(-(ps + qs) * t))
      }
      ff1 <- function(t, par) {
        c - ff0(t, par[1], par[2], par[3], par[4], par[5])
      }
      ff2 <- function(t, par) {
        ff0(t, par[1], par[2], par[3], par[4], par[5])
      }

      if (is.null(prelimestimates) == TRUE) {
        prelimestimates <-
          c(BM(series = s, display = F)$Estimate[,1],0.001,0.1)
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
      )$coefficients[,
                     c(1, 2)], 0, 0, 0)
      names(aa) <- c("Estimate", "Std.Error", "Lower", "Upper",
                     "p-value")
      row.names(aa) <- c("K  ", "pc  ", "qc  ", "ps  ", "qs  ")
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
      )$coefficients,
      digits = 3)
      aa[, 5] <- sssss[, 4]
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
    res <-
      nls.lm(
        par = prelimestimates,
        fn = ff1,
        t = t,
        control = nls.lm.control(maxiter = 1024)
      )$fvec
    coefi <- aa$Estimate
    names(coefi) <- rownames(aa)

    cl <- match.call()
    ao <-
      list(
        model = ff2,
        type = "Guseo Guidolin Model",
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
