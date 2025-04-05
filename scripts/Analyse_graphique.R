library(readxl)
Excel_trafic_routier <- read_excel("data/raw/Excel - trafic routier.xlsx", col_names = TRUE, col_types = NULL) 

View(Excel_trafic_routier)

#création de la série temporelle
trafic_routier <- ts(Excel_trafic_routier[,2], start=c(2001,01),end=c(2024,09),frequency=12)

# graphique chronologique de la série
plot(trafic_routier,main="Trafic routier mensuel sur routes nationales",xlab="t",ylab="milliards de voitures-kilomètres")

# décomposition de la série temporelle pour isoler sa tendance, saisonnalité et aléatoire
decomp.x=decompose(trafic_routier,type="multiplicative")
decomp.x$figure
plot(decomp.x)

# corrélogrammes
layout(matrix(1:2,1,2))
acf(c(trafic_routier),lag.max= 36, type = c("correlation"),plot = TRUE, col="cyan", main=element_blank())
acf(c(trafic_routier),lag.max= 36, type = c("partial"),plot = TRUE, col="red",main=element_blank())


#transformation de la série pour obtenir ses variations
trafic_routier_diff1=diff(trafic_routier,lag=1,differences=1)

#transformation pour supprimmer les variations saisonnières de la série
trafic_routier_diff1_12=diff(trafic_routier_diff1,lag=12,differences=1)

#ACF et PACF de toutes les séries
layout(matrix(1:6,3,2))
acf(c(trafic_routier),lag.max= 36, type = c("correlation"),plot = TRUE, col="cyan")
acf(c(trafic_routier_diff1),lag.max= 36, type = c("correlation"),plot = TRUE, col="cyan")
acf(c(trafic_routier_diff1_12),lag.max= 36, type = c("correlation"),plot = TRUE, col="cyan")
acf(c(trafic_routier),lag.max= 36, type = c("partial"),plot = TRUE, col="red", main=element_blank())
acf(c(trafic_routier_diff1),lag.max= 36, type = c("partial"),plot = TRUE, col="red", main=element_blank())
acf(c(trafic_routier_diff1_12),lag.max= 36, type = c("partial"),plot = TRUE, col="red", main=element_blank())

#test de racine unitaire Dickey-Fuller Augmenté

  #création de la fonction coeftest
  coeftest <- function(x, vcov. = NULL, df = NULL, ...)
  {
    UseMethod("coeftest")
  }
  
  coeftest.default <- function(x, vcov. = NULL, df = NULL, ..., save = FALSE)
  {
    ## use S4 methods if loaded
    coef0 <- if("stats4" %in% loadedNamespaces()) stats4::coef else coef
    vcov0 <- if("stats4" %in% loadedNamespaces()) stats4::vcov else vcov
    nobs0 <- if("stats4" %in% loadedNamespaces()) stats4::nobs else nobs
    logl0 <- if("stats4" %in% loadedNamespaces()) stats4::logLik else logLik
    
    ## extract coefficients and standard errors
    est <- coef0(x)
    if(is.null(vcov.)) se <- vcov0(x) else {
      if(is.function(vcov.)) se <- vcov.(x, ...)
      else se <- vcov.
    }
    se <- sqrt(diag(se))
    
    ## match using names and compute t/z statistics
    if(!is.null(names(est)) && !is.null(names(se))) {
      if(length(unique(names(est))) == length(names(est)) && length(unique(names(se))) == length(names(se))) {
        anames <- names(est)[names(est) %in% names(se)]
        est <- est[anames]
        se <- se[anames]
      }
    }  
    tval <- as.vector(est)/se
    
    ## apply central limit theorem
    if(is.null(df)) {
      df <- try(df.residual(x), silent = TRUE)
      if(inherits(df, "try-error")) df <- NULL
    }
    if(is.null(df)) df <- 0
    
    if(any(is.finite(df)) && all(df > 0)) {
      pval <- 2 * pt(abs(tval), df = df, lower.tail = FALSE)
      cnames <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
      mthd <- "t"
    } else {
      pval <- 2 * pnorm(abs(tval), lower.tail = FALSE)
      cnames <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
      mthd <- "z"
    }
    rval <- cbind(est, se, tval, pval)
    colnames(rval) <- cnames
    class(rval) <- "coeftest"
    attr(rval, "method") <- paste(mthd, "test of coefficients")
    attr(rval, "df") <- df
    
    ## supplementary information for model summary
    n <- try(nobs0(x), silent = TRUE)
    attr(rval, "nobs") <- if(inherits(n, "try-error")) NULL else n
    ll <- try(logl0(x), silent = TRUE)
    attr(rval, "logLik") <- if(inherits(ll, "try-error")) NULL else ll
    if(save) attr(rval, "object") <- x
    
    return(rval)
  } 
  
  coeftest.glm <- function(x, vcov. = NULL, df = Inf, ...)
    coeftest.default(x, vcov. = vcov., df = df, ...)  
  
  coeftest.mlm <- function(x, vcov. = NULL, df = NULL, ...)
  {
    ## obtain vcov
    v <- if(is.null(vcov.)) vcov(x) else if(is.function(vcov.)) vcov.(x) else vcov.
    
    ## nasty hack: replace coefficients so that their names match the vcov() method
    x$coefficients <- structure(as.vector(x$coefficients), .Names = colnames(vcov(x)))
    
    ## call default method
    coeftest.default(x, vcov. = v, df = df, ...)
  }
  
  coeftest.survreg <- function(x, vcov. = NULL, df = Inf, ...)
  {
    if(is.null(vcov.)) v <- vcov(x) else {
      if(is.function(vcov.)) v <- vcov.(x)
      else v <- vcov.
    }
    if(length(x$coefficients) < NROW(x$var)) {
      x$coefficients <- c(x$coefficients, "Log(scale)" = log(x$scale))
    }
    coeftest.default(x, vcov. = v, df = df, ...)  
  } 
  
  coeftest.breakpointsfull <- function(x, vcov. = NULL, df = NULL, ..., save = FALSE)
  {
    est <- coef(x, ...)
    if(is.null(df)) {
      df <- df.residual(x, ...)
      df <- as.vector(rep(df, rep(NCOL(est), length(df))))
    }  
    
    rnames <- as.vector(t(outer(rownames(est), colnames(est), paste)))
    est <- as.vector(t(est))
    
    se <- vcov(x, vcov. = vcov., ...)
    
    se <- as.vector(sapply(seq_along(se), function(x) sqrt(diag(se[[x]]))))
    tval <- est/se
    
    if(any(is.finite(df)) && all(df > 0)) {
      pval <- 2 * pt(abs(tval), df = df, lower.tail = FALSE)
      cnames <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
      mthd <- "t"
    } else {
      pval <- 2 * pnorm(abs(tval), lower.tail = FALSE)
      cnames <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
      mthd <- "z"
    }
    rval <- cbind(est, se, tval, pval)
    colnames(rval) <- cnames
    rownames(rval) <- rnames
    class(rval) <- "coeftest"
    attr(rval, "method") <- paste(mthd, "test of coefficients")
    ##  dQuote(class(x)[1]), "object", sQuote(deparse(substitute(x))))
    
    ## supplementary information for model summary
    attr(rval, "df") <- df
    attr(rval, "nobs") <- x$nobs
    attr(rval, "logLik") <- logLik(x, ...)
    if(save) attr(rval, "object") <- x
    
    return(rval)
  } 
  
  print.coeftest <- function(x, ...)
  {
    mthd <- attr(x, "method")
    if(is.null(mthd)) mthd <- "Test of coefficients"
    cat(paste("\n", mthd,":\n\n", sep = ""))
    printCoefmat(x, ...)
    cat("\n")
    invisible(x)
  }
  
  coef.coeftest <- function(object, ...) {
    object[, 1L, drop = TRUE]
  }
  
  df.residual.coeftest <- function(object, ...) {
    df <- attr(object, "df")
    if(df > 0) df else NULL
  }
  
  nobs.coeftest <- function(object, ...) {
    nobs <- attr(object, "nobs")
    if(nobs >= 0) nobs else NULL
  }
  
  logLik.coeftest <- function(object, ...) {
    attr(object, "logLik")
  }
  
  confint.coeftest <- function(object, parm = NULL, level = 0.95, ...)
  {
    ## get estimates
    est <- object[, 1L]
    se <- object[, 2L]
    
    ## process level
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    
    ## get quantile from central limit theorem
    df <- attr(object, "df")
    if(is.null(df)) df <- 0
    fac <- if(any(is.finite(df)) && all(df > 0)) qt(a, df = df) else qnorm(a)
    
    ## set up confidence intervals
    ci <- cbind(est + fac[1] * se, est + fac[2] * se)
    colnames(ci) <- paste(format(100 * a, trim = TRUE, scientific = FALSE, digits = 3L), "%")
    
    ## process parm
    if(is.null(parm)) parm <- seq_along(est)
    if(is.character(parm)) parm <- which(names(est) %in% parm)
    ci <- ci[parm, , drop = FALSE]
    return(ci)
  } 

  #détermination du décalage à appliquer pour que les résidus soient bruit blanc
  library(TSA)
  t<-armasubsets(trafic_routier, nar=15, nma=15, y.name="trafic_routier", ar.method="ols")
  plot(t) #permet de voir les combinaisons de décalages possibles
  
  #vérification du caractère bruit blanc des résidus
  library(lmtest)
  fit1<-arima(trafic_routier,order=c(13,0,12)) 
  fit1
  coeftest(fit1)
  acf(c(fit1$residuals),lag.max=36) #les résidus sont bien bruit blanc pour un décalage p=13 et q=12
  
 


library(TSA)
armatest<-armasubsets(trafic_routier, nar=13, nma=13, y.name="trafic_routier", ar.method="ols")
plot(armatest)


fit1<-arima(trafic_routier,order=c(25,0,0)) 
acf(c(fit1$residuals),lag.max=36)
plot(fit1$residuals)
abline(h=0)
plot(trafic_routier_diff1)
abline(h=0)



  #commande armaselect
# Auxiliary function

Lag <- function (x, shift = 1) 
{
  xLen <- length(x)
  if (shift == 0) 
    return(x)
  ret <- as.vector(character(xLen), mode = storage.mode(x))
  attrib <- attributes(x)
  if (length(attrib$label)) 
    attrib$label <- paste(attrib$label, "lagged", shift, 
                          "observations")
  if (abs(shift) < xLen) {
    if (shift > 0) 
      ret[-(1:shift)] <- x[1:(xLen - shift)]
    else ret[1:(xLen + shift)] <- x[(1 - shift):xLen]
  }
  attributes(ret) <- attrib
  return(ret)
}


armaselect <- function (y, max.p = 15, max.q = 15, nbmod = 10) 
{
  matlag <- function(y, maxlag) {
    y = as.matrix(y)
    n = nrow(y)
    x = matrix(1, nrow = n, ncol = 1)
    for (i in 1:maxlag) {
      x = cbind(x, Lag(y, i))
    }
    x = x[, -1]
    colnames(x) = paste("Lag_", as.character(1:maxlag), sep = "")
    x
  }
  n = length(y)
  pmaxi = floor(min(n - 1, 10 * log10(n)))
  yc = y - mean(y)
  z.tilde = ar(yc, aic = FALSE, order.max = pmaxi, method = "yule-walker", 
               demean = FALSE)$resid
  yret = matlag(yc, max.p)
  resret = matlag(z.tilde, max.q)
  mm = lm(yc ~ 0)
  sbc = nrow(resret) * log(var(mm$residuals))
  bic = AIC(mm, k = log(nrow(resret)))
  resul = matrix(NA, nrow = (max.p + 1) * (max.q + 1), ncol = 4)
  colnames(resul) = c("p", "q", "bic", "sbc")
  ili = 1
  resul[ili, ] = c(0, 0, bic, sbc)
  for (ip in 1:max.p) {
    ili = ili + 1
    iq = 0
    mm = lm(yc ~ yret[, 1:ip] - 1)
    bic = AIC(mm, k = log(nrow(resret)))
    sbc = nrow(resret) * log(var(mm$residuals)) + ip * log(nrow(resret))
    resul[ili, ] = c(ip, 0, bic, sbc)
  }
  for (iq in 1:max.q) {
    ili = ili + 1
    ip = 0
    mm = lm(yc ~ resret[, 1:iq] - 1)
    bic = AIC(mm, k = log(nrow(resret)))
    sbc = nrow(resret) * log(var(mm$residuals)) + iq * log(nrow(resret))
    resul[ili, ] = c(0, iq, bic, sbc)
  }
  for (ip in 1:max.p) {
    for (iq in 1:max.q) {
      ili = ili + 1
      mm = lm(yc ~ yret[, 1:ip] + resret[, 1:iq] - 1)
      bic = AIC(mm, k = log(nrow(resret)))
      sbc = nrow(resret) * log(var(mm$residuals)) + (ip + 
                                                       iq) * log(nrow(resret))
      resul[ili, ] = c(ip, iq, bic, sbc)
    }
  }
  ordre = order(resul[, 4])
  sbc_opt = resul[ordre, ][1:nbmod, c(1, 2, 4)]
  colnames(sbc_opt) = c("p", "q", "sbc")
  sbc_opt
}
 
armaselect(trafic_routier,max.p=15,max.q=15,nbmod=10)
fit1<-arima(trafic_routier,order=c(14,0,0))
fit1
coeftest(fit1)
acf(fit1$residuals,lag.max=36)


install.packages("forecast")
library(forecast)
auto.model<-auto.arima(trafic_routier)
summary(auto.model)
coeftest(auto.model)
acf(c(auto.model$residuals),lag.max=36)

sarima<-Arima(trafic_routier, order=c(13,1,12), seasonal=c(0,0,0),include.constant=TRUE)
summary(sarima)
coeftest(sarima)
acf(sarima$residuals,lag.max=36)

sarima<-Arima(trafic_routier, order=c(7,0,1), seasonal=c(0,1,2),include.constant=TRUE)
summary(sarima)
coeftest(sarima)
acf(sarima$residuals,lag.max=36)

sarima<-Arima(trafic_routier, order=c(1,0,8), seasonal=c(0,1,2),include.constant=TRUE)
summary(sarima)
coeftest(sarima)
acf(sarima$residuals,lag.max=36)

sarima<-Arima(trafic_routier, order=c(6,0,6), seasonal=c(0,1,2))
summary(sarima)
coeftest(sarima)
acf(sarima$residuals,lag.max=36)


testmerde<-forecast(sarima, h=12)
autoplot(testmerde)