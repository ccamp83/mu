#' Comprehensive ANOVA
#' @export
ANOVA <- function (data, dv, formula, type = 3) 
{
  # get variables of the formula
  lhs <- all.vars(update(formula, . ~ 0))
  rhs <- all.vars(update(formula, 0 ~ .))
  
  multivariate <- length(rhs) > 0
  
  variables <- all.vars(formula)
  # factorize all variables
  for (var in variables) {
    data[, var] <- factor(data[, var])
  }
  # reshape to wide format
  wideData <- dcast(data, formula, value.var = dv, fun.aggregate = mean)
  # set the between term
  # and fit the multivariate linear model
  if(length(lhs)>1){
    between <- wideData[,lhs[-1]]
    man.fit.d <- lm(as.matrix(wideData[, (length(lhs)+1):length(wideData)]) ~ between)
  } else {
    man.fit.d <- lm(as.matrix(wideData[, (length(lhs)+1):length(wideData)]) ~ 1)
  }
  
  # prepare idata & idesign
  idata <- NULL
  idesign <- NULL
  
  if(multivariate){
    idata0 <- list()
    i <- 1
    for (var in rhs) {
      idata0[[i]] <- levels(data[, var])
      i <- i + 1
    }
    idata1 <- expand.grid(idata0)
    names(idata1) <- rhs
    idata <- sortdataframe(idata1, by = rhs)
    
    idesign <- formula(paste("~ 1", paste(rhs, collapse = " * "), 
                             sep = " + "))
  }
  man.res.d <- Anova(man.fit.d, type = type, idata = idata, idesign = idesign)
  
  if(multivariate){
    res.uni <- summary(man.res.d)$uni
    
    SSeffect <- res.uni[,"SS"]
    SSerror <- res.uni[,"Error SS"]
    dfEffect <- res.uni[,"num Df"]
    dfError <- res.uni[,"den Df"]
    MSerror <- SSerror/dfError
    SStotal <- SSeffect + sum(unique(SSerror))
    etaSq <- SSeffect / SStotal
    etaSqP <- SSeffect / (SSeffect + SSerror)
    # etaSqP2 <- (Fval * dfEffect) / ((Fval * dfEffect) + dfError)
    omegaSq <- (SSeffect - dfEffect*MSerror) / (SStotal + MSerror)
    
    Fval = res.uni[,"F"]
    Pval = res.uni[,"Pr(>F)"]
    
  } else {
    res.uni <- man.res.d
    
    SSeffect <- res.uni[-nrow(res.uni),"Sum Sq"]
    SSerror <- res.uni[nrow(res.uni),"Sum Sq"]
    dfEffect <- res.uni[-nrow(res.uni),"Df"]
    dfError <- res.uni[nrow(res.uni),"Df"]
    MSerror <- SSerror/dfError
    SStotal <- SSeffect + sum(unique(SSerror))
    etaSq <- SSeffect / SStotal
    etaSqP <- SSeffect / (SSeffect + SSerror)
    omegaSq <- (SSeffect - dfEffect*MSerror) / (SStotal + MSerror)
    
    Fval = res.uni[-nrow(res.uni),"F value"]
    Pval = res.uni[-nrow(res.uni),"Pr(>F)"]
  }
  
  uni <- data.frame("etaSq" = round(etaSq, 3),
                    "etaSqP" = round(etaSqP, 3),
                    "omegaSq" = round(omegaSq, 3),
                    "SS" = round(SSeffect, 2),
                    "Error SS" = round(SSerror, 2),
                    "num Df" = dfEffect,
                    "den Df" = dfError,
                    "F" = round(Fval, 3),
                    "p" = round(Pval, 3),
                    "sig" = ifelse(Pval < .001, "***", 
                                   ifelse(Pval < .01, "** ", 
                                          ifelse(Pval < .05, "*  ", 
                                                 ifelse(Pval < .1, ".  ", ""))))
  )
  
  multi <- NULL
  spherTest <- NULL
  spherCorrection <- NULL
  if(multivariate){
    multi <- etasq(man.res.d, anova = T)
    spherTest <- round(summary(man.res.d)$sphericity.tests, 3)
    spherCorrection <- round(as.data.frame(summary(man.res.d)$pval.adjustments)[c(2,4)], 3)
  }
  
  output <- list(fit = man.fit.d, 
                 idata = idata,
                 wdata = wideData,
                 full.analysis = man.res.d,
                 results = list(univariate = uni,
                                multivariate = multi),
                 sphericity.test = spherTest,
                 sphericity.correction = spherCorrection
  )
  
  class(output) <- "ANOVA"
  print(output$results)
  return(invisible(output))
}

