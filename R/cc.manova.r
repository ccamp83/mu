#' @export
cc.manova <- function(data, dv, formula, uni=T){
#   dv = "MSE"
#   data = mseData
#   formula = form
  
  variables <- all.vars(formula)
  for(var in variables){
    data[,var] <- factor(data[,var])
  }
  
  wideData <- dcast(data, formula, value.var = dv)
  man.fit.d <- lm(as.matrix(wideData[,2:length(wideData)]) ~ 1)
  
  idata0 <- list()
  i <- 1
  for(var in variables[-1]){
    idata0[[i]] <- levels(data[,var])
    i <- i+1
  }
  idata1 <- expand.grid(idata0)
  names(idata1) <- variables[-1]
  idata <- sortdataframe(idata1, by=variables[2:(length(variables)-1)])
  idesign <- formula(paste("~ 1",paste(variables[-1], collapse=" * "), sep = " + "))
  
  man.res.d <- Anova(man.fit.d,
                     idata = idata,
                     idesign = idesign)
  if(uni){
    return(summary(man.res.d)$uni)
  } else {
    return(man.res.d)
  }
}

#' @export
remindmeofmanova <- function(){
  output <- "wideData <- dcast(mseData, subjName ~ RelDepthObjF + AbsDepthF + depth_cue, value.var = 'MSE')
man.fit.d <- lm(as.matrix(wideData[,2:length(wideData)]) ~ 1)
man.res.d <- Anova(man.fit.d, type = 2,
                   idata = data.frame(RelDepthObjF = factor(rep(1:3, each=2), labels=c('30','40','50')),
                                      AbsDepthF = factor(rep(1:2, 3), labels=c('Near','Far')),
                                      depth_cue = ),
                   idesign = ~ 1 + RelDepthObjF * AbsDepthF)
summary(man.res.d)$uni "
  print(output)
}
