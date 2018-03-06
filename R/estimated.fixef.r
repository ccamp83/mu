#' @export
estimated.fixef <- function(lmer.model, true.model=NULL)
{
  if(is.null(true.model))
  {
    modfixefLmer <- round(data.frame(
      "estimate" = fixef(lmer.model),
      "standard_error" =  as.numeric(sqrt(diag(as.matrix(vcov(lmer.model)))))
    ), 3) # rounded to the first three decimals
    modfixefLmer$name <- names(fixef(lmer.model))
    print(ggplot(aes(name, estimate), data=modfixefLmer) + geom_bar(stat='identity', position='dodge') + 
            geom_errorbar(aes(ymin=estimate-standard_error*1.96, ymax=estimate+standard_error*1.96), 
                          color='black', position=position_dodge(.9), width=.5) + theme_bw())
  } else
  {
    beta.matrix <- true.model$beta.matrix
    beta.fix <- as.numeric(true.model$beta.matrix)
    theta <- as.numeric(true.model$theta.matrix)
    sigma <- true.model$sigma
    ncoef <- length(beta.matrix)
    
    modfixefLmer <- round(data.frame(
      "estimate" = fixef(lmer.model),
      "standard_error" = as.numeric(sqrt(diag(as.matrix(vcov(lmer.model)))))
    ), 3) # rounded to the first three decimals
    modfixefLmer$name <- names(fixef(lmer.model))
    
    model.extra.coefs <- setdiff(names(fixef(lmer.model)),names(beta.matrix))
    
    fixef.comparison <- as.data.frame(matrix(NA, ncol=length(c(names(beta.matrix), model.extra.coefs)), nrow=1))
    fixef.comparison <- c(as.numeric(beta.matrix), rep(NA, length(model.extra.coefs)))
    names(fixef.comparison) <- union(names(beta.matrix), names(fixef(lmer.model)))
    
    fixef.mod <- fixef(lmer.model)[match(names(fixef.comparison), names(fixef(lmer.model)))]
    names(fixef.mod) <- names(fixef.comparison)
    fixef.comparison <- as.data.frame(rbind(fixef.comparison, fixef.mod))
    
    mod.stder <- as.data.frame(t(modfixefLmer[2]))
    fixef.mod.stder <- as.numeric(mod.stder)[match(names(fixef.comparison), names(mod.stder))]
    names(fixef.mod.stder) <- names(fixef.comparison)
    fixef.mod.stder <- as.data.frame(rbind(fixef.comparison[2,], fixef.mod.stder))
    
    stder.temp <- suppressMessages(melt(fixef.mod.stder)$value)
    stder.temp[seq(1,(length(fixef.comparison)*2), by=2)] <- NA
    
    fixef.comparison <- suppressMessages(as.data.frame(melt(fixef.comparison)))
    fixef.comparison$stder <- stder.temp
    fixef.comparison$which <- c("true_values","model_estimates")
    
    print(ggplot(aes(variable, value, color=which, fill=which), data=fixef.comparison) + geom_bar(stat='identity', position='dodge') + 
            geom_errorbar(aes(ymin=value-stder*1.96, ymax=value+stder*1.96), color='black', position=position_dodge(.9), width=.5) + theme_bw())    
  }
}  


