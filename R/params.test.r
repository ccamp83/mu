#' Test a model's parameters
#' @author Carlo Campagnoli
#' @details test
#' @export
params.test <- function(object)
{
  if(class(object)=='shapelab.exp.design')
  {    
    beta.matrix <- object$beta.matrix
    theta.matrix <- diag(as.matrix(object$theta.matrix))
    sigma <- object$sigma
    
    # theoretical fixef effects test
    
    beta.strength <- data.frame(
      "beta_z_score" = names(beta.matrix), 
      "value" = round(as.numeric(abs(beta.matrix)/theta.matrix), 3), # mean/stddev ratio (z score)
    )
    
    # this graph shows which coefficients are actually true parameters of the model (threshold: greater than 1)
    theor.beta <- ggplot_gtable(ggplot_build(qplot(beta_z_score, value, data=beta.strength) +
                                               stat_summary(fun.data='mean_sdl', geom='bar', size=3, color='black') + geom_hline(yintercept=1, linetype='dashed', size=2) + 
                                               theme_bw()))
    
    # theoretical random effects test
    
    theta.strength <- data.frame(
      "theta_strength" = names(theta.matrix), 
      "value" = round(as.numeric((theta.matrix^2)/(sigma^2)), 3)
    )
    # if the ratio between a coefficient's variance and the residuals' variance
    # is very large (i.e. > 1/sigma^2 and > 1) the random coefficients will be off from true values = they cannot be trusted
    theor.theta <- ggplot_gtable(ggplot_build(qplot(theta_strength, value, data=theta.strength) + 
                                                stat_summary(fun.data='mean_sdl', geom='bar', size=3, color='black') + 
                                                geom_hline(yintercept=1/sigma^2, linetype='dashed', size=2) + 
                                                geom_hline(yintercept=1, linetype='dashed', size=1.6, color='red') +                       
                                                theme_bw()))
    
    return(grid.arrange(theor.beta, theor.theta, ncol=2))
    
  } else if(class(object)=='lmerMod' | class(object)=='merModLmerTest')
  {
    lmod <- object
    
    # test of fixed effects
    
    r <- length(names(VarCorr(lmod)))
    
    random.effects.mean <- list()
    random.effects.stddev <- list()
    
    for(i in 1:r)
    {
      random.effects.mean[[i]] <- abs(fixef(lmod)[intersect(names(attr(VarCorr(lmod)[[i]], "stddev")),names(fixef(lmod)))])
      random.effects.stddev[[i]] <- attr(VarCorr(lmod)[[i]], "stddev")  
    }
    
    all.ratios.fixed <- list()
    for(i in 1:r)
    {
      all.ratios.fixed[[i]] <- fixef(lmod) # temporarily filled with fiexd effects
      all.ratios.fixed[[i]] <- all.ratios.fixed[[i]] * NA
      all.ratios.fixed[[i]][intersect(names(all.ratios.fixed[[i]]),names(random.effects.mean[[i]]))] <- random.effects.mean[[i]]/random.effects.stddev[[i]]
    }
    
    all.ratios.fixed <- data.frame(all.ratios.fixed)
    names(all.ratios.fixed) <- names(VarCorr(lmod))
    all.ratios.fixed <- suppressMessages(melt(all.ratios.fixed))
    
    mod.beta.strength <- data.frame(
      "beta_z_score" = names(fixef(lmod)), 
      "value" = round(as.numeric(all.ratios.fixed$value), 3),
      "name" = all.ratios.fixed$variable
    )
    mod.beta.strength$which <- with(mod.beta.strength, ifelse(!is.na(value), "random", "constant"))
    mod.beta <- ggplot_gtable(ggplot_build(qplot(beta_z_score, value, data=mod.beta.strength) +
                                             stat_summary(fun.data='mean_sdl', geom='bar', size=3, color='black') + geom_hline(yintercept=1, linetype='dashed', size=2) + 
                                             facet_grid(name ~ .) + theme_bw()))
    
    # test of random effects
    
    all.ratios.random <- list()
    
    for(i in 1:r)
    {
      all.ratios.random[[i]] <- fixef(lmod) # temporarily filled with fixed effects
      all.ratios.random[[i]] <- all.ratios.random[[i]] * NA # values are NAed
      all.ratios.random[[i]][intersect(names(all.ratios.random[[i]]),names(random.effects.mean[[i]]))] <- (random.effects.stddev[[i]]^2)/(sigma(lmod)^2)
    }
    
    all.ratios.random <- data.frame(all.ratios.random)
    names(all.ratios.random) <- names(VarCorr(lmod))
    all.ratios.random <- suppressMessages(melt(all.ratios.random))
    
    mod.theta.strength <- data.frame(
      "mod_theta_strength" = names(fixef(lmod)), 
      "value" = round(as.numeric(all.ratios.random$value), 3),
      "name" = all.ratios.random$variable
    )
    mod.theta.strength$which <- with(mod.theta.strength, ifelse(!is.na(value), "random", "constant"))
    # if the ratio between a coefficient's standard deviation and the residuals' stddev
    # is very small (i.e. < 1/sigma and < 1) the random coefficients will be off from true values = they cannot be trusted
    mod.theta <- ggplot_gtable(ggplot_build(qplot(mod_theta_strength, value, data=mod.theta.strength) +
                                              stat_summary(fun.data='mean_sdl', geom='bar', size=3, color='black') + 
                                              geom_hline(yintercept=1/sigma(lmod)^2, linetype='dashed', size=2) + 
                                              geom_hline(yintercept=1, linetype='dashed', size=1.6, color='red') +                       
                                              facet_grid(name ~ .) + theme_bw()))
    
    print(grid.arrange(mod.beta, mod.theta, ncol=2))
    test.results <- list("mod.beta.strength" = mod.beta.strength,
                         "mod.theta.strength" = mod.theta.strength)
    return(test.results)
  } else
  {
    cat("Model not supported.")
  }
}