#' @export
estimated.coef <- function(lmer.model, true.model, exp.data)
{
  beta.matrix <- true.model$beta.matrix
  ncoef <- length(beta.matrix)
  model.extra.coefs <- intersect(names(fixef(lmer.model)),names(beta.matrix))
  
  b <- length(names(coef(lmer.model)))
  t <- length(names(VarCorr(lmer.model)))
  
  # random effects
  modcoefLmer <- list()
  
  
  for(i in 1:b)
  {
    modcoefLmer[[i]] <- coef(lmer.model)[[i]][mixedsort(rownames(coef(lmer.model)[[i]])),model.extra.coefs]
    
    par(mfrow=dim(matrix(names(modcoefLmer[[i]]), ncol=ceil(ncoef/2))))
    
    for(j in 1:ncoef)
    {
      plot(data.frame("true_values" = exp.data$coef.matrix[j],
                      modcoefLmer[[i]][j]), pch=19); abline(0,1)
    }
    
    cat("Page", i, "of", b, "\nHit any key to continue.\n\n")
    line <- readline()
  }    
}