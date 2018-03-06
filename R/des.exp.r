#' Tool for building experiment designs
#' @description Returns the regressors matrix given a certain set of independent variables. See ?sim.mvlmod for details.
#' @export
des.exp <- function(dv.n, iv.lev, formula = "full", is.iv.factor)
{
  if(length(dv.n)==0)
    stop('dv.n is empty')
  
  if(length(iv.lev)==0)
    stop('iv.lev is empty')
  
  if(length(is.iv.factor)==0)
    stop('is.iv.factor is empty')

  # m = # dependent vars
  m <- dv.n
  
  ## IMPLEMENTATION
  
  # 1) regressors matrix X
  
  #-- first find k:
  # let's say we have r independent vars
  r <- length(iv.lev)
  
  # were the names of the IV specified by the user?
  if(!is.list(iv.lev)) # no, then create a list of IV1, IV2, ...., IVn names
  {
    ind.vars <- paste(rep("IV", r), 1:r, sep='')
  } else # yes, save the custom names
  {
    ind.vars <- names(iv.lev)
  }
  
  # each having the following number of levels
  ind.vars.lev <- iv.lev
  # and being or not a factor
  is.ind.var.factor <- is.iv.factor
  
  # the linear model is
  if(formula=='full'){ # full model by default
    lm.formula <- as.formula(paste("~", paste(ind.vars, collapse=" * "), sep=" "))
  } else # custom model
  {
    lm.formula <- formula
  }
  
  # following is the list of independent variables ...
  IV <- list()
  # ... their values
  for(i in 1:r)
  {
    if(!is.list(ind.vars.lev)){
      IV[[ind.vars[i]]] = 1:ind.vars.lev[i]
    } else{
      IV[[ind.vars[i]]] = ind.vars.lev[[i]]
    }
    if(is.ind.var.factor[i]) # ... and initialized as factors if required
      IV[[ind.vars[i]]] = factor(IV[[ind.vars[i]]])
  }
  
  # the resulting design is:
  iv.grid <- expand.grid(IV)
  # and the combinations (samples) given the design grid are:
  samples <- nrow(iv.grid)
  
  # which brings to the final model matrix
  lm.grid <- model.matrix(lm.formula, iv.grid)
  
  return(as.data.frame(lm.grid))
}