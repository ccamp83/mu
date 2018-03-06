#' Simulate data according to a specific multivariate linear model
#' @param dv.n # of dependent variables
#' @param iv.lev can be either a vector or a list. If a vector, it must be of length equal to the # of independent variables, where each value in the vector specifies the # of levels of each iv. If a list, it must have an equal amount of items to the # of ind vars, where each item is a vector of length equal to the levels of that particular ind var. List helps particularly in case of numeric ind vars.
#' @param formula optional. A specific model can be called. The function implements the full model by default (main effects of all iv plus all their interactions). Valid entries are "full" (default) or any formula in the form compatible to R.
#' @param is.iv.factor logical vector of length iv.len. Each value specifies whether an iv must be treated as factor (true) or numerical (false)
#' @param repetitions # of repetitions. Must be > 0 (default to 1).
#' @param B matrix of coefficients of the linear model. If not input by user (default), a random matrix is generated from a unifom distribution within the range [0, 100] according to the model's formula
#' @param e.sd vector of standard deviations of residuals. Must have the same length of dv.n. If not input by user (default), a random vector is generated from a unifom distribution within the range [0, 25]
#' @param dv.orthogonal logical. If true the correlation matrix is an identity matrix of dimension dv.n-by-dv.n. Default to TRUE.
#' @param seed allows to input a specific seed for reproducibility. If NA, a random seed is generated
#' @param dv.min set an inferior limit for the dependent variable (for now limited to one boundary that will affect all dependent variables)
#' @param dv.max set an superior limit for the dependent variable (for now limited to one boundary that will affect all dependent variables)
#' @examples 
#' ### Example 1
#' libraries()
#' dv.n.t <- 2
#' repetitions.t <- 5
#' iv.lev.t <- c(2, 2)
#' is.iv.factor.t <- c(T,T)
#' B.t <- matrix(c(30, 10, -5, 0, 300, 0, 100, -20), ncol=2)
#' e.sd.t <- c(10, 10)
#' 
#' test <- sim.mvlmod(dv.n.t, iv.lev.t, is.iv.factor=is.iv.factor.t, repetitions=repetitions.t, B=B.t, e.sd=e.sd.t, dv.orthogonal=T)
#' test
#' 
#' ### Example 2
#' # des.exp can be used to help the construction of sim.mvlmod
#' libraries()
#' dv.n.t <- 2
#' repetitions.t <- 2
#' iv.lev.t <- list("V1" = c(30,40), "V2" = c(0:1), "V3" = c(100, 200)) # custom levels of the iv
#' is.iv.factor.t <- c(T,F,F)
#' formula.t <- ~ V1+V2+V3 # custom formula
#' seed.t <- 123456
#' (exp.des <- des.exp(dv.n.t, iv.lev.t, formula.t, is.iv.factor=is.iv.factor.t)) # check the design
#' # note IV1 and remember that IV2 is numeric
#' exp <- sim.mvlmod(dv.n.t, iv.lev.t, formula.t, is.iv.factor=is.iv.factor.t, repetitions=repetitions.t, seed=seed.t)
#' (Ydata <- exp$Y)
#' 
#' 
#' @return A list with the following items:
#' @return IV a list with the independent variables
#' @return R  the correlation matrix
#' @return X  the regressors matrix
#' @return B  the coefficients matrix
#' @return E  the errors matrix
#' @return Y  the complete dataset generated by the function
#' @return formula  the formula used to generate the dataset
#' @return seed the seed used for the generation of the data
#' 
#' @export
sim.mvlmod <- function(dv.n, iv.lev, formula="full", is.iv.factor, repetitions=1, B=NULL, e.sd=NULL, dv.orthogonal=T, seed=NA, dv.min=-Inf, dv.max=Inf, ...)
{
  if(!is.na(seed))
  {
    set.seed(seed)
  } else
  {
    seed <- round(runif(1, 0, 1000000))
    set.seed(seed)
    cat('Seed randomly initialized: ', seed, '.\n', sep='')
  }
  
  if(length(dv.n)==0)
    stop('dv.n is empty')
  
  if(length(iv.lev)==0)
    stop('iv.lev is empty')
  
  if(length(is.iv.factor)==0)
    stop('is.iv.factor is empty')
  
  if(repetitions <= 0)
    stop('repetiions must be > 0')
  
  ##### model: Y = X B + E
  
  # where
  # Y = n-by-m      (dependent vars matrix)
  # X = n-by-k      (regressors matrix)
  # B = k-by-m      (coefficients matrix)
  # E = n-by-m      (error terms matrix)
  
  # where
  # n = # of observations
  n <- repetitions
  # m = # dependent vars
  m <- dv.n
  # k = 1 + r = intercept + # of regressors
  # k depends on the # and type (levels) of independent variables and on their relations specified by the model
  
  ## IMPLEMENTATION
  
  # 1) regressors matrix X
  
  #-- first find k (number of regressors in the model):
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
  
  # which brings to the final model matrix (for one observation/data point)
  lm.grid <- model.matrix(lm.formula, iv.grid)
  
  # the total number of predictors of the model is k
  k <- ncol(lm.grid)
  
  X <- lm.grid[rep(1:samples, n),] # repeat the model matrix n times ( = times the number of observations)
  
  # 2) coefficients matrix B
  if(length(B)==0){
    beta <- rnorm(k*m, round(runif(k*m, 0, 50))) # coefficients of the regressors, one set for each dependent variable
    B <- matrix(beta, k, m)
  } else {
    if(length(B) != k*m){
      cat('B must be a ', k , '-by-', m, ' matrix.', sep='')
      stop('Wrong # of coefficients.')
    }
  }
  
  # 3) error terms matrix E
  
  # generate a random one in case it is not given in input
  if(length(e.sd)==0)
  {
    e.sd <- runif(m, 0, 25) # residual standard deviations of each dependent variable
  } else {
    if(length(e.sd) != m){
      cat('sdtdev of residuals must be a vector of length ', m, '.', sep='')
      stop('Wrong # of residuals.')
    }
  }
  
  # are the dependent variables orthogonal?
  if(!dv.orthogonal)
  {
    e.r <- rcorrmatrix(m) # correlation matrix with random correlations
  } else
    e.r <- diag(m) # correlation matrix (with independent variables)
  
  # apply correlation matrix to error term = covariance matrix
  if(length(e.sd) == 1)
  {
    e.M <- e.sd %*% e.r %*% e.sd # covariance matrix
  } else
  {
    e.M <- diag(e.sd) %*% e.r %*% diag(e.sd) # covariance matrix
  }
  
  # generate the error matrix given the covariance matrix and the user-defined limits (if any)
  E <- rmvnorm(n*samples, sigma=e.M)
  
  ######## MODEL CONSTRUCTION
  # full data matrix
  Y = X %*% B + E
  
  # check if there are values that are outside of the user-defined limits
  outliers <- length(Y[Y < dv.min | Y > dv.max])
  if(outliers > 0)
  {
    while(outliers > 0)
    {
      outliers <- length(Y[Y < dv.min | Y > dv.max])
      E <- rmvnorm(n*samples, sigma=e.M)
      Y[Y < dv.min | Y > dv.max] <- (X %*% B)[Y < dv.min | Y > dv.max] + E[Y < dv.min | Y > dv.max]
    }
    E <- Y - as.matrix(X %*% B) # recalculate error matrix
  }
  
  # data frame
  rownames(Y) <- 1:(n*samples)
  Ydata <- as.data.frame(Y); names(Ydata) <- paste("m", 1:m, sep='')
  IV.exp <- IV
  IV.exp[["repetition"]] <- factor(1:n)
  Ydata <- cbind(Ydata, expand.grid(IV.exp))
  Ydata$sample <- 1:samples
  Ydata$trialN <- 1:nrow(Ydata)
  # readjust the columns of Ydata
  allcols <- length(Ydata) # all columns
  varscols <- allcols - 3 # only variables columns
  Ydata <- Ydata[c(1:varscols, allcols-1, allcols-2, allcols)] # variables columns, sample (unique combination of IV), repetition, trialN
  
  ######## OUTPUT
  
  R.out <- as.data.frame(e.r); colnames(R.out) <- rownames(R.out) <- paste("m", 1:m, sep='')
  B.out <- as.data.frame(B); colnames(B.out) <- paste("m", 1:m, sep=''); rownames(B.out) <- names(as.data.frame(lm.grid))
  rownames(E) <- 1:(n*samples); E.out <- as.data.frame(E); colnames(E.out) <- paste("m", 1:m, sep='')
  
  output <- list("IV" = IV,
                 "R" = as.matrix(R.out),
                 "X"= X,
                 "B" = as.matrix(B.out),
                 "E" = E.out,
                 "Y" = Ydata,
                 "formula" = lm.formula,
                 "seed" = seed)
  return(output)
}