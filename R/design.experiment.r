#' Design an experiment
#' @param list.IV a list of the independent variables with their relative values
#' @param type.IV a vector specifying whether each IV is within ("w") or between ("b")
#' @param formula a string describing the linear model to be simulated, of the type ~ F1 + (F2*F3)
#' @param calculate.coef.num logical, if TRUE the function just calculates the number of coefficients corresponding to the variables listed
#' @param seed optional, a seed to apply to the simulation (for reproducibility)
#' @param beta.min lower boundary of the uniform distribution from which the values/means of the betas are extracted (if betas = NA)
#' @param beta.max upper boundary of the uniform distribution from which the values/means of the betas are extracted (if betas = NA)
#' @param betas this parameters allows to assign a specific mean/value to each parameter of the model. The length of betas must be equal to the amount of coefficients in the model specified by the formula.
#' @param theta.min lower boundary of the uniform distribution from which the standard deviation of the betas are extracted (if thetas = NA)
#' @param theta.max upper boundary of the uniform distribution from which the standard deviation of the betas are extracted (if thetas = NA)
#' @param thetas this parameters allows to assign a specific standard deviation to each parameter of the model. The length of thetas must be equal to the amount of coefficients in the model specified by the formula.
#' @param thetas.orthogonal logical, are the parameters orthogonal?
#' @param s.min lower boundary of the uniform distribution from which the standard deviation of the model's residuals is extracted (if s = NA)
#' @param s.max upper boundary of the uniform distribution from which the standard deviation of the model's residuals is extracted (if s = NA)
#' @param s this parameters allows to assign a specific standard deviation to the model's residuals
#' @examples
#' libraries()
#'
#' # set a seed
#' trial.seed <- 9000
#'
#' # initialize the factors list
#' lIV <- list(
#'   Var1 = factor(1:2, labels = c("a","b"))
#'   ,
#'   Var2 = factor(1:2)
#' )
#'
#' tIV <- c("w","b")
#'
#' # describe the model
#' exp.formula <- ~ Var1*Var2
#'
#' # initialize the coefficients
#' cus.beta <- c(1, 4, 5, 1)*3 # means of the fixed effects
#' cus.theta <- c(1, 4, 5, 1) # standard errors of the random components
#' cus.sigma <- 3 # sigma of the model
#'
#' # few examples
#' (r1 <- design.experiment(lIV, tIV, exp.formula, seed=trial.seed))
#' (r2 <- design.experiment(lIV, tIV, exp.formula, seed=trial.seed, thetas.orthogonal=F))
#' (r3 <- design.experiment(lIV, tIV, exp.formula, seed=trial.seed, beta.min=-5, beta.max=10, theta.min=0, theta.max=10, s.min=5, s.max=12))
#' (r4 <- design.experiment(lIV, tIV, exp.formula, seed=trial.seed, betas=cus.beta, thetas=cus.theta, s=cus.sigma, thetas.orthogonal=F))
#'
#' @return An object of the class mu.exp.design to be used with the function sample.experiment with the following values
#' @return seed the seed that will be used for simulation
#' @return ncoef the number of coefficients of the model
#' @return beta.matrix coefficients matrix
#' @return theta.matrix random components matrix
#' @return corr.matrix random components correlation matrix
#' @return sigma the model residuals standard deviation
#' @return IV list of the parameters of the model (basically returns list.IV)
#' @return type_IV returns type.IV
#' @return formula returns the formula
#' @export
design.experiment <- function(list.IV, type.IV, formula, calculate.coef.num = F, seed=NA, beta.min=0, beta.max=20, betas=NA, theta.min=0, theta.max=8, thetas=NA, thetas.orthogonal=T, s.min=0, s.max=20, s=NA)
{
  # independent variables
  IV <- list.IV
  tIV <- type.IV

  design.grid <- expand.grid(IV)

  design.formula <- formula

  design.matrix <- as.data.frame(model.matrix(design.formula, data=design.grid))

  # create coefficients
  ncoef <- length(design.matrix) # how many coefficients
  coef.names <- names(design.matrix)

  cat('\n# of independent variables:    ', length(IV),
      '\nBetween: ', paste0(names(IV)[tIV %in% "b"], sep = " "),
      '\nWithin: ', paste0(names(IV)[tIV %in% "w"], sep = " "),
      '\nParameters of the model:  ', ncoef, '\n\n',
      sep = '')

  if(!calculate.coef.num)
  {
    if(!is.na(seed))
    {
      set.seed(seed)
    } else
    {
      seed <- sample(1e3, 1)
      set.seed(seed)
    }

    if(any(is.na(betas)))
    {
      beta.fix <- runif(ncoef, min=beta.min, max=beta.max) # coefficients means (fixed effects) are randomly initialized within specific boundaries
    } else
    {
      beta.fix <- betas # coefficients means (fixed effects) are specified by user
    }
    beta.matrix <- data.frame(t(beta.fix)); names(beta.matrix) <- names(design.matrix)

    if(any(is.na(thetas)))
    {
      theta <- runif(ncoef, min=theta.min, max=theta.max)^2 # coefficients standard deviations are randomly initialized within specific boundaries
    } else
    {
      theta <- thetas^2
    }

    if(!thetas.orthogonal)
    {
      R <- rcorrmatrix(ncoef)
      M <- diag(sqrt(theta)) %*% R %*% diag(sqrt(theta))
      theta.matrix <- data.frame(M); names(theta.matrix) <- names(design.matrix); row.names(theta.matrix) <- names(design.matrix)
      corr.matrix <- data.frame(R); names(corr.matrix) <- names(design.matrix); row.names(corr.matrix) <- names(design.matrix)
    } else
    {
      R <- diag(ncoef)
      M <- diag(sqrt(theta)) %*% R %*% diag(sqrt(theta))
      theta.matrix <- data.frame(M); names(theta.matrix) <- names(design.matrix); row.names(theta.matrix) <- names(design.matrix)
      corr.matrix <- data.frame(R); names(corr.matrix) <- names(design.matrix); row.names(corr.matrix) <- names(design.matrix)
    }

    if(is.na(s))
    {
      sigma <- runif(1, min=s.min, max=s.max) # residuals standard deviation is randomly initialized up to a max
    } else
    {
      sigma <- s
    }

    output <- list("seed" = seed,
                   "ncoef" = ncoef,
                   "beta.matrix" = beta.matrix,
                   "theta.matrix" = theta.matrix,
                   "corr.matrix" = corr.matrix,
                   "sigma" = sigma,
                   "IV" = list.IV,
                   "IV_type" = tIV,
                   "formula" = formula
    )
    class(output) <- "mu.exp.design"

  } else {
    output <- list("ncoef" = ncoef,
                   "coef names" = coef.names)
  }

  return(output)
}
