#' Simulate data of an experiment with a given linear design
#' @param design an object of the class mu.exp.design produced with the function design.experiment
#' @param subj number of subjects
#' @param repetitions number of repetitions per subject
#' @param return_data default to TRUE. Use FALSE to produce just a trials sequence
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
#' (r <- design.experiment(lIV, tIV, betas = cus.beta, thetas = cus.theta, s = cus.sigma, exp.formula, seed=trial.seed))
#' 
#' # start the simulation
#' exp.info <- simulate.experiment(r, 10, 10)
#' 
#' # extract the simulated data
#' exp <- exp.info$exp
#' 
#' # plot the results
#' ggplot(aes(Var1, y, color=Var2, group=Var2), data=exp) + facet_wrap(~subjName) + 
#'   geom_point(size=3, alpha=.4) +                                                              # draw raw scores
#'   stat_smooth(method='lm', level=.68, se=F, size=1) +                                         # draw fit line
#'   stat_summary(fun.data='mean_sdl', geom='point', size=3, color='black') +                    # draw mean points
#'   stat_summary(fun.data='mean_sdl', mult=1/sqrt(10), geom='errorbar', width=.2, size=1.2) +   # draw errorbars
#'   theme_bw()
#'   
#' # check a fit of the data (with more subjects)
#' exp.info <- simulate.experiment(r, 100, 10)
#' exp <- exp.info$exp
#' (lmod1 <- lmer(y ~ Var1*Var2 + (1 + Var1*Var2|subjName), data=exp))
#' @return An object of the class mu.exp.design with the following values
#' 
#' @return seed the seed that was used during the simulation
#' @return exp the simulated data
#' @return coef.matrix the subj-by-subj coefficient matrix 
#' @export
simulate.experiment <- function(object, subj, repetitions, return_data = T)
{ 
  if(class(object) == "mu.exp.design")
  {
    cat("Simulating... ")
  } else
  {
    stop('Object must be of class mu.exp.design')
  }
  
  lexp <- object$IV
  lexp[["IV_type"]] = object$IV_type
  between_cols <- which(lexp[["IV_type"]] == "b")
  between_data <- expand.grid(lexp[between_cols])
  lexp[["repetition"]] = 1:repetitions
  lexp[["subjName"]] = 1:subj
  ## we create a subset with only the within variables
  within_cols <- which(lexp[["IV_type"]] == "w")
  within_data <- expand.grid(lexp[c(within_cols, length(lexp)-1, length(lexp))])
  
  # preparing the dataset
  exp <- NULL
  # is there a between component to add to the dataset?
  if(nrow(between_data) > 0)
  {
    for(i in 1:nrow(between_data))
    {
      within_data_temp <- within_data
      within_data_temp$subjName <- within_data_temp$subjName + subj*(i-1)
      exp <- rbind(exp, cbind(within_data_temp, between_data[i,], row.names = NULL))
    }
    names(exp)[(length(within_data)+1):length(exp)] <- names(between_data)
  } else
    exp <- within_data
  
  exp$subjName <- paste('subj', exp$subjName, sep='')
  exp$trialN <- 1:(nrow(within_data)/subj)
  # reorder the columns
  exp <- exp[c("subjName","trialN","repetition",names(object$IV))]
  
  # dataset matrix
  exp.matrix <- as.data.frame(model.matrix(object$formula, data=exp))
  
  # prepare the output
  beta <- NULL
  coef.matrix <- NULL
  
  if(return_data)
  {
    #### sampling
    
    beta.matrix <- object$beta.matrix
    beta.fix <- as.numeric(object$beta.matrix)
    sigma <- object$sigma
    ncoef <- length(beta.matrix)
    
    M <- as.matrix(object$theta.matrix)
    L = t(chol(M)) # transposed so it gives the lower triangle
    
    # First we need to extract the coefficients for each individual, given the covariance matrix
    # Random variables that follow an M correlation matrix
    r = L %*% matrix(rnorm(lengthunique(exp$subjName)*ncoef), nrow=ncoef, ncol=lengthunique(exp$subjName))
    r = t(r)
    # Translate each column by its respective beta (thus preserving variances and covariances)
    fixefM <- t(matrix(rep(beta.fix, ncoef*lengthunique(exp$subjName)), nrow=ncoef, ncol=lengthunique(exp$subjName)))
    beta <- r + fixefM
    beta <- as.data.frame(beta); row.names(beta) <- unique(exp$subjName)
    
    coef.matrix <- beta[rep(seq_len(nrow(beta)), each=max(exp$trialN)),]; row.names(coef.matrix) <- 1:nrow(coef.matrix)
    exp.coef <- as.data.frame(as.matrix(exp.matrix) * coef.matrix)
    
    #### experiment responses
    exp$y <- apply(exp.coef, 1, sum) + rnorm(nrow(exp), 0, sigma)
  }
  
  # package the output
  output <- list("seed" = object$seed,
                 "exp" = exp,
                 "coef.matrix" = beta)
  class(output) <- "mu.exp.design"
  
  cat("Done.\n\n")
  return(output)
}