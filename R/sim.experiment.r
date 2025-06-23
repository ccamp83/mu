#' Simulate data of an experiment with a given linear design
#' @param design an object of the class mu.exp.design produced with the function design.experiment
#' @param subj number of subjects
#' @param repetitions number of repetitions per subject
#' @param family response type. Default is "gaussian". Use "binomial" for binary responses
#' @param random_seed logical. When FALSE (default) the data is simulated using the seed passed via the design object. Set to TRUE to generate a random seed (ignores the seed parameter passed by the design object)
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
#' cus.beta <- c(3, 6, 9, 12) # means of the fixed effects
#' cus.theta <- c(1, 2, 3, 4)  # standard errors of the random components
#' cus.sigma <- 3 # sigma of the model
#'
#' # few examples
#' (r <- design.experiment(lIV, tIV, betas = cus.beta, thetas = cus.theta, s = cus.sigma, exp.formula, seed=trial.seed))
#'
#' # start the simulation
#' exp.info <- sim.experiment(design = r,
#'                            subj = 10,
#'                            repetitions = 10)
#'
#' # extract the simulated data
#' exp <- exp.info$exp
#'
#' # plot the results
#' ggplot(aes(Var1, y, color=Var2, group=Var2), data=exp) + facet_wrap(~subjName) +
#'   geom_point(size=3, alpha=.4) +                                                  # draw raw scores
#'   stat_smooth(method=lm, level=.68, se=F, linewidth=1, formula = y ~ x) +         # draw fit line
#'   stat_summary(fun.data = mean_se, geom='point', size=3, color='black') +         # draw mean points
#'   stat_summary(fun.data = mean_se, geom='errorbar', width=.2, linewidth=1.2) +    # draw errorbars
#'   theme_bw()
#'
#' # check a fit of the data (with more subjects)
#' exp.info <- sim.experiment(design = r,
#'                            subj = 100,
#'                            repetitions = 10)
#' exp <- exp.info$exp
#' (lmod1 <- lme4::lmer(y ~ Var1*Var2 + (1 + Var1*Var2|subjName), data=exp,
#'                      control = lmerControl(optimizer = "bobyqa")))
#' @return An object of the class mu.exp.design with the following values
#'
#' @return seed the seed that was used during the simulation
#' @return exp the simulated data
#' @return coef.matrix the subj-by-subj coefficient matrix
#' @export sim.experiment
sim.experiment <- function(design, subj, repetitions, family = "gaussian", random_seed = F, return_data = T)
{
  # Input validation
  if (!inherits(design, "mu.exp.design")) {
    stop('Design must be of class mu.exp.design')
  }
  formula_vars <- all.vars(design$formula)
  if (length(formula_vars) == 0) {
    stop("Design formula must contain at least one variable.")
  }
  if (is.null(names(design$IV)) || any(nchar(names(design$IV)) == 0) || any(is.na(names(design$IV)))) {
    stop("All elements of design$IV must be named and non-empty.")
  }
  if (!all(formula_vars %in% names(design$IV))) {
    stop("All variables in the design formula must be present in design$IV.")
  }

  if(class(design) == "mu.exp.design")
  {
    cat("Simulating... ")
  } else
  {
    stop('Design must be of class mu.exp.design')
  }

  sim.seed <- ifelse(random_seed, round(runif(1, 0, 10000)), design$seed)
  set.seed(sim.seed)

  lexp <- design$IV
  lexp[["IV_type"]] = design$IV_type
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
  exp <- exp[c("subjName","trialN","repetition",names(design$IV))]

  # dataset matrix
  exp.matrix <- as.data.frame(model.matrix(design$formula, data=exp))

  # prepare the output
  beta <- NULL
  coef.matrix <- NULL

  if(return_data)
  {
    #### sampling

    beta.matrix <- design$beta.matrix
    beta.fix <- as.numeric(design$beta.matrix)
    sigma <- design$sigma
    ncoef <- length(beta.matrix)

    M <- as.matrix(design$theta.matrix)
    L = t(chol(M)) # transposed so it gives the lower triangle

    # First we need to extract the coefficients for each individual, given the covariance matrix
    # Random variables that follow an M correlation matrix
    r = L %*% matrix(rnorm(lengthunique(exp$subjName)*ncoef), nrow=ncoef, ncol=lengthunique(exp$subjName))
    r = t(r)
    # Translate each column by its respective beta (thus preserving variances and covariances)
    fixefM <- t(matrix(rep(beta.fix, lengthunique(exp$subjName)), nrow=ncoef, ncol=lengthunique(exp$subjName)))
    beta <- r + fixefM
    beta <- as.data.frame(beta); row.names(beta) <- unique(exp$subjName)

    coef.matrix <- beta[rep(seq_len(nrow(beta)), each=max(exp$trialN)),]; row.names(coef.matrix) <- 1:nrow(coef.matrix)
    exp.coef <- as.data.frame(as.matrix(exp.matrix) * coef.matrix)

    #### experiment responses
    if (family == "gaussian") {
      exp$y <- apply(exp.coef, 1, sum) + rnorm(nrow(exp), 0, sigma)
    } else if (family == "binomial") {
      eta <- apply(exp.coef, 1, sum)
      p <- 1 / (1 + exp(-eta))
      exp$y <- rbinom(n = nrow(exp), size = 1, prob = p)
    } else {
      stop("family must be either 'gaussian' or 'binomial'")
    }
  }

  # package the output
  output <- list("seed" = sim.seed,
                 "exp" = exp,
                 "coef.matrix" = beta)
  class(output) <- "mu.exp.design"

  cat("Done.\n\n")
  return(output)
}
