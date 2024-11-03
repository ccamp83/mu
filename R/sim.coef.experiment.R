#' Generate coefficients given desired data and vice versa
#' @examples
#' # set a seed
#' trial.seed <- 9000
#'
#' # initialize the factors list
#' lIV <- list(
#'   Var1 = factor(1:2, labels = c("a","b"))
#'   ,
#'   Var2 = factor(1:3)
#' )
#'
#' tIV <- c("w","w")
#'
#' # describe the model
#' exp.formula <- ~ Var1+Var2
#'
#' # simply builds the dataset from the model formula
#' sim.coef.experiment(lIV, exp.formula)
#' # calculates results given desired coefficients
#' sim.coef.experiment(lIV, exp.formula, coefs = c(1,1,1,1))
#' # calculate coefficients given desired results
#' sim.coef.experiment(lIV, exp.formula, results = c(10,20,15,25,30,40))
#'
#' (r <- design.experiment(lIV, tIV, exp.formula, calculate.coef.num = T))
#'
#' # initialize the coefficients
#' cus.theta <- c(1, 1, 1, 1)  # standard errors of the random components
#' cus.sigma <- 1 # sigma of the model
#'
#' # few examples
#' (r <- design.experiment(lIV, tIV, exp.formula,
#'                         betas = sim.coef.experiment(lIV, exp.formula, results = c(10,20,15,25,30,40))$coef[1:4],
#'                         thetas = cus.theta,
#'                         s = cus.sigma,
#'                         seed=trial.seed))
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
#' (lmod1 <- lme4::lmer(y ~ Var1*Var2 + (1 + Var1*Var2|subjName), data=exp,
#'                      control = lmerControl(optimizer = "bobyqa")))
#' @export
sim.coef.experiment <- function(list.IV, formula, coefs = NA, results = NA)
{
  data <- expand.grid(list.IV[all.vars(formula)])

  modm <- model.matrix(formula, data)
  ncoef = length(names(as.data.frame(modm)))
  coef.names <- names(as.data.frame(modm))

  # fetch data points N
  dataPointsN <- nrow(data)
  # fetch coefficients N
  coefsN <- length(coef.names)

  output <- list(data = data,
                 coefficients = coef.names)

  hasCoefs <- any(!is.na(coefs))
  hasResults <- any(!is.na(results))

  if(hasCoefs & hasResults)
  {
    stop("coefs and results cannot be both specified.")
  }

  # calculate results
  if(hasCoefs)
  {
    output[["data"]]$results <- as.numeric(modm %*% coefs)

    output[["coefficients"]] <- as.numeric(matrix(coefs, ncol = ncoef))
    names(output[["coefficients"]]) <- coef.names

  } else
    # calculate coefficients
    if(hasResults)
    {
      formula.fmod <- as.formula(paste0("~ ", paste0(all.vars(formula), collapse = "*")))
      fmodm <- model.matrix(formula.fmod, model.frame(formula.fmod, data))

      if(any(dim(fmodm) != dim(modm)))
      {
        fmodmf <- as.data.frame(fmodm)
        modmf <- as.data.frame(modm)
        missingcoefs <- as.data.frame(setdiff(fmodmf, modmf))
        names(missingcoefs) <- paste0("(",names(missingcoefs),")")

        fmodm <- as.matrix(cbind(modmf, missingcoefs))
        coef.names <- names(as.data.frame(fmodm))
      }

      output[["data"]]$results <- results

      output[["coefficients"]] <- zapsmall(as.numeric(t(MASS::ginv(fmodm) %*% results)))
      names(output[["coefficients"]]) <- coef.names

      # update coefficients N to exclude those with value zero (not estimated)
      coefsN <- length(output[["coefficients"]][output[["coefficients"]]!=0])
    }

  cat("\n", coefsN, "coefficients to be estimated with", dataPointsN, "data points.\n\n")

  return(output)
}

