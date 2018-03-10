libraries()

# set a seed
trial.seed <- 9000

# initialize the factors list
lIV <- list(
  Var1 = factor(1:2, labels = c("a","b"))
  ,
  Var2 = factor(1:2)
)

# specify within and between variables
tIV <- c("w","b")

# describe the model
exp.formula <- ~ Var1*Var2

# initialize the coefficients
cus.beta <- c(1, 4, 5, 1)*3 # means of the fixed effects
cus.theta <- c(1, 4, 5, 1) # standard errors of the random components
cus.sigma <- 3 # sigma of the model

# few examples
(r <- design.experiment(lIV, tIV, betas = cus.beta, thetas = cus.theta, s = cus.sigma, exp.formula, seed=trial.seed))

# run the simulation
exp.info <- simulate.experiment(r, 10, 10)

# extract the simulated data
exp <- exp.info$exp

# plot the results
ggplot(aes(Var1, y, color=Var2, group=Var2), data=exp) + facet_wrap(~subjName) + 
  geom_point(size=3, alpha=.4) +                                                              # draw raw scores
  stat_smooth(method='lm', level=.68, se=F, size=1) +                                         # draw fit line
  stat_summary(geom='point', size=3, color='black') +                    # draw mean points
  stat_summary(geom='errorbar', width=.2, size=1.2) +   # draw errorbars
  theme_bw()

# check a fit of the data (with more subjects)
exp.info <- simulate.experiment(r, 100, 10)
exp <- exp.info$exp
(lmod1 <- lmer(y ~ Var1*Var2 + (1 + Var1*Var2|subjName), data=exp))
mres <- ANOVA(exp, "y", subjName + Var2 ~ Var1)

