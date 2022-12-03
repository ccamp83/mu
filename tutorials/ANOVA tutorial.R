library(mu)
libraries()

# set a seed
trial.seed <- 9000

# initialize the factors list
lIV <- list(
  Var1 = factor(1:3, labels = c("a","b","c"))
  ,
  Var2 = factor(1:2)
)

tIV <- c("w","b")

# describe the model
exp.formula <- ~ Var1*Var2

# how many coefficients in this model?
design.experiment(lIV, tIV, exp.formula, calculate.coef.num = T)

# initialize the coefficients
cus.beta <- c(3, 12, 15, 3, 5, 6) # means of the fixed effects
cus.theta <- c(1, 4, 5, 1, 2, 3) # standard errors of the random components
cus.sigma <- 3 # sigma of the model

# few examples
(r <- design.experiment(lIV, tIV, betas = cus.beta, thetas = cus.theta, s = cus.sigma, exp.formula, seed=trial.seed))

t(matrix(rep(cus.beta, 6*200), nrow=6, ncol=200*6))

# start the simulation
exp.info <- simulate.experiment(r, 
                                subj = 200, 
                                repetitions = 100)

# extract the simulated data
exp <- exp.info$exp
(lmod1 <- lmerTest::lmer(y ~ Var1*Var2 + (1 + Var1*Var2|subjName), data=exp))
summary(lmod1, corr = F)

aov.res <- ANOVA(exp,
                 dv = "y",
                 formula = subjName + Var2 ~ Var1)

uni(aov.res)
multi(aov.res)
aov.res$sphericity.test

posthoc(aov.res)
