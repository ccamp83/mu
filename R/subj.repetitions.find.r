#' @title Find the ideal amount of subjects and of repetitions per subject
#' @description This function is a tool to help pre-determining the amount of subjects and/or repetitions per subject given the a-priori distribution of the parameters of a model
#' @param object an object of the type shapelab.exp.design as generated with design.experiment
#' @param find.subj logical: do you want to determine the ideal amount of subjects? (default to TRUE)
#' @param find.reps logical: do you want to determine the ideal amount of repetitions per subject? (default to TRUE)
#' @param subj.start initial amount of subjects for the simulation
#' @param subj.max maximum amount of subjects for the simulation
#' @param reps.start initial amount of repetitions per subject for the simulation
#' @param reps.max maximum amount of repetitions per subject for the simulation
#' @export
subj.repetition.find <- function(object, find.subj=T, find.reps=T, subj.start=2, subj.max=20, reps.start=2, reps.max=15)
{

  beta.matrix <- object$beta.matrix
  beta.fix <- as.numeric(object$beta.matrix)
  theta <- as.numeric(diag(as.matrix(object$theta.matrix)))
  sigma <- object$sigma
  ncoef <- length(beta.matrix)
  
  if(reps.start < 2)
  {
    cat('Minimum number of repetitions is 2. Adjusting to 2...\n')
    reps.start <- 2
  }
  
  if(subj.start < 2)
  {
    cat('Minimum number of subjects is 2. Adjusting to 2...\n')
    subj.start <- 2
  }
  
  if(find.subj)
  {
    subj.theor <- subj.start
    max.subj <- subj.max
    
    repeat{
      coef.test <- NULL
      for(co in 1:ncoef)
      {
        coef.test.temp <- NULL
        for(i in 1:1000)
        {
          p <- t.test(rnorm(subj.theor, beta.fix[co], theta[co]))$p.value
          coef.test.temp <- c(coef.test.temp, p)
        }
        # now we look at the proportion of times in which we got a p value greater than .05 across 1000 simulated t-tests
        # the goal is to minimize this number as to make it < .05
        coef.test <- c(coef.test, length(coef.test.temp[coef.test.temp>.05])/length(coef.test.temp))
      }
      coef.test <- as.data.frame(t(coef.test))
      names(coef.test) <- names(beta.matrix)
      coef.test # this tells what proportion of subjects will NOT show the effect of each coefficient, 
      # given each coefficient random standard deviation and the sample size (# of subjects)
      if(subj.theor < max.subj)
      {
        if(sum(coef.test)>.05/length(beta.matrix))
        {
          subj.theor <- subj.theor+1
          cat('subj.theor:', subj.theor,'\n')
        } else
        {
          cat('Completed.\n\nOptimal # of subj:', subj.theor,'\n\ncoef.test: \n',
              'Probability of type I error associated to the estimation of the true parameters:\n')
          print(coef.test)
          break
        }
      } else
      {
        cat('Reached max subjects: ', subj.theor,'\nStopped.\n\ncoef.test: \n',
            'Probability of type I error associated to the estimation of the true parameters:\n')
        print(coef.test)
        break
      }
    }
  }
  
  if(find.reps)
  {
    # find the minimum # of repetitions per subject given the residuals stddev (response variability)
    repetitions.theor <- reps.start
    max.repetitions <- reps.max
    repeat{
      coef.test <- NULL
      coef.test.temp <- NULL
      for(i in 1:50)
      {
        rep.test.temp <- as.data.frame(t(replicate(repetitions.theor, rnorm(4, beta.fix, sigma))))
        names(rep.test.temp) <- names(beta.matrix)
        rep.test <- suppressMessages(melt(rep.test.temp))
        rep.test$obs <- factor(1:repetitions.theor)
        mod <- lme4::lmer(value ~ variable + (1|obs), data=rep.test)
        # p <- sum(round(fortify(summary(glht(mod,linfct=mcp(variable="Tukey"))))$p, 3))/((length(beta.matrix)*(length(beta.matrix)-1))/2)
        p <- mean(as.data.frame(Anova(mod, type=3))[,3])
        coef.test.temp <- c(coef.test.temp, p)
      }
      coef.test <- length(coef.test.temp[coef.test.temp>.05])/length(coef.test.temp)
      
      if(repetitions.theor < max.repetitions)
      {
        if(coef.test>.05)
        {
          repetitions.theor <- repetitions.theor+1
          cat('\nrepetitions.theor:', repetitions.theor)
        } else
        {
          cat('\nCompleted.\n\nOptimal # of repetitions:', repetitions.theor,'\n\ncoef.test: \n')
          # print(summary(glht(mod,linfct=mcp(variable="Tukey"))))
          print(Anova(mod, type=3))
          break
        }
      } else
      {
        cat('Reached max repetitions: ',repetitions.theor,'\nStopped.\n\ncoef.test: \n')
        # print(summary(glht(mod,linfct=mcp(variable="Tukey"))))
        print(Anova(mod, type=3))
        break
      }
    }
  }

  if(find.subj & !find.reps)
  {
    return(list("subj.theor" = subj.theor))
  }
  if(!find.subj & find.reps)
  {
    return(list("repetitions.theor" = repetitions.theor))
  }
  if(find.subj & find.reps)
  {
    return(list("subj.theor" = subj.theor, "repetitions.theor" = repetitions.theor))
  }
  
}