#' @export
kin.handTheta <- function(datatraj, lambda=.2, deg=T, ...) {
  lambda.glob <<- lambda
  
  if(deg){
    k <<- 180/pi
  } else {
    k <<- 1
  }
  
  datatraj <- ddply(datatraj, .(trialN), mutate,
                    index_theta = cc.derive(indexZ, indexX, l=lambda.glob, 1)*k,
                    index_thetaVel = cc.derive(indexZ, indexX, l=lambda.glob, 2),
                    thumb_theta = cc.derive(thumbZ, thumbX, l=lambda.glob, 1)*k,
                    thumb_thetaVel = cc.derive(thumbZ, thumbX, l=lambda.glob, 2),
                    GP_theta = cc.derive(GPZ, GPX, l=lambda.glob, 1)*k,
                    GP_thetaVel = cc.derive(GPZ, GPX, l=lambda.glob, 2))
  
  remove(lambda.glob, envir=.GlobalEnv)
  remove(k, envir=.GlobalEnv)
  
  return(datatraj)
}
