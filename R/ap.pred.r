#' Predict "approaching parameter" (as in Brenner and Smeets' model (1998))
#' @param MGA maximum grip aperture
#' @param r radius of the target object
#' @examples 
#' libraries()
#' (ap <- ap.pred(MGA=100.25, r=40))
#' @details
#' By definition, MGA must be >= r*2
#' @export
ap.pred <- function(MGA, r)
{
  if(MGA <= r*2)
    stop('MGA must be >= r*2')
  
  ap <- 0:5000
  MGA.cont <- (54*((20*r + ap)^4)*(15*r + 2*ap)) / (3125 * ((12*r + ap)^4))
  fit <- sreg(MGA.cont, ap)
  ap.pred <- predict(fit, MGA)
  
  return(ap.pred)
}

