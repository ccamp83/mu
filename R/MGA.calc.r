#' Calculate maximum grip aperture accordind to Brenner and Smeets' model (1998)
#' @param ap approaching parameter, as in Brenner & Smeets (1998)
#' @param r radius of the target object
#' @examples 
#' # single calculation
#' (MGA <- MGA.calc(ap=1500, r=40))
#' 
#' # it is possible to explore the range of values of ap 
#' # (given a certain radius) as a function of MGA
#' libraries()
#' app.par <- 100:1000
#' rad <- 20
#' MGA <- MGA.calc(ap=app.par, r=rad)
#' qplot(MGA, app.par)
#' @export
MGA.calc <- function(ap, r)
{
  MGA <- (54*((20*r + ap)^4)*(15*r + 2*ap)) / (3125 * ((12*r + ap)^4))
  
  return(MGA)
}