#' Generate grip aperture profiles accordind to Brenner and Smeets' model (1998)
#' @param ap approaching parameter, as in Brenner & Smeets (1998)
#' @param MT movement time (in millisec)
#' @param r radius of the target object
#' @examples 
#' libraries()
#' mov_time <- 1500
#' GA <- grip.aperture(ap=1500, MT=mov_time, r=40)
#' time <- 1:mov_time
#' qplot(time, GA)
#' @export
grip.aperture <- function(ap, MT, r)
{
  # Time vector
  t <- 1:MT
  
  # Normalized time vector
  tr <- t/MT

  GA <- ( ap*(tr - 1)^2 + 2*r*(6*(tr^2) - 15*tr + 10) ) * tr^3

  return(GA)
}
