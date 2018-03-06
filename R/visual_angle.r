#' @export
visual_angle <- function(x = NULL, z = NULL, theta = NULL)
{
  output <- NULL
  theta.rad <- theta * pi/180
  
  if(is.null(x) & !is.null(c(z,theta))){
    output <- z * 2 * tan(theta.rad/2)
    cat("Width is\n")
  } else if(is.null(z) & !is.null(c(x,theta.rad))){
    output <- x / 2 * tan(theta.rad/2)
    cat("Distance is\n")
  } else if(is.null(theta) & !is.null(c(x,z))){
    output.rad <- 2 * atan(x/(2*z))
    output <- output.rad * 180/pi
    cat("Angle is\n")
  } else
    stop("There are too many unspecified parameters.")
  
  return(output)
}