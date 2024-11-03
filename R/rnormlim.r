#' Generate Random Normal Numbers with Limits
#' 
#' @description
#' Generates random numbers from a normal distribution with specified mean and 
#' standard deviation, constrained between minimum and maximum values. Any values 
#' outside the specified range are resampled until they fall within the limits.
#' 
#' @param n Number of observations to generate
#' @param mu Mean of the normal distribution (default = 0)
#' @param sigma Standard deviation of the normal distribution (default = 1)
#' @param min Minimum allowed value (default = -Inf)
#' @param max Maximum allowed value (default = Inf)
#' 
#' @return A numeric vector of length n containing random normal values 
#' constrained between min and max
#'
#' @examples
#' # Generate 1000 standard normal values between -2 and 2
#' x <- rnormlim(1000, mu = 0, sigma = 1, min = -2, max = 2)
#' 
#' # Generate 500 normal values with mean 10, sd 2, minimum 5
#' y <- rnormlim(500, mu = 10, sigma = 2, min = 5)
#'
#' @export
rnormlim <- function(n, mu=0, sigma=1, min=-Inf, max=Inf)
{
  x <- rnorm(n, mu, sigma)
  
  # count the smaller numbers
  outliers <- length(x[x < min | x > max])
  if(outliers > 0)
  {
    while(outliers > 0)
    {
      outliers <- length(x[x < min | x > max])
      x[x < min | x > max] <- rnorm(outliers, mu, sigma)
    }
  }  
  return(x)
}
