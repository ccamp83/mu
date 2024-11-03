#' Generate Multivariate Normal Random Numbers with Limits
#'
#' @description
#' Generates random numbers from a multivariate normal distribution with specified
#' covariance matrix, constrained between minimum and maximum values. Any generated
#' values outside the limits are resampled until they fall within bounds.
#'
#' @param n Integer. Number of random vectors to generate
#' @param sigma Numeric matrix. The covariance matrix
#' @param min Numeric. Lower bound for generated values (default = -Inf)
#' @param max Numeric. Upper bound for generated values (default = Inf)
#'
#' @return A matrix of random numbers from the multivariate normal distribution,
#'         where all values fall between min and max
#'
#' @details
#' The function uses mvtnorm::rmvnorm() to generate the random numbers. If any
#' generated values fall outside the specified limits, those specific values are
#' resampled until they fall within bounds. This ensures the returned matrix
#' contains only values between min and max while maintaining the specified
#' covariance structure as much as possible.
#'
#' @examples
#' # Generate 100 random vectors with correlation 0.5
#' sigma <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
#' x <- rmvnormlim(100, sigma, min=-2, max=2)
#'
#'
#' @export
rmvnormlim <- function(n, sigma, min=-Inf, max=Inf)
{
  x <- mvtnorm::rmvnorm(n, sigma=sigma)

  # count the smaller numbers
  outliers <- length(x[x < min | x > max])
  if(outliers > 0)
  {
    while(outliers > 0)
    {
      smaller <- length(x[x < min | x > max])
      x[x < min | x > max] <- mvtnorm::rmvnorm(n, sigma=sigma)[x < min | x > max]
    }
  }
  return(x)
}
