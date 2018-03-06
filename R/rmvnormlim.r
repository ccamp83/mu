#' @export
rmvnormlim <- function(n, sigma, min=-Inf, max=Inf)
{
  x <- rmvnorm(n, sigma=sigma)
  
  # count the smaller numbers
  outliers <- length(x[x < min | x > max])
  if(outliers > 0)
  {
    while(outliers > 0)
    {
      smaller <- length(x[x < min | x > max])
      x[x < min | x > max] <- rmvnorm(n, sigma=sigma)[x < min | x > max]
    }
  }  
  return(x)
}
