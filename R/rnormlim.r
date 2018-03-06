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
