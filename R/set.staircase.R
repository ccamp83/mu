#' @export
set.staircase <- function(startValue, down.steps, down.delta, up.steps, up.delta, direction)
{
  output <- list(startValue = startValue,
                 down.param = c(steps = down.steps, delta = down.delta),
                 up.param = c(steps = up.steps, delta = up.delta),
                 direction = direction)
  return(output)
}
