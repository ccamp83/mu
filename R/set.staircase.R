#' Set Up Staircase Parameters
#' 
#' @description
#' Creates a staircase configuration for psychophysical testing procedures. This function sets up
#' the parameters for an adaptive staircase method, including starting value and step sizes for
#' both upward and downward adjustments.
#' 
#' @param startValue Numeric value indicating the initial stimulus intensity level
#' @param down.steps Numeric value specifying the number of correct responses needed for a downward step
#' @param down.delta Numeric value specifying the size of downward steps
#' @param up.steps Numeric value specifying the number of incorrect responses needed for an upward step
#' @param up.delta Numeric value specifying the size of upward steps
#' @param direction Character string indicating the initial direction of the staircase ("up" or "down")
#' 
#' @return A list containing the staircase configuration with components:
#'   \item{startValue}{The initial stimulus intensity}
#'   \item{down.param}{Vector containing steps and delta for downward adjustments}
#'   \item{up.param}{Vector containing steps and delta for upward adjustments}
#'   \item{direction}{Initial direction of the staircase}
#'
#' @examples
#' # Create a basic staircase configuration
#' staircase <- set.staircase(
#'   startValue = 10,
#'   down.steps = 2,
#'   down.delta = 1,
#'   up.steps = 1,
#'   up.delta = 1,
#'   direction = "down"
#' )
#'
#' @export
set.staircase <- function(startValue, down.steps, down.delta, up.steps, up.delta, direction)
{
  output <- list(startValue = startValue,
                 down.param = c(steps = down.steps, delta = down.delta),
                 up.param = c(steps = up.steps, delta = up.delta),
                 direction = direction)
  return(output)
}
