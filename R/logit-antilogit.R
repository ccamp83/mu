#' @export
logit <- function(x) log(x / (1-x))
#' @export
antilogit <- function(x) {1 / (1 + exp(-x))}
