# Partial eta squared calculation from F statistic and its degrees of freedom
#' @export
etaSqP <- function(Fval, dfEffect, dfError) { (Fval * dfEffect) / ((Fval * dfEffect) + dfError) }
