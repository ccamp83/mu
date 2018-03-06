#' Calculate R-squared based on correlation between fitted values and observed values
#' @export
r2.lmer <- function(m) {
  lmfit <-  lm(model.response(model.frame(m)) ~ fitted(m))
  return(summary(lmfit)$r.squared)
}