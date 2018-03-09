# Cohen's w effect size calculation given Chi-square statistic and number of observations
#' @export
cohen.w <- function (chi_sq, n) { sqrt(chi_sq/n) }