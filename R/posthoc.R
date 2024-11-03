#' Perform Post-hoc Analysis on ANOVA Results
#'
#' @description
#' This function performs post-hoc analysis on an ANOVA object using the phia package's
#' testInteractions functionality. It allows for detailed examination of interaction effects
#' and pairwise comparisons following an ANOVA analysis.
#'
#' @param ANOVA.object An object of class "ANOVA" containing the analysis results
#' @param ... Additional arguments passed to phia::testInteractions
#'
#' @return Returns the results of phia::testInteractions, which includes:
#'   \item{F-tests}{F statistics for the specified contrasts}
#'   \item{p-values}{Associated p-values for the contrasts}
#'   \item{effect sizes}{Effect sizes for the contrasts}
#'
#' @details
#' The function checks if the input object is of the correct class ("ANOVA") and then
#' performs post-hoc analysis using the model fit and interaction data stored in the
#' ANOVA object.
#'
#' @examples
#' # Assuming you have performed an ANOVA analysis:
#' my_anova <- ANOVA(data = mydata, dv = "score", formula = ~ group)
#'
#' # Perform post-hoc analysis
#' posthoc(my_anova)
#'
#' # With additional arguments
#' posthoc(my_anova, adjustment = "bonferroni")
#'
#' @export
posthoc <- function(ANOVA.object, ...) {
  if(class(ANOVA.object)!="ANOVA"){
    stop("Object must be of class ANOVA")
  } else {
    phia::testInteractions(model = ANOVA.object$fit,
                           idata = ANOVA.object$idata,
                           ...)
  }
}
