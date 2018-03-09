# Method to perform post-hoc analysis on an ANOVA object
#' @export
posthoc <- function(ANOVA.object, ...) {
  if(class(ANOVA.object)!="ANOVA"){
    stop("Object must be of class ANOVA")
  } else {
    phia::testInteractions(model = ANOVA.object$fit, idata = ANOVA.object$idata)
  }
}
