#' @export
uni <- function(ANOVA.object) {
  
  if(class(ANOVA.object)!="ANOVA"){
    stop("Object must be of class ANOVA")
  } else {
    output <- ANOVA.object$results$uni
    return(output)
  }
}

#' @export
multi <- function(ANOVA.object) {
  
  if(class(ANOVA.object)!="ANOVA"){
    stop("Object must be of class ANOVA")
  } else {
    output <- ANOVA.object$results$multi
    return(output)
  }
}