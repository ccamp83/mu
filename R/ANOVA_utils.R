#' Extract Univariate Results from ANOVA Object
#'
#' @description
#' Extracts the univariate test results from an ANOVA object.
#'
#' @param ANOVA.object An object of class "ANOVA" containing analysis results
#'
#' @return A data frame containing univariate test results including F-values, 
#' degrees of freedom, p-values, and effect sizes
#'
#' @examples
#' model <- ANOVA(data = mydata, dv = "score", formula = ~ group)
#' uni(model)
#'
#' @export
uni <- function(ANOVA.object) {
  if(class(ANOVA.object)!="ANOVA"){
    stop("Object must be of class ANOVA")
  } else {
    output <- ANOVA.object$results$uni
    return(output)
  }
}

#' Extract Multivariate Results from ANOVA Object
#'
#' @description
#' Extracts the multivariate test results from an ANOVA object.
#'
#' @param ANOVA.object An object of class "ANOVA" containing analysis results
#'
#' @return A data frame containing multivariate test results including Pillai's Trace,
#' Wilks' Lambda, Hotelling's Trace, and Roy's Largest Root, along with their
#' respective F-statistics and p-values
#'
#' @examples
#' model <- ANOVA(data = mydata, dv = "score", formula = ~ group)
#' multi(model)
#'
#' @export
multi <- function(ANOVA.object) {
  if(class(ANOVA.object)!="ANOVA"){
    stop("Object must be of class ANOVA")
  } else {
    output <- ANOVA.object$results$multi
    return(output)
  }
}