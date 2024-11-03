#' Calculate Psychometric Parameters from a Fitted Model
#'
#' @description
#' Calculates the Point of Subjective Equality (PSE) and Just Noticeable Difference (JND)
#' from a fitted psychometric model, along with their confidence intervals.
#'
#' @param model A fitted model object (either a standard linear model or an lme4 model)
#' @param alpha Numeric value specifying the significance level for confidence intervals (default = 0.05)
#' @param lme4 Logical value indicating whether the model is from lme4 package (default = FALSE)
#'
#' @return A 2x4 matrix containing:
#'   \item{pse}{Point of Subjective Equality estimate, standard error, and confidence interval}
#'   \item{jnd}{Just Noticeable Difference estimate, standard error, and confidence interval}
#'
#' @details
#' The function calculates:
#' * PSE = -α/β where α is the intercept and β is the slope
#' * JND = 1/β where β is the slope
#' * Standard errors and confidence intervals using the delta method
#'
#' The confidence intervals are calculated as:
#' * estimate ± (z_(1-α/2) * SE)
#' where z_(1-α/2) is the standard normal quantile
#'
#' @examples
#' # For a standard linear model
#' model <- lm(response ~ stimulus)
#' psychometric(model)
#'
#' # For an lme4 model
#' model <- lmer(response ~ stimulus + (1|subject))
#' psychometric(model, lme4 = TRUE)
#'
#' @export
psychometric <- function(model, alpha = 0.05, lme4 = F){

  if(lme4 == F){
    pse <- -model$coef[1]/model$coef[2]
    BETA <- model$coef[2]} else {
      #  	if extracting form mer, check lme4 version!
      fixed.par = getME(model, "beta")
      pse <- -(fixed.par[1]/fixed.par[2])
      BETA <- fixed.par[2]}

  #		if extracting from mer, check lme4 version!
  var.alpha <- vcov(model)[1,1]
  var.beta <- vcov(model)[2,2]
  cov.alpha.beta <- vcov(model)[2,1]

  var.pse <- (1/BETA^2)*(var.alpha + (2*pse*cov.alpha.beta)+(pse^2*var.beta))   #PSE
  inferior.pse <- pse - (qnorm(1 - (alpha/2))*sqrt(var.pse))
  superior.pse <- pse + (qnorm(1 - (alpha/2))*sqrt(var.pse))

  jnd <- 1/BETA
  var.jnd <- (-1/BETA^2)^2 * var.beta                           #JND
  inferior.jnd <- jnd - (qnorm(1 - (alpha/2))*sqrt(var.jnd))
  superior.jnd <- jnd + (qnorm(1 - (alpha/2))*sqrt(var.jnd))

  output <- matrix(rbind(c(pse, sqrt(var.pse), inferior.pse, superior.pse),
                         c(jnd, sqrt(var.jnd), inferior.jnd, superior.jnd)), nrow = 2,
                   dimnames = list(param <- c("pse", "jnd"), statistics <- c("Estimate",
                                                                             "Std. Error", "Inferior", "Superior")))

  return(output)}
