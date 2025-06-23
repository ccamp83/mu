# Helper: Efficiently partition integer S into K integers in [minLikert, maxLikert]
# Returns a random vector of length K in [minLikert, maxLikert] that sums to S, or NA if impossible.
partition_score <- function(S, K, minLikert, maxLikert) {
  S_shifted <- S - K * minLikert
  max_shifted <- maxLikert - minLikert
  if (S_shifted < 0 || S_shifted > K * max_shifted) return(rep(NA, K))
  
  # Start with a multinomial draw
  parts <- as.vector(rmultinom(1, S_shifted, rep(1, K)))
  
  # If all parts are within bounds, return
  if (all(parts <= max_shifted)) return(parts + minLikert)
  
  # Otherwise, clip and redistribute excess
  for (i in 1:K) {
    if (parts[i] > max_shifted) {
      excess <- parts[i] - max_shifted
      parts[i] <- max_shifted
      # Redistribute excess to other items
      idx <- which(parts < max_shifted)
      if (length(idx) > 0) {
        add <- as.vector(rmultinom(1, excess, rep(1, length(idx))))
        parts[idx] <- parts[idx] + add
      }
    }
  }
  # If still not valid, fallback to NA
  if (any(parts > max_shifted)) return(rep(NA, K))
  return(parts + minLikert)
}

#' Simulate item-level questionnaire data with specified total score distribution
#'
#' @param N Number of participants
#' @param n_items Number of items in the questionnaire
#' @param likert Vector of possible Likert scale values (e.g., 1:5)
#' @param total_mean Mean of the desired total score distribution
#' @param total_sd Standard deviation of the desired total score distribution
#' @param total_min Minimum possible total score
#' @param total_max Maximum possible total score
#' @return Data frame with item-level responses and total scores
#' @examples
#' set.seed(123)
#' simdat <- sim.questionnaire(N = 100, n_items = 10, likert = 1:5, total_mean = 39, total_sd = 5, total_min = 10, total_max = 50)
#' summary(simdat$total)
#' head(simdat)
#' @export
sim.questionnaire <- function(N = 100, 
                             n_items = 10, 
                             likert = 1:5, 
                             total_mean = 39, 
                             total_sd = 5, 
                             total_min = 10, 
                             total_max = 50) {
  # Calculate the mathematically possible range
  possible_min <- n_items * min(likert)
  possible_max <- n_items * max(likert)
  
  # Adjust total_min and total_max if they're outside the possible range
  if (total_min < possible_min) {
    warning(sprintf("total_min (%d) is below the minimum possible total (%d). Setting to %d.", 
                   total_min, possible_min, possible_min))
    total_min <- possible_min
  }
  if (total_max > possible_max) {
    warning(sprintf("total_max (%d) is above the maximum possible total (%d). Setting to %d.", 
                   total_max, possible_max, possible_max))
    total_max <- possible_max
  }
  
  # Generate participants until we have N valid ones
  valid_participants <- 0
  all_item_responses <- NULL
  all_total_scores <- NULL
  
  while (valid_participants < N) {
    # Generate a batch of potential total scores
    batch_size <- min(N - valid_participants + 10, 100)  # Generate extra to account for invalid ones
    total_scores <- round(rnorm(batch_size, mean = total_mean, sd = total_sd))
    total_scores <- pmin(pmax(total_scores, total_min), total_max)
    
    # Try to partition each total score
    for (i in 1:length(total_scores)) {
      item_responses <- partition_score(total_scores[i], n_items, min(likert), max(likert))
      
      # If partitioning succeeded (no NAs), keep this participant
      if (!any(is.na(item_responses))) {
        all_item_responses <- rbind(all_item_responses, item_responses)
        all_total_scores <- c(all_total_scores, total_scores[i])
        valid_participants <- valid_participants + 1
        
        if (valid_participants >= N) break
      }
    }
  }
  
  # Assemble data frame
  df <- as.data.frame(all_item_responses)
  names(df) <- paste0("item", 1:n_items)
  df$total <- all_total_scores
  df$subj <- 1:N
  return(df)
}

#' Simulate multiple questionnaires with custom linear model relationships
#'
#' @param N Number of participants
#' @param questionnaires List of questionnaire specifications. Each element should be a list with:
#'   - n_items: number of items
#'   - likert: vector of possible Likert scale values
#'   - total_mean: mean of total score distribution
#'   - total_sd: standard deviation of total score distribution
#'   - total_min: minimum possible total score (optional, will be calculated if not provided)
#'   - total_max: maximum possible total score (optional, will be calculated if not provided)
#' @param linear_model Optional linear model specification. A list with:
#'   - formula: R formula string (e.g., "S2 ~ S1", "S3 ~ S1 + S2 + S1:S2")
#'   - coefficients: named vector of coefficients (intercept, slopes, interactions)
#'   - residual_sd: standard deviation of residuals
#' @param correlations Optional correlation matrix between questionnaire total scores
#' @param seed Random seed for reproducibility
#' @param debug If TRUE, returns additional diagnostic information about the linear model
#' @return Data frame with item-level responses and total scores for all questionnaires
#' @examples
#' # Two questionnaires with linear relationship
#' set.seed(123)
#' simdat <- sim.questionnaire.multi(
#'   N = 100,
#'   questionnaires = list(
#'     S1 = list(n_items = 10, likert = 1:5, total_mean = 35, total_sd = 5),
#'     S2 = list(n_items = 8, likert = 0:4, total_mean = 20, total_sd = 4)
#'   ),
#'   linear_model = list(
#'     formula = "S2 ~ S1",
#'     coefficients = c(intercept = 5, S1 = 0.3),
#'     residual_sd = 2
#'   )
#' )
#' 
#' # Three questionnaires with interactions
#' simdat <- sim.questionnaire.multi(
#'   N = 200,
#'   questionnaires = list(
#'     Anxiety = list(n_items = 10, likert = 1:5, total_mean = 30, total_sd = 6),
#'     Depression = list(n_items = 12, likert = 1:5, total_mean = 28, total_sd = 7),
#'     Stress = list(n_items = 8, likert = 0:4, total_mean = 15, total_sd = 4)
#'   ),
#'   linear_model = list(
#'     formula = "Stress ~ Anxiety + Depression + Anxiety*Depression",
#'     coefficients = c(intercept = 2, Anxiety = 0.2, Depression = 0.3, "Anxiety:Depression" = 0.01),
#'     residual_sd = 1.5
#'   )
#' )
#' @export
sim.questionnaire.multi <- function(N = 100, 
                                   questionnaires, 
                                   linear_model = NULL, 
                                   correlations = NULL,
                                   seed = NULL,
                                   debug = FALSE) {
  
  if (!is.null(seed)) set.seed(seed)
  
  # Validate inputs
  if (!is.list(questionnaires) || length(questionnaires) == 0) {
    stop("questionnaires must be a non-empty list")
  }
  
  n_questionnaires <- length(questionnaires)
  questionnaire_names <- names(questionnaires)
  if (is.null(questionnaire_names)) {
    questionnaire_names <- paste0("S", 1:n_questionnaires)
    names(questionnaires) <- questionnaire_names
  }
  
  # Calculate possible ranges for each questionnaire
  for (i in 1:n_questionnaires) {
    q <- questionnaires[[i]]
    possible_min <- q$n_items * min(q$likert)
    possible_max <- q$n_items * max(q$likert)
    
    if (is.null(q$total_min)) q$total_min <- possible_min
    if (is.null(q$total_max)) q$total_max <- possible_max
    
    if (q$total_min < possible_min) {
      warning(sprintf("total_min for %s (%d) is below minimum possible (%d). Setting to %d.", 
                     questionnaire_names[i], q$total_min, possible_min, possible_min))
      q$total_min <- possible_min
    }
    if (q$total_max > possible_max) {
      warning(sprintf("total_max for %s (%d) is above maximum possible (%d). Setting to %d.", 
                     questionnaire_names[i], q$total_max, possible_max, possible_max))
      q$total_max <- possible_max
    }
    
    questionnaires[[i]] <- q
  }
  
  # Generate total scores based on linear model or correlations
  if (!is.null(linear_model)) {
    total_scores <- generate_scores_with_linear_model_v2(N, questionnaires, linear_model)
  } else if (!is.null(correlations)) {
    total_scores <- generate_scores_with_correlations(N, questionnaires, correlations)
  } else {
    # Independent questionnaires
    total_scores <- matrix(NA, nrow = N, ncol = n_questionnaires)
    for (i in 1:n_questionnaires) {
      q <- questionnaires[[i]]
      total_scores[, i] <- generate_independent_scores(N, q)
    }
  }
  
  # Generate item-level responses for each questionnaire
  all_data <- data.frame(subj = 1:N)
  
  for (i in 1:n_questionnaires) {
    q <- questionnaires[[i]]
    q_name <- questionnaire_names[i]
    
    # Generate item responses for this questionnaire
    item_responses <- matrix(NA, nrow = N, ncol = q$n_items)
    for (j in 1:N) {
      item_responses[j, ] <- partition_score(total_scores[j, i], q$n_items, min(q$likert), max(q$likert))
    }
    
    # Add to data frame
    item_df <- as.data.frame(item_responses)
    names(item_df) <- paste0(q_name, "_item", 1:q$n_items)
    all_data <- cbind(all_data, item_df)
    
    # Add total score
    all_data[[paste0(q_name, "_total")]] <- total_scores[, i]
  }
  
  if (debug) {
    # Additional diagnostic information about the linear model
    diagnostic_info <- list(
      total_scores = total_scores,
      questionnaires = questionnaires,
      linear_model = linear_model,
      correlations = correlations
    )
    all_data$diagnostic_info <- I(list(diagnostic_info))
  }
  
  return(all_data)
}

# Helper function to generate independent scores
generate_independent_scores <- function(N, questionnaire) {
  scores <- round(rnorm(N, mean = questionnaire$total_mean, sd = questionnaire$total_sd))
  scores <- pmin(pmax(scores, questionnaire$total_min), questionnaire$total_max)
  return(scores)
}

# Helper function to generate scores with linear model (new approach)
generate_scores_with_linear_model_v2 <- function(N, questionnaires, linear_model) {
  n_questionnaires <- length(questionnaires)
  total_scores <- matrix(NA, nrow = N, ncol = n_questionnaires)
  
  # Parse formula to determine dependencies
  formula_str <- linear_model$formula
  response_var <- gsub("\\s*~.*", "", formula_str)
  predictor_vars <- gsub(".*~\\s*", "", formula_str)
  
  # Find which questionnaire is the response variable
  response_idx <- which(names(questionnaires) == response_var)
  if (length(response_idx) == 0) {
    stop(sprintf("Response variable '%s' not found in questionnaire names", response_var))
  }
  
  # Parse predictor variables
  predictor_vars_split <- strsplit(predictor_vars, "\\s*\\+\\s*")[[1]]
  
  # Validate all variables exist
  all_vars <- unique(unlist(lapply(predictor_vars_split, function(var) {
    if (grepl("\\*", var)) {
      strsplit(var, "\\s*\\*\\s*")[[1]]
    } else {
      var
    }
  })))
  
  for (var in all_vars) {
    if (!var %in% names(questionnaires)) {
      stop(sprintf("Variable '%s' not found in questionnaire names", var))
    }
  }
  
  # Generate independent scores for all predictor variables first
  for (i in 1:n_questionnaires) {
    if (i != response_idx) {
      total_scores[, i] <- generate_independent_scores(N, questionnaires[[i]])
    }
  }
  
  # Create a data frame with predictor variables for model.matrix
  pred_data <- as.data.frame(total_scores[, -response_idx, drop = FALSE])
  names(pred_data) <- names(questionnaires)[-response_idx]
  
  # Create the formula for model.matrix
  formula_obj <- as.formula(paste("~", predictor_vars))
  
  # Create design matrix using model.matrix (like sim.experiment)
  design_matrix <- model.matrix(formula_obj, data = pred_data)
  
  # Get coefficients in the correct order
  coefs <- linear_model$coefficients
  residual_sd <- linear_model$residual_sd
  
  # Convert "intercept" to "(Intercept)" if present (for user convenience)
  if ("intercept" %in% names(coefs)) {
    names(coefs)[names(coefs) == "intercept"] <- "(Intercept)"
  }
  
  # Ensure we have all required coefficients
  required_coefs <- colnames(design_matrix)
  if (!all(required_coefs %in% names(coefs))) {
    missing_coefs <- setdiff(required_coefs, names(coefs))
    stop(sprintf("Missing coefficients for: %s", paste(missing_coefs, collapse = ", ")))
  }
  
  # Generate predicted values using the design matrix approach (like sim.experiment)
  predicted <- design_matrix %*% coefs[required_coefs]
  
  # Add residuals
  residuals <- rnorm(N, 0, residual_sd)
  response_scores <- predicted + residuals
  
  # Ensure scores are within bounds
  response_q <- questionnaires[[response_idx]]
  response_scores <- pmin(pmax(response_scores, response_q$total_min), response_q$total_max)
  
  # Now round to integers
  response_scores <- round(response_scores)
  
  # Final bounds check after rounding
  response_scores <- pmin(pmax(response_scores, response_q$total_min), response_q$total_max)
  
  total_scores[, response_idx] <- response_scores
  
  return(total_scores)
}

# Helper function to generate scores with correlation matrix
generate_scores_with_correlations <- function(N, questionnaires, correlations) {
  n_questionnaires <- length(questionnaires)
  
  # Validate correlation matrix
  if (!is.matrix(correlations) || nrow(correlations) != ncol(correlations) || 
      nrow(correlations) != n_questionnaires) {
    stop("correlations must be a square matrix with dimensions matching number of questionnaires")
  }
  
  # Generate multivariate normal scores
  means <- sapply(questionnaires, function(q) q$total_mean)
  sds <- sapply(questionnaires, function(q) q$total_sd)
  
  # Convert correlation matrix to covariance matrix
  cov_matrix <- correlations * outer(sds, sds)
  
  # Generate multivariate normal data
  library(MASS)
  mv_scores <- mvrnorm(N, mu = means, Sigma = cov_matrix)
  
  # Round and constrain to bounds
  total_scores <- matrix(NA, nrow = N, ncol = n_questionnaires)
  for (i in 1:n_questionnaires) {
    scores <- round(mv_scores[, i])
    scores <- pmin(pmax(scores, questionnaires[[i]]$total_min), questionnaires[[i]]$total_max)
    total_scores[, i] <- scores
  }
  
  return(total_scores)
} 