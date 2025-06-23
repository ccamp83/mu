# Helper: Efficiently partition integer S into K integers in [minLikert, maxLikert]
# Returns a random vector of length K in [minLikert, maxLikert] that sums to S, or NA if impossible.
# This revised function guarantees a valid partition if S is within the theoretically possible range.
partition_score <- function(S, K, minLikert, maxLikert) {
  # Calculate the required sum of "shifted" scores (scores if minLikert was 0)
  S_shifted <- S - K * minLikert
  # Calculate the maximum possible "shifted" score for a single item
  max_shifted_per_item <- maxLikert - minLikert

  # Check if the total sum S is mathematically possible to achieve
  # S_shifted < 0 means S is less than the absolute minimum sum (K * minLikert)
  # S_shifted > K * max_shifted_per_item means S is greater than the absolute maximum sum (K * maxLikert)
  if (S_shifted < 0 || S_shifted > K * max_shifted_per_item) {
    return(rep(NA, K)) # Return NA if impossible to partition
  }

  # Initialize all parts to 0 (representing the "shifted" scores relative to minLikert)
  parts <- rep(0, K)

  # Distribute S_shifted points randomly among K items, ensuring individual limits are respected.
  # This iterative approach guarantees a valid partition if the total sum is possible.
  for (p in 1:S_shifted) {
    # Find indices of items that can still receive points (i.e., not yet at their max_shifted_per_item)
    eligible_indices <- which(parts < max_shifted_per_item)

    # This check should ideally not be hit if the initial sum check passes,
    # as it means there's no more capacity to distribute points, contradicting
    # the fact that S_shifted was within bounds. It acts as a robust fallback.
    if (length(eligible_indices) == 0) {
      return(rep(NA, K))
    }

    # Randomly select one eligible item to increment
    selected_idx <- sample(eligible_indices, 1)

    # Increment the selected item's shifted score by 1
    parts[selected_idx] <- parts[selected_idx] + 1
  }

  # Add back the minLikert value to each part to get the actual Likert scores
  return(parts + minLikert)
}

# NEW HELPER: Adjusts total_min/max based on mathematically possible ranges
#' Adjusts provided total min/max to be within mathematically possible bounds for a questionnaire.
#'
#' @param total_min User-specified minimum total score.
#' @param total_max User-specified maximum total score.
#' @param n_items Number of items in the questionnaire.
#' @param likert Vector of possible Likert scale values.
#' @param q_name (Optional) Name of the questionnaire for warnings.
#' @return A list containing the adjusted total_min and total_max.
adjust_total_score_bounds <- function(total_min, total_max, n_items, likert, q_name = NULL) {
  possible_min <- n_items * min(likert)
  possible_max <- n_items * max(likert)

  if (total_min < possible_min) {
    warning_msg <- sprintf("total_min%s (%d) is below the minimum possible total (%d). Setting to %d.",
                           ifelse(is.null(q_name), "", paste0(" for ", q_name)),
                           total_min, possible_min, possible_min)
    warning(warning_msg)
    total_min <- possible_min
  }
  if (total_max > possible_max) {
    warning_msg <- sprintf("total_max%s (%d) is above the maximum possible total (%d). Setting to %d.",
                           ifelse(is.null(q_name), "", paste0(" for ", q_name)),
                           total_max, possible_max, possible_max)
    warning(warning_msg)
    total_max <- possible_max
  }
  return(list(total_min = total_min, total_max = total_max))
}

# NEW HELPER: Rounds and constrains scores to specified min/max
#' Rounds scores to integers and constrains them within specified min and max bounds.
#'
#' @param scores A numeric vector of scores.
#' @param min_val Minimum allowable score.
#' @param max_val Maximum allowable score.
#' @return A numeric vector of rounded and bounded scores.
round_and_bound_scores <- function(scores, min_val, max_val) {
  scores <- round(scores)
  scores <- pmin(pmax(scores, min_val), max_val)
  return(scores)
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

  # Adjust total_min and total_max based on possible range using new helper
  adjusted_bounds <- adjust_total_score_bounds(total_min, total_max, n_items, likert)
  total_min <- adjusted_bounds$total_min
  total_max <- adjusted_bounds$total_max

  # Generate participants until we have N valid ones
  valid_participants <- 0
  all_item_responses <- NULL
  all_total_scores <- NULL

  while (valid_participants < N) {
    # Generate a batch of potential total scores
    batch_size <- min(N - valid_participants + 10, 100) # Generate extra to account for invalid ones
    total_scores_batch <- rnorm(batch_size, mean = total_mean, sd = total_sd)
    total_scores_batch <- round_and_bound_scores(total_scores_batch, total_min, total_max) # Use new helper

    # Try to partition each total score from the batch
    for (i in 1:length(total_scores_batch)) {
      item_responses <- partition_score(total_scores_batch[i], n_items, min(likert), max(likert))

      # If partitioning succeeded (no NAs), keep this participant
      if (!any(is.na(item_responses))) {
        all_item_responses <- rbind(all_item_responses, item_responses)
        all_total_scores <- c(all_total_scores, total_scores_batch[i])
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
#'    - n_items: number of items
#'    - likert: vector of possible Likert scale values
#'    - total_mean: mean of total score distribution
#'    - total_sd: standard deviation of desired total score distribution
#'    - total_min: minimum possible total score (optional, will be calculated if not provided)
#'    - total_max: maximum possible total score (optional, will be calculated if not provided)
#' @param linear_model Optional linear model specification. A list with:
#'    - formula: R formula string (e.g., "S2 ~ S1", "S3 ~ S1 + S2 + S1:S2")
#'    - coefficients: (Optional) named vector of coefficients (intercept, slopes, interactions). If not provided,
#'                    will be estimated based on `desired_coefficient_signs` and scale characteristics.
#'    - residual_sd: (Optional) standard deviation of residuals. If not provided, will be estimated.
#'    - desired_coefficient_signs: (Optional) A named list of 1 or -1, specifying desired signs.
#' @param correlations Optional correlation matrix between questionnaire total scores
#' @param seed Random seed for reproducibility
#' @param debug If TRUE, returns additional diagnostic information about the linear model
#' @return Data frame with item-level responses and total scores for all questionnaires
#' @examples
#' # Example 1: Two questionnaires with linear relationship (manual coefficients)
#' set.seed(123)
#' simdat_manual <- sim.questionnaire.multi(
#'    N = 100,
#'    questionnaires = list(
#'      S1 = list(n_items = 10, likert = 1:5, total_mean = 35, total_sd = 5),
#'      S2 = list(n_items = 8, likert = 0:4, total_mean = 20, total_sd = 4)
#'    ),
#'    linear_model = list(
#'      formula = "S2 ~ S1",
#'      coefficients = c(intercept = 5, S1 = 0.3),
#'      residual_sd = 2
#'    )
#' )
#' summary(lm(S2_total ~ S1_total, data = simdat_manual))
#'
#' # Example 2: Three questionnaires with interactions (auto-estimated coefficients with signs)
#' set.seed(456)
#' simdat_auto <- sim.questionnaire.multi(
#'    N = 200,
#'    questionnaires = list(
#'      Anxiety = list(n_items = 10, likert = 1:5, total_mean = 30, total_sd = 6),
#'      Depression = list(n_items = 12, likert = 1:5, total_mean = 28, total_sd = 7),
#'      Stress = list(n_items = 8, likert = 1:7, total_mean = 15, total_sd = 4)
#'    ),
#'    linear_model = list(
#'      formula = "Stress ~ Anxiety + Depression + Anxiety*Depression",
#'      desired_coefficient_signs = list(Anxiety = 1, Depression = -1, "Anxiety:Depression" = -1)
#'    )
#' )
#' summary(lm(Stress_total ~ Anxiety_total + Depression_total + Anxiety_total:Depression_total, data = simdat_auto))
#'
#' # Example 3: Simulating with correlations instead of a linear model
#' set.seed(789)
#' simdat_corr <- sim.questionnaire.multi(
#'   N = 100,
#'   questionnaires = list(
#'     Q1 = list(n_items = 5, likert = 1:5, total_mean = 15, total_sd = 3),
#'     Q2 = list(n_items = 7, likert = 1:4, total_mean = 20, total_sd = 4)
#'   ),
#'   correlations = matrix(c(
#'     1.0, 0.6,
#'     0.6, 1.0
#'   ), nrow = 2, byrow = TRUE)
#' )
#' cor(simdat_corr$Q1_total, simdat_corr$Q2_total)
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

  # Calculate and adjust possible ranges for each questionnaire using new helper
  for (i in 1:n_questionnaires) {
    q <- questionnaires[[i]]

    # Ensure total_min/max are initialized if not provided, before adjustment
    if (is.null(q$total_min)) q$total_min <- q$n_items * min(q$likert)
    if (is.null(q$total_max)) q$total_max <- q$n_items * max(q$likert)

    adjusted_bounds <- adjust_total_score_bounds(q$total_min, q$total_max,
                                                 q$n_items, q$likert,
                                                 questionnaire_names[i])
    q$total_min <- adjusted_bounds$total_min
    q$total_max <- adjusted_bounds$total_max

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
      # The revised partition_score function guarantees a valid partition if total_scores[j, i] is possible
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
  scores <- rnorm(N, mean = questionnaire$total_mean, sd = questionnaire$total_sd)
  scores <- round_and_bound_scores(scores, questionnaire$total_min, questionnaire$total_max) # Use new helper
  return(scores)
}

# Helper function to generate scores with linear model (new approach)
generate_scores_with_linear_model_v2 <- function(N, questionnaires, linear_model) {
  n_questionnaires <- length(questionnaires)

  # 1. Generate ALL questionnaire scores as continuous, unbounded, normally distributed values initially
  latent_scores_matrix <- matrix(NA, nrow = N, ncol = n_questionnaires)
  colnames(latent_scores_matrix) <- names(questionnaires)

  for (i in 1:n_questionnaires) {
    q <- questionnaires[[i]]
    # Generate continuous, unbounded scores based on mean/sd
    latent_scores_matrix[, i] <- rnorm(N, mean = q$total_mean, sd = q$total_sd)
  }

  # 2. Identify response and predictor variables for the linear model
  formula_str <- linear_model$formula
  response_var <- gsub("\\s*~.*", "", formula_str)
  predictor_vars_str <- gsub(".*~\\s*", "", formula_str)

  response_idx <- which(colnames(latent_scores_matrix) == response_var)
  if (length(response_idx) == 0) {
    stop(sprintf("Response variable '%s' not found in questionnaire names", response_var))
  }

  # Extract unique predictor variable names from the formula string (handling interaction terms)
  # This regex splits by '+' or '*' and then removes empty strings
  predictor_names_in_formula <- unique(
    trimws(
      grep(
        "\\S+", # Matches one or more non-whitespace characters
        unlist(strsplit(predictor_vars_str, "[+*]")),
        value = TRUE
      )
    )
  )

  # Validate all variables exist in questionnaires list
  for (var_name in predictor_names_in_formula) {
    if (!var_name %in% names(questionnaires)) {
      stop(sprintf("Variable '%s' used in formula but not found in questionnaire names.", var_name))
    }
  }

  # Dynamically determine coefficients and residual_sd if not explicitly provided
  coefs <- linear_model$coefficients
  residual_sd <- linear_model$residual_sd

  if (is.null(coefs) || is.null(residual_sd)) {
    message("Coefficients or residual_sd not explicitly provided. Estimating guidelines internally...")
    # These parameters are internal to this function and not exposed to the user
    coverage_sd <- 2
    max_main_effect_contribution_ratio <- 0.3
    interaction_effect_scaling_factor <- 0.005

    # --- Start of logic previously in estimate_lm_coefficient_guidelines ---
    response_q_spec <- questionnaires[[response_var]]
    response_min_val <- response_q_spec$total_min
    response_max_val <- response_q_spec$total_max
    response_range <- response_max_val - response_min_val
    response_mean <- response_q_spec$total_mean
    response_sd <- response_q_spec$total_sd

    # Predictor characteristics and estimated ranges (using predictor_names_in_formula)
    predictor_info <- list()
    for (pred_name in predictor_names_in_formula) { # Use predictor_names_in_formula here
      pq <- questionnaires[[pred_name]]

      pred_mean <- pq$total_mean
      pred_sd <- pq$total_sd
      pred_effective_min <- pq$total_min # already adjusted earlier in sim.questionnaire.multi
      pred_effective_max <- pq$total_max # already adjusted earlier in sim.questionnaire.multi

      pred_min_est <- max(pred_mean - coverage_sd * pred_sd, pred_effective_min)
      pred_max_est <- min(pred_mean + coverage_sd * pred_sd, pred_effective_max)
      pred_range_est <- pred_max_est - pred_min_est

      predictor_info[[pred_name]] <- list(
        mean = pred_mean,
        sd = pred_sd,
        min_est = pred_min_est,
        max_est = pred_max_est,
        range_est = pred_range_est
      )
    }

    # Intercept estimate
    coefs["(Intercept)"] <- response_mean

    # Main effects (slopes)
    for (pred_name in predictor_names_in_formula) { # Use predictor_names_in_formula here
      pi <- predictor_info[[pred_name]]
      # Get desired sign, default to positive if not specified
      sign_val <- ifelse(!is.null(linear_model$desired_coefficient_signs[[pred_name]]), linear_model$desired_coefficient_signs[[pred_name]], 1)

      if (pi$range_est > 0) {
        max_abs_slope <- (response_range * max_main_effect_contribution_ratio) / pi$range_est
        coefs[pred_name] <- max_abs_slope * sign_val
      } else {
        coefs[pred_name] <- 0
      }
    }

    # Interaction terms
    interaction_terms_in_formula <- attr(terms(as.formula(formula_str)), "term.labels")
    interaction_terms_in_formula <- grep(":", interaction_terms_in_formula, value = TRUE)

    if (length(interaction_terms_in_formula) > 0) {
      for (int_term in interaction_terms_in_formula) {
        int_vars <- strsplit(int_term, ":")[[1]]

        product_of_ranges <- 1
        valid_interaction_component <- TRUE
        for (var_name in int_vars) {
          trimmed_var_name <- trimws(var_name)
          if (trimmed_var_name %in% names(predictor_info)) {
            product_of_ranges <- product_of_ranges * predictor_info[[trimmed_var_name]]$range_est
          } else {
            warning(sprintf("Interaction term '%s' involves variable '%s' not found in predictor info.", int_term, trimmed_var_name))
            valid_interaction_component <- FALSE
            break
          }
        }

        sign_val <- ifelse(!is.null(linear_model$desired_coefficient_signs[[int_term]]), linear_model$desired_coefficient_signs[[int_term]], 1)

        if (valid_interaction_component && product_of_ranges > 0) {
          max_abs_interaction_slope <- (response_range * interaction_effect_scaling_factor) / product_of_ranges
          coefs[int_term] <- max_abs_interaction_slope * sign_val
        } else {
          coefs[int_term] <- 0
        }
      }
    }

    # Residual SD estimate
    residual_sd <- response_sd * 0.5 # A common heuristic: residual SD is a fraction of response SD

    # --- End of logic previously in estimate_lm_coefficient_guidelines ---

    # Message for debug purposes
    message("Automatically estimated coefficients: ", paste(names(coefs), round(coefs, 3), sep="=", collapse=", "))
    message("Automatically estimated residual_sd: ", round(residual_sd, 3))
  }


  # Create a data frame for model.matrix using the *latent* predictor scores
  # This ensures that interactions are calculated from continuous inputs
  pred_data_latent <- as.data.frame(latent_scores_matrix[, predictor_names_in_formula, drop = FALSE])
  colnames(pred_data_latent) <- predictor_names_in_formula # Ensure names match formula

  # Create the formula object for model.matrix
  formula_obj <- as.formula(paste("~", predictor_vars_str))

  # Create design matrix using model.matrix on the *latent* predictor data
  design_matrix <- model.matrix(formula_obj, data = pred_data_latent)

  # Convert "intercept" to "(Intercept)" if present (for user convenience)
  if ("intercept" %in% names(coefs)) {
    names(coefs)[names(coefs) == "intercept"] <- "(Intercept)"
  }

  # Ensure we have all required coefficients from the design matrix
  required_coefs <- colnames(design_matrix)
  if (!all(required_coefs %in% names(coefs))) {
    missing_coefs <- setdiff(required_coefs, names(coefs))
    stop(sprintf("Missing coefficients for: %s. This should not happen if estimation logic is correct.", paste(missing_coefs, collapse = ", ")))
  }

  # Ensure coefficients are in the order expected by design_matrix
  coefs_ordered <- coefs[required_coefs]


  # Calculate the *latent* response scores using the design matrix and coefficients
  latent_response_scores <- design_matrix %*% coefs_ordered

  # Add *continuous* residuals
  latent_response_scores <- latent_response_scores + rnorm(N, 0, residual_sd)

  # 3. Apply rounding and bounding to *all* scores (predictors and response) to get final discrete scores
  final_total_scores <- matrix(NA, nrow = N, ncol = n_questionnaires)
  colnames(final_total_scores) <- names(questionnaires)

  for (i in 1:n_questionnaires) {
    q_name <- colnames(latent_scores_matrix)[i]
    q_spec <- questionnaires[[q_name]]

    if (i == response_idx) {
      # For the response variable, use the calculated latent_response_scores
      final_total_scores[, i] <- round_and_bound_scores(latent_response_scores, q_spec$total_min, q_spec$total_max)
    } else {
      # For predictor variables, use their initial latent scores
      final_total_scores[, i] <- round_and_bound_scores(latent_scores_matrix[, i], q_spec$total_min, q_spec$total_max)
    }
  }

  return(final_total_scores)
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
    scores <- mv_scores[, i]
    scores <- round_and_bound_scores(scores, questionnaires[[i]]$total_min, questionnaires[[i]]$total_max) # Use new helper
    total_scores[, i] <- scores
  }

  return(total_scores)
}
