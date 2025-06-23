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
#' @param linear_model (For backward compatibility or single models) A list specifying a single linear model.
#' @param linear_models (Recommended) A list of lists, where each inner list specifies a linear model to be applied sequentially. Use this for mediation or other path models.
#' @param correlations Optional correlation matrix between questionnaire total scores
#' @param seed Random seed for reproducibility
#' @param debug If TRUE, returns additional diagnostic information about the linear model
#' @param max_retries The maximum number of attempts to generate a dataset that matches the desired coefficient signs.
#' @param verbose If TRUE, prints messages about retry attempts for matching coefficient signs.
#' @return Data frame with item-level responses and total scores for all questionnaires
#' @examples
#' # Example 1: Single moderation model (backward compatible)
#' set.seed(456)
#' simdat_single_mod <- sim.questionnaire.multi(
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
#' summary(lm(Stress_total ~ Anxiety_total * Depression_total, data = simdat_single_mod))
#'
#' # Example 2: Mediation model (IV -> M -> DV)
#' set.seed(111)
#' simdat_mediation <- sim.questionnaire.multi(
#'   N = 150,
#'   questionnaires = list(
#'     Workload = list(n_items = 10, likert = 1:5, total_mean = 30, total_sd = 5),
#'     Burnout = list(n_items = 8, likert = 1:7, total_mean = 35, total_sd = 6),
#'     Satisfaction = list(n_items = 6, likert = 0:10, total_mean = 40, total_sd = 8)
#'   ),
#'   linear_models = list(
#'     list(formula = "Burnout ~ Workload", desired_coefficient_signs = list(Workload = 1)),
#'     list(formula = "Satisfaction ~ Burnout + Workload", desired_coefficient_signs = list(Burnout = -1, Workload = -1))
#'   ),
#'   max_retries = 10
#' )
#' summary(lm(Burnout_total ~ Workload_total, data = simdat_mediation))
#' summary(lm(Satisfaction_total ~ Burnout_total + Workload_total, data = simdat_mediation))
#' @export
sim.questionnaire.multi <- function(N = 100,
                                    questionnaires,
                                    linear_model = NULL,
                                    linear_models = NULL,
                                    correlations = NULL,
                                    seed = NULL,
                                    debug = FALSE,
                                    max_retries = 20,
                                    verbose = TRUE) {

  if (!is.null(seed)) set.seed(seed)

  # --- Parameter Handling for Backward Compatibility ---
  if (!is.null(linear_model) && !is.null(linear_models)) {
    stop("Please provide either 'linear_model' for a single model or 'linear_models' for multiple, but not both.")
  }
  if (!is.null(linear_model)) {
    if (verbose) message("The 'linear_model' parameter is for single models or backward compatibility. For mediation, please use 'linear_models'.")
    linear_models <- list(linear_model)
  }

  # Validate inputs
  if (!is.list(questionnaires) || length(questionnaires) == 0) {
    stop("questionnaires must be a non-empty list")
  }
  if (!is.null(linear_models) && !is.null(correlations)) {
    stop("Please specify relationships using either 'linear_models' or 'correlations', not both.")
  }

  n_questionnaires <- length(questionnaires)
  questionnaire_names <- names(questionnaires)
  if (is.null(questionnaire_names)) {
    questionnaire_names <- paste0("S", 1:n_questionnaires)
    names(questionnaires) <- questionnaire_names
  }

  # Calculate and adjust possible ranges for each questionnaire
  for (i in 1:n_questionnaires) {
    q <- questionnaires[[i]]
    if (is.null(q$total_min)) q$total_min <- q$n_items * min(q$likert)
    if (is.null(q$total_max)) q$total_max <- q$n_items * max(q$likert)
    adjusted_bounds <- adjust_total_score_bounds(q$total_min, q$total_max,
                                                 q$n_items, q$likert,
                                                 questionnaire_names[i])
    questionnaires[[i]]$total_min <- adjusted_bounds$total_min
    questionnaires[[i]]$total_max <- adjusted_bounds$total_max
  }

  attempt <- 0
  signs_ok <- FALSE
  all_data <- NULL

  while (attempt < max_retries && !signs_ok) {
    if (attempt > 0 && verbose) {
      message(sprintf("Attempt %d of %d failed to match all coefficient signs. Retrying...", attempt, max_retries))
    }
    attempt <- attempt + 1

    # --- Data Generation ---
    if (!is.null(linear_models)) {
      total_scores <- generate_scores_with_path_model(N, questionnaires, linear_models, verbose)
    } else if (!is.null(correlations)) {
      total_scores <- generate_scores_with_correlations(N, questionnaires, correlations)
    } else {
      total_scores <- matrix(NA, nrow = N, ncol = n_questionnaires)
      for (i in 1:n_questionnaires) {
        total_scores[, i] <- generate_independent_scores(N, questionnaires[[i]])
      }
    }

    all_data <- data.frame(subj = 1:N)
    for (i in 1:n_questionnaires) {
      q <- questionnaires[[i]]
      q_name <- questionnaire_names[i]
      item_responses <- matrix(NA, nrow = N, ncol = q$n_items)
      for (j in 1:N) {
        item_responses[j, ] <- partition_score(total_scores[j, i], q$n_items, min(q$likert), max(q$likert))
      }
      item_df <- as.data.frame(item_responses)
      names(item_df) <- paste0(q_name, "_item", 1:q$n_items)
      all_data <- cbind(all_data, item_df)
      all_data[[paste0(q_name, "_total")]] <- total_scores[, i]
    }

    # --- Sign Checking for all models ---
    has_signs_to_check <- !is.null(linear_models) && any(sapply(linear_models, function(m) !is.null(m$desired_coefficient_signs)))
    if (has_signs_to_check) {
      all_models_signs_ok <- TRUE
      for (model_spec in linear_models) {
        if (is.null(model_spec$desired_coefficient_signs) || length(model_spec$desired_coefficient_signs) == 0) next

        formula_str <- model_spec$formula
        vars_in_formula <- all.vars(as.formula(formula_str))
        lm_formula_str <- formula_str
        for (v in vars_in_formula) {
          lm_formula_str <- gsub(paste0("\\b", v, "\\b"), paste0(v, "_total"), lm_formula_str)
        }
        lm_formula <- as.formula(lm_formula_str)
        model_check <- lm(lm_formula, data = all_data)
        model_coefs <- summary(model_check)$coefficients

        current_model_signs_match <- TRUE
        for (term_name in names(model_spec$desired_coefficient_signs)) {
          desired_sign <- model_spec$desired_coefficient_signs[[term_name]]
          model_term_name <- gsub("([A-Za-z0-9_\\.]+)", "\\1_total", term_name)
          if (model_term_name %in% rownames(model_coefs)) {
            actual_sign <- sign(model_coefs[model_term_name, "Estimate"])
            if (actual_sign != desired_sign) {
              current_model_signs_match <- FALSE
              break
            }
          } else {
            if (verbose) warning(paste0("Term '", term_name, "' not in model analysis. Cannot check sign."))
            current_model_signs_match <- FALSE
            break
          }
        }
        if (!current_model_signs_match) {
          all_models_signs_ok <- FALSE
          break
        }
      }
      if (all_models_signs_ok) signs_ok <- TRUE
    } else {
      signs_ok <- TRUE
    }
  } # End while

  if (signs_ok) {
    # If successful, and we were running silently, and it took more than one attempt,
    # provide a confirmation message.
    if (!verbose && attempt > 1) {
      message(sprintf("Successfully generated data with desired coefficient signs after %d attempts.", attempt))
    }
  } else {
    # If sign matching failed, always issue a warning regardless of verbosity.
    warning(sprintf("Failed to generate data with desired signs after %d attempts. Returning last dataset.", attempt))
  }

  if (debug) {
    diagnostic_info <- list(total_scores = total_scores, questionnaires = questionnaires,
                            linear_models = linear_models, correlations = correlations)
    all_data$diagnostic_info <- I(list(diagnostic_info))
  }

  return(all_data)
}

# Helper function to generate independent scores
generate_independent_scores <- function(N, questionnaire) {
  scores <- rnorm(N, mean = questionnaire$total_mean, sd = questionnaire$total_sd)
  scores <- round_and_bound_scores(scores, questionnaire$total_min, questionnaire$total_max)
  return(scores)
}

# New Helper: Generate scores based on a sequential path model (e.g., mediation)
generate_scores_with_path_model <- function(N, questionnaires, linear_models, verbose = TRUE) {
  n_questionnaires <- length(questionnaires)
  q_names <- names(questionnaires)

  # 1. Generate ALL questionnaire scores as continuous, unbounded, normally distributed values initially
  latent_scores_matrix <- matrix(NA, nrow = N, ncol = n_questionnaires)
  colnames(latent_scores_matrix) <- q_names
  for (i in 1:n_questionnaires) {
    q <- questionnaires[[i]]
    latent_scores_matrix[, i] <- rnorm(N, mean = q$total_mean, sd = q$total_sd)
  }

  # 2. Sequentially apply each linear model to update the latent scores
  for (model_spec in linear_models) {
    formula_str <- model_spec$formula
    response_var <- trimws(gsub("\\s*~.*", "", formula_str))
    predictor_vars_str <- trimws(gsub(".*~\\s*", "", formula_str))
    response_idx <- which(q_names == response_var)
    if (length(response_idx) == 0) {
      stop(sprintf("Response variable '%s' from formula not found in questionnaire names", response_var))
    }

    # Extract unique predictor variable names from the formula string
    predictor_names <- unique(trimws(grep("\\S+", unlist(strsplit(predictor_vars_str, "[+*:]")), value = TRUE)))
    for (var_name in predictor_names) {
      if (!var_name %in% q_names) {
        stop(sprintf("Variable '%s' in formula but not in questionnaire names.", var_name))
      }
    }

    # Determine coefficients and residual_sd, estimating if not provided
    coefs <- model_spec$coefficients
    residual_sd <- model_spec$residual_sd
    if (is.null(coefs) || is.null(residual_sd)) {
      if (verbose) message(sprintf("Estimating coefficients/residual_sd for model: '%s'", formula_str))
      est_params <- estimate_lm_parameters(model_spec, questionnaires, predictor_names, response_var, verbose)
      if(is.null(coefs)) coefs <- est_params$coefficients
      if(is.null(residual_sd)) residual_sd <- est_params$residual_sd
    }

    # Create design matrix from *current* latent scores
    pred_data_latent <- as.data.frame(latent_scores_matrix[, predictor_names, drop = FALSE])
    design_matrix <- model.matrix(as.formula(paste("~", predictor_vars_str)), data = pred_data_latent)
    if ("intercept" %in% names(coefs)) names(coefs)[names(coefs) == "intercept"] <- "(Intercept)"
    required_coefs <- colnames(design_matrix)
    if (!all(required_coefs %in% names(coefs))) {
      stop(sprintf("Missing coefficients for: %s in model '%s'",
                   paste(setdiff(required_coefs, names(coefs)), collapse = ", "), formula_str))
    }
    coefs_ordered <- coefs[required_coefs]

    # Calculate new latent scores for the response and update the matrix
    latent_response_scores <- (design_matrix %*% coefs_ordered) + rnorm(N, 0, residual_sd)
    latent_scores_matrix[, response_idx] <- latent_response_scores
  }

  # 3. Apply rounding and bounding to the final latent scores matrix
  final_total_scores <- matrix(NA, nrow = N, ncol = n_questionnaires)
  for (i in 1:n_questionnaires) {
    q_spec <- questionnaires[[i]]
    final_total_scores[, i] <- round_and_bound_scores(latent_scores_matrix[, i], q_spec$total_min, q_spec$total_max)
  }

  return(final_total_scores)
}

# New Helper: Estimate reasonable LM parameters if not provided by user
estimate_lm_parameters <- function(model_spec, questionnaires, predictor_names, response_var, verbose = TRUE) {
  # Internal heuristic parameters
  coverage_sd <- 2
  max_main_effect_ratio <- 0.3
  interaction_effect_factor <- 0.005

  response_q <- questionnaires[[response_var]]
  response_range <- response_q$total_max - response_q$total_min

  predictor_info <- lapply(setNames(predictor_names, predictor_names), function(p_name) {
    pq <- questionnaires[[p_name]]
    p_mean <- pq$total_mean
    p_sd <- pq$total_sd
    p_min <- max(p_mean - coverage_sd * p_sd, pq$total_min)
    p_max <- min(p_mean + coverage_sd * p_sd, pq$total_max)
    list(range_est = p_max - p_min, mean = p_mean)
  })

  coefs <- list()
  coefs["(Intercept)"] <- response_q$total_mean

  formula_obj <- terms(as.formula(model_spec$formula))
  term_labels <- attr(formula_obj, "term.labels")

  for (term in term_labels) {
    vars_in_term <- strsplit(term, ":")[[1]]
    is_interaction <- length(vars_in_term) > 1

    sign_val <- model_spec$desired_coefficient_signs[[term]] %||% 1

    if (!is_interaction) {
      p_info <- predictor_info[[term]]
      if (p_info$range_est > 0) {
        max_slope <- (response_range * max_main_effect_ratio) / p_info$range_est
        coefs[[term]] <- max_slope * sign_val
      } else {
        coefs[[term]] <- 0
      }
    } else { # Interaction Term
      product_of_ranges <- prod(sapply(vars_in_term, function(v) predictor_info[[v]]$range_est))
      if (product_of_ranges > 0) {
        max_slope <- (response_range * interaction_effect_factor) / product_of_ranges
        coefs[[term]] <- max_slope * sign_val
      } else {
        coefs[[term]] <- 0
      }
    }
  }

  residual_sd <- response_q$total_sd * 0.5

  if (verbose) {
    message("  > Estimated coefficients: ", paste(names(coefs), round(unlist(coefs), 3), sep="=", collapse=", "))
    message("  > Estimated residual_sd: ", round(residual_sd, 3))
  }

  return(list(coefficients = unlist(coefs), residual_sd = residual_sd))
}

# Helper for null-coalescing operator
`%||%` <- function(a, b) if (is.null(a)) b else a

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
    scores <- round_and_bound_scores(scores, questionnaires[[i]]$total_min, questionnaires[[i]]$total_max)
    total_scores[, i] <- scores
  }

  return(total_scores)
}
