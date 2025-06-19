# ============================================================================
# COMPLETE ENHANCED MACHINE LEARNING ANALYSIS FOR MALARIA DETECTION
# Statistical Rigor Framework for Scientific Reports Publication
# WITH COMPREHENSIVE VISUALIZATIONS & PRECISION-RECALL CURVES
# Addresses ALL Reviewer Requirements
# ============================================================================

# Load ALL required libraries
suppressMessages({
  library(e1071)         # For Naive Bayes
  library(caret)         # For cross-validation and evaluation
  library(randomForest)  # For Random Forest
  library(xgboost)       # For XGBoost
  library(brms)          # For Bayesian models
  library(pROC)          # For ROC curves
  library(ggplot2)       # For visualization
  library(dplyr)         # For data manipulation
  library(tidyr)         # For data reshaping
  library(boot)          # For bootstrap methods
  library(PRROC)         # For Precision-Recall curves
  library(effsize)       # For effect size calculations
  library(broom)         # For tidy statistical output
  library(knitr)         # For table formatting
  library(gridExtra)     # For plot arrangements
  library(openxlsx)      # For Excel output
  library(corrplot)      # For correlation matrices
  library(pheatmap)      # For heatmaps
  library(RColorBrewer)  # For color palettes
  library(scales)        # For formatting
  library(cowplot)       # For plot arrangements
  library(tibble)        # ADD THIS LINE - for column_to_rownames
})

# Set seed for reproducibility
set.seed(42)

# ============================================================================
# 0. COMPREHENSIVE RESULT SAVING SETUP
# ============================================================================

# Create output directories
output_base_dir <- "Malaria_ML_Results_Complete"
output_dirs <- list(
  main = output_base_dir,
  tables = file.path(output_base_dir, "Tables"),
  models = file.path(output_base_dir, "Models"), 
  plots = file.path(output_base_dir, "Plots"),
  statistical_tests = file.path(output_base_dir, "Statistical_Tests"),
  reports = file.path(output_base_dir, "Reports"),
  raw_results = file.path(output_base_dir, "Raw_Results")
)

# Create all directories
for(dir_path in output_dirs) {
  if(!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
}

cat("=== COMPLETE ENHANCED MALARIA ML ANALYSIS ===\n")
cat("Output directories created:\n")
for(name in names(output_dirs)) {
  cat("  -", name, ":", output_dirs[[name]], "\n")
}

# Function to save results with timestamp
save_with_timestamp <- function(object, filename, dir_path, format = "csv") {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  if(format == "csv" && is.data.frame(object)) {
    full_path <- file.path(dir_path, paste0(timestamp, "_", filename, ".csv"))
    write.csv(object, full_path, row.names = FALSE)
  } else if(format == "rds") {
    full_path <- file.path(dir_path, paste0(timestamp, "_", filename, ".rds"))
    saveRDS(object, full_path)
  } else if(format == "txt") {
    full_path <- file.path(dir_path, paste0(timestamp, "_", filename, ".txt"))
    writeLines(as.character(object), full_path)
  } else if(format == "xlsx") {
    full_path <- file.path(dir_path, paste0(timestamp, "_", filename, ".xlsx"))
    write.xlsx(object, full_path)
  }
  
  cat("✓ Saved:", basename(full_path), "\n")
  return(full_path)
}

# Analysis log
analysis_log <- list()
analysis_log$start_time <- Sys.time()
analysis_log$session_info <- sessionInfo()

# ============================================================================
# 1. DATA LOADING AND PREPROCESSING
# ============================================================================

cat("\n=== DATA LOADING AND PREPROCESSING ===\n")

# Load synthetic data
synthetic_data <- read.csv("synthetic_malaria_data_validated.csv", stringsAsFactors = FALSE)
cat("Data loaded successfully. Dimensions:", dim(synthetic_data), "\n")

# Data preprocessing and validation
synthetic_data$Malaria <- factor(synthetic_data$Malaria, levels = c("No", "Yes"))
synthetic_data$Fever <- factor(synthetic_data$Fever, levels = c("No", "Yes"))
synthetic_data$Chills <- factor(synthetic_data$Chills, levels = c("No", "Yes"))
synthetic_data$Fatigue <- factor(synthetic_data$Fatigue, levels = c("No", "Yes"))

# Remove Temperature_constrained if it's identical to Temperature
if("Temperature_constrained" %in% names(synthetic_data)) {
  if(all(synthetic_data$Temperature == synthetic_data$Temperature_constrained, na.rm = TRUE)) {
    synthetic_data$Temperature_constrained <- NULL
    cat("Removed duplicate Temperature_constrained column\n")
  }
}

# Define features and target
features <- c("Fever", "Chills", "Fatigue", "Age", "Temperature", "Rainfall")
target <- "Malaria"
model_names <- c("Naive_Bayes", "Logistic_Regression", "Random_Forest", 
                 "XGBoost", "Enhanced_Bayesian_LR")

# Save data summary
data_summary <- data.frame(
  Metric = c("Total_Samples", "Features", "Malaria_Prevalence_Percent", 
             "No_Malaria_Count", "Yes_Malaria_Count"),
  Value = c(nrow(synthetic_data), 
            length(features),
            round(prop.table(table(synthetic_data$Malaria))[2] * 100, 2),
            sum(synthetic_data$Malaria == "No"),
            sum(synthetic_data$Malaria == "Yes"))
)

save_with_timestamp(data_summary, "data_summary", output_dirs$tables)

# Log preprocessing
analysis_log$data_preprocessing <- list(
  original_dimensions = dim(synthetic_data),
  features_used = features,
  target_variable = target,
  malaria_prevalence = prop.table(table(synthetic_data$Malaria))[2]
)

cat("Data preprocessing complete and saved\n")

# ============================================================================
# 2. ENHANCED CROSS-VALIDATION SETUP WITH STATISTICAL FRAMEWORK
# ============================================================================

# Cost weights for threshold tuning
cost_fn <- 15  # Cost of false negative
cost_fp <- 3   # Cost of false positive

# Enhanced cross-validation setup
set.seed(42)
folds <- createFolds(synthetic_data$Malaria, k = 5, list = TRUE, returnTrain = FALSE)

# Save CV setup
cv_setup <- data.frame(
  Fold = 1:5,
  Test_Size = sapply(folds, length),
  Train_Size = nrow(synthetic_data) - sapply(folds, length)
)

save_with_timestamp(cv_setup, "cross_validation_setup", output_dirs$tables)

# Function for bootstrap confidence intervals
bootstrap_ci <- function(data, statistic, R = 1000, conf.level = 0.95) {
  boot_results <- boot(data, statistic, R = R)
  ci <- boot.ci(boot_results, conf = conf.level, type = "perc")
  return(list(
    estimate = boot_results$t0,
    ci_lower = ci$percent[4],
    ci_upper = ci$percent[5],
    se = sd(boot_results$t)
  ))
}

# Enhanced evaluation function with comprehensive metrics
evaluate_model_comprehensive <- function(actual, predicted, probabilities) {
  actual <- factor(actual, levels = c("No", "Yes"))
  predicted <- factor(predicted, levels = c("No", "Yes"))
  
  cm <- confusionMatrix(predicted, actual, positive = "Yes")
  roc_obj <- roc(actual, probabilities, levels = c("No", "Yes"), direction = "<")
  
  pr_obj <- pr.curve(scores.class0 = probabilities[actual == "Yes"],
                     scores.class1 = probabilities[actual == "No"],
                     curve = TRUE)
  
  tn <- cm$table[1,1]; fp <- cm$table[2,1]
  fn <- cm$table[1,2]; tp <- cm$table[2,2]
  total_cost <- (cost_fn * fn) + (cost_fp * fp)
  
  return(list(
    accuracy = as.numeric(cm$overall["Accuracy"]),
    sensitivity = as.numeric(cm$byClass["Sensitivity"]),
    specificity = as.numeric(cm$byClass["Specificity"]),
    ppv = as.numeric(cm$byClass["Pos Pred Value"]),
    npv = as.numeric(cm$byClass["Neg Pred Value"]),
    f1 = as.numeric(cm$byClass["F1"]),
    auc = as.numeric(auc(roc_obj)),
    auprc = pr_obj$auc.integral,
    cost = total_cost,
    cm = cm$table,
    roc_obj = roc_obj
  ))
}

# Function for optimal threshold finding
find_optimal_threshold <- function(probabilities, actual, cost_fn, cost_fp) {
  thresholds <- seq(0.05, 0.95, by = 0.01)
  costs <- sapply(thresholds, function(t) {
    pred <- ifelse(probabilities > t, "Yes", "No")
    pred <- factor(pred, levels = c("No", "Yes"))
    actual <- factor(actual, levels = c("No", "Yes"))
    cm <- table(Predicted = pred, Actual = actual)
    if(nrow(cm) < 2 || ncol(cm) < 2) return(Inf)
    fn <- ifelse(nrow(cm) > 1 && ncol(cm) > 1, cm[1, 2], 0)
    fp <- ifelse(nrow(cm) > 1 && ncol(cm) > 1, cm[2, 1], 0)
    return((cost_fn * fn) + (cost_fp * fp))
  })
  return(thresholds[which.min(costs)])
}

# ============================================================================
# 3. ENHANCED BAYESIAN LOGISTIC REGRESSION FUNCTION
# ============================================================================

enhanced_bayesian_lr <- function(synthetic_data, features, target, cv_folds) {
  
  cat("Training Enhanced Bayesian Logistic Regression...\n")
  cat("Features: Clinical interactions, hierarchical priors, and regularization\n")
  
  # Create age groups for hierarchical modeling
  synthetic_data$Age_Group <- cut(synthetic_data$Age, 
                                  breaks = c(0, 18, 45, 65, 100), 
                                  labels = c("Child", "Young_Adult", "Middle_Age", "Elderly"),
                                  include.lowest = TRUE)
  
  # Standardize continuous variables for better MCMC convergence
  synthetic_data$Age_std <- scale(synthetic_data$Age)[,1]
  synthetic_data$Temperature_std <- scale(synthetic_data$Temperature)[,1]
  synthetic_data$Rainfall_std <- scale(synthetic_data$Rainfall)[,1]
  
  # Enhanced formula with clinical domain knowledge
  enhanced_formula <- bf(
    Malaria ~ 
      # Main effects
      Fever + Chills + Fatigue + Age_std + Temperature_std + Rainfall_std +
      
      # Clinical interactions (based on malaria pathophysiology)
      Fever:Age_std +                    # Age affects fever response
      Chills:Temperature_std +           # Environmental-symptom interaction
      Fatigue:Age_std +                  # Age affects fatigue severity
      Fever:Chills +                     # Symptom co-occurrence
      
      # Environmental interactions
      Temperature_std:Rainfall_std +     # Climate interaction
      
      # Non-linear age effects (polynomial)
      I(Age_std^2) +
      
      # Hierarchical effects by age group
      (1 | Age_Group),
    
    family = bernoulli(link = "logit")
  )
  
  # Enhanced prior specification with clinical reasoning
  enhanced_priors <- c(
    # Intercept prior (log-odds of malaria prevalence ~20%)
    prior(normal(-1.4, 0.5), class = "Intercept"),
    
    # Main symptom effects (expected to be positive and strong)
    prior(normal(1.0, 0.5), class = "b", coef = "FeverYes"),
    prior(normal(0.8, 0.5), class = "b", coef = "ChillsYes"),
    prior(normal(0.6, 0.5), class = "b", coef = "FatigueYes"),
    
    # Age effect (children and elderly more susceptible)
    prior(normal(0.3, 0.3), class = "b", coef = "Age_std"),
    prior(normal(-0.2, 0.2), class = "b", coef = "IAge_stdE2"),  # Quadratic age
    
    # Environmental effects (moderate)
    prior(normal(0.2, 0.3), class = "b", coef = "Temperature_std"),
    prior(normal(0.1, 0.2), class = "b", coef = "Rainfall_std"),
    
    # Interaction effects (smaller but important)
    prior(normal(0, 0.3), class = "b", coef = "FeverYes:Age_std"),
    prior(normal(0, 0.3), class = "b", coef = "ChillsYes:Temperature_std"),
    prior(normal(0, 0.3), class = "b", coef = "FatigueYes:Age_std"),
    prior(normal(0.5, 0.4), class = "b", coef = "FeverYes:ChillsYes"),
    prior(normal(0, 0.2), class = "b", coef = "Temperature_std:Rainfall_std"),
    
    # Hierarchical variance (age group effects)
    prior(exponential(2), class = "sd", group = "Age_Group")
  )
  
  # Fit enhanced model with optimized MCMC settings
  enhanced_model <- brm(
    formula = enhanced_formula,
    data = synthetic_data,
    prior = enhanced_priors,
    
    # MCMC settings optimized for complex model
    chains = 4,
    iter = 3000,        # Increased for complex model
    warmup = 1500,      # Longer warmup for convergence
    cores = 4,
    
    # Enhanced control parameters
    control = list(
      adapt_delta = 0.99,      # Higher for complex model
      max_treedepth = 15,      # Deeper trees for complex posterior
      stepsize = 0.01          # Smaller steps for stability
    ),
    
    # Output control
    silent = 2,
    refresh = 0,
    
    # Save predictions for cross-validation
    save_pars = save_pars(all = TRUE)
  )
  
  # Model diagnostics
  cat("Model Diagnostics:\n")
  model_summary <- summary(enhanced_model)
  rhat_summary <- model_summary$fixed[, "Rhat"]
  cat("- Rhat values (should be < 1.01):\n")
  cat("  Max Rhat:", round(max(rhat_summary, na.rm = TRUE), 4), "\n")
  
  # Effective sample size
  ess_summary <- model_summary$fixed[, "Bulk_ESS"]
  cat("- Effective Sample Size (should be > 400):\n")
  cat("  Min ESS:", round(min(ess_summary, na.rm = TRUE), 0), "\n")
  
  # Cross-validation for enhanced model
  cat("Performing cross-validation for Enhanced Bayesian LR...\n")
  enhanced_cv_results <- lapply(1:5, function(fold_idx) {
    test_indices <- cv_folds[[fold_idx]]
    train_data <- synthetic_data[-test_indices, ]
    test_data <- synthetic_data[test_indices, ]
    
    # Update model with fold data
    fold_model <- update(enhanced_model, newdata = train_data, 
                         refresh = 0, silent = 2)
    
    # Predict on test set
    fold_pred <- predict(fold_model, newdata = test_data, 
                         type = "response", allow_new_levels = TRUE)
    
    return(data.frame(
      obs = test_data$Malaria,
      Yes = fold_pred[, "Estimate"],
      No = 1 - fold_pred[, "Estimate"],
      Resample = paste("Fold", fold_idx),
      Uncertainty = fold_pred[, "Est.Error"]  # Bayesian uncertainty
    ))
  })
  
  enhanced_cv_pred <- do.call(rbind, enhanced_cv_results)
  
  # Calculate AUC with uncertainty quantification
  enhanced_cv_auc <- auc(roc(enhanced_cv_pred$obs, enhanced_cv_pred$Yes, 
                             levels = c("No", "Yes"), direction = "<", quiet = TRUE))
  
  # Feature importance from posterior distributions
  posterior_samples <- as.matrix(enhanced_model)
  feature_importance <- data.frame(
    Feature = colnames(posterior_samples),
    Mean_Effect = apply(posterior_samples, 2, mean),
    SD_Effect = apply(posterior_samples, 2, sd),
    CI_Lower = apply(posterior_samples, 2, quantile, 0.025),
    CI_Upper = apply(posterior_samples, 2, quantile, 0.975),
    Prob_Positive = apply(posterior_samples > 0, 2, mean)
  )
  
  # Return comprehensive results
  return(list(
    model = enhanced_model,
    cv_predictions = enhanced_cv_pred,
    cv_auc = as.numeric(enhanced_cv_auc),
    feature_importance = feature_importance,
    diagnostics = list(
      max_rhat = max(rhat_summary, na.rm = TRUE),
      min_ess = min(ess_summary, na.rm = TRUE)
    ),
    enhanced_data = synthetic_data  # Return data with new variables
  ))
}

# ============================================================================
# 4. MODEL TRAINING WITH STATISTICAL VALIDATION
# ============================================================================

cat("\n=== TRAINING MODELS WITH STATISTICAL VALIDATION ===\n")

# Initialize results storage
cv_results <- list()
model_objects <- list()

# Cross-validation control with proper settings
train_control <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = "final",
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel = TRUE,
  verboseIter = FALSE,
  index = lapply(folds, function(x) setdiff(1:nrow(synthetic_data), x))
)

# MODEL 1: NAIVE BAYES
cat("Training Naive Bayes...\n")
set.seed(42)
start_time <- Sys.time()

nb_model <- train(
  x = synthetic_data[, features],
  y = synthetic_data[, target],
  method = "nb",
  trControl = train_control,
  metric = "ROC"
)

nb_cv_pred <- nb_model$pred
nb_opt_threshold <- find_optimal_threshold(nb_cv_pred$Yes, nb_cv_pred$obs, cost_fn, cost_fp)

cv_results[["Naive_Bayes"]] <- list(
  model = nb_model,
  cv_predictions = nb_cv_pred,
  optimal_threshold = nb_opt_threshold,
  cv_auc = nb_model$results$ROC,
  training_time = as.numeric(difftime(Sys.time(), start_time, units = "mins"))
)

model_objects[["Naive_Bayes"]] <- nb_model

# MODEL 2: LOGISTIC REGRESSION
cat("Training Logistic Regression...\n")
set.seed(42)
start_time <- Sys.time()

lr_model <- train(
  x = synthetic_data[, features],
  y = synthetic_data[, target],
  method = "glm",
  family = "binomial",
  trControl = train_control,
  metric = "ROC"
)

lr_cv_pred <- lr_model$pred
lr_opt_threshold <- find_optimal_threshold(lr_cv_pred$Yes, lr_cv_pred$obs, cost_fn, cost_fp)

cv_results[["Logistic_Regression"]] <- list(
  model = lr_model,
  cv_predictions = lr_cv_pred,
  optimal_threshold = lr_opt_threshold,
  cv_auc = lr_model$results$ROC,
  training_time = as.numeric(difftime(Sys.time(), start_time, units = "mins"))
)

model_objects[["Logistic_Regression"]] <- lr_model

# MODEL 3: RANDOM FOREST
cat("Training Random Forest...\n")
set.seed(42)
start_time <- Sys.time()

rf_model <- train(
  x = synthetic_data[, features],
  y = synthetic_data[, target],
  method = "rf",
  trControl = train_control,
  tuneGrid = data.frame(mtry = c(2, 3, 4)),
  metric = "ROC",
  ntree = 500
)

rf_cv_pred <- rf_model$pred[rf_model$pred$mtry == rf_model$bestTune$mtry, ]
rf_opt_threshold <- find_optimal_threshold(rf_cv_pred$Yes, rf_cv_pred$obs, cost_fn, cost_fp)

cv_results[["Random_Forest"]] <- list(
  model = rf_model,
  cv_predictions = rf_cv_pred,
  optimal_threshold = rf_opt_threshold,
  cv_auc = max(rf_model$results$ROC),
  training_time = as.numeric(difftime(Sys.time(), start_time, units = "mins"))
)

model_objects[["Random_Forest"]] <- rf_model

# MODEL 4: XGBOOST
cat("Training XGBoost...\n")
set.seed(42)
start_time <- Sys.time()

xgb_data_prep <- synthetic_data
xgb_data_prep$Fever <- as.numeric(xgb_data_prep$Fever) - 1
xgb_data_prep$Chills <- as.numeric(xgb_data_prep$Chills) - 1
xgb_data_prep$Fatigue <- as.numeric(xgb_data_prep$Fatigue) - 1

xgb_model <- train(
  x = xgb_data_prep[, features],
  y = synthetic_data[, target],
  method = "xgbTree",
  trControl = train_control,
  tuneGrid = expand.grid(
    nrounds = c(50, 100, 150),
    max_depth = c(3, 4, 5),
    eta = c(0.1, 0.2, 0.3),
    gamma = 0,
    colsample_bytree = 0.8,
    min_child_weight = 1,
    subsample = 0.8
  ),
  metric = "ROC",
  verbosity = 0
)

best_params <- xgb_model$bestTune
xgb_cv_pred <- xgb_model$pred[
  xgb_model$pred$nrounds == best_params$nrounds &
    xgb_model$pred$max_depth == best_params$max_depth &
    xgb_model$pred$eta == best_params$eta &
    xgb_model$pred$gamma == best_params$gamma &
    xgb_model$pred$colsample_bytree == best_params$colsample_bytree &
    xgb_model$pred$min_child_weight == best_params$min_child_weight &
    xgb_model$pred$subsample == best_params$subsample, ]

xgb_opt_threshold <- find_optimal_threshold(xgb_cv_pred$Yes, xgb_cv_pred$obs, cost_fn, cost_fp)

cv_results[["XGBoost"]] <- list(
  model = xgb_model,
  cv_predictions = xgb_cv_pred,
  optimal_threshold = xgb_opt_threshold,
  cv_auc = max(xgb_model$results$ROC),
  training_time = as.numeric(difftime(Sys.time(), start_time, units = "mins"))
)

model_objects[["XGBoost"]] <- xgb_model

# MODEL 5: ENHANCED BAYESIAN LOGISTIC REGRESSION
cat("Training Enhanced Bayesian Logistic Regression...\n")
set.seed(42)
start_time <- Sys.time()

enhanced_bayes_results <- enhanced_bayesian_lr(
  synthetic_data = synthetic_data,
  features = features,
  target = target,
  cv_folds = folds
)

cv_results[["Enhanced_Bayesian_LR"]] <- list(
  model = enhanced_bayes_results$model,
  cv_predictions = enhanced_bayes_results$cv_predictions,
  optimal_threshold = find_optimal_threshold(
    enhanced_bayes_results$cv_predictions$Yes, 
    enhanced_bayes_results$cv_predictions$obs, 
    cost_fn, cost_fp
  ),
  cv_auc = enhanced_bayes_results$cv_auc,
  feature_importance = enhanced_bayes_results$feature_importance,
  diagnostics = enhanced_bayes_results$diagnostics,
  training_time = as.numeric(difftime(Sys.time(), start_time, units = "mins"))
)

model_objects[["Enhanced_Bayesian_LR"]] <- enhanced_bayes_results$model

# Save all model objects
save_with_timestamp(model_objects, "all_trained_models", output_dirs$models, "rds")

# Save training times
training_times <- data.frame(
  Model = model_names,
  Training_Time_Minutes = sapply(model_names, function(x) cv_results[[x]]$training_time)
)
save_with_timestamp(training_times, "training_times", output_dirs$tables)

cat("✓ All models trained and saved\n")

# ============================================================================
# 5. COMPREHENSIVE STATISTICAL ANALYSIS
# ============================================================================

cat("\n=== COMPREHENSIVE STATISTICAL ANALYSIS ===\n")

# Calculate CV performance metrics with confidence intervals
cv_performance <- data.frame()

for(model_name in model_names) {
  pred_data <- cv_results[[model_name]]$cv_predictions
  threshold <- cv_results[[model_name]]$optimal_threshold
  
  # Apply threshold to get predictions
  pred_class <- ifelse(pred_data$Yes > threshold, "Yes", "No")
  pred_class <- factor(pred_class, levels = c("No", "Yes"))
  actual <- factor(pred_data$obs, levels = c("No", "Yes"))
  
  # Calculate metrics
  metrics <- evaluate_model_comprehensive(actual, pred_class, pred_data$Yes)
  
  # Bootstrap confidence intervals for key metrics
  n_bootstrap <- 1000
  set.seed(42)
  
  bootstrap_metrics <- replicate(n_bootstrap, {
    boot_indices <- sample(length(actual), replace = TRUE)
    boot_actual <- actual[boot_indices]
    boot_pred_class <- pred_class[boot_indices]
    boot_prob <- pred_data$Yes[boot_indices]
    
    boot_eval <- evaluate_model_comprehensive(boot_actual, boot_pred_class, boot_prob)
    c(boot_eval$accuracy, boot_eval$sensitivity, boot_eval$specificity, 
      boot_eval$auc, boot_eval$auprc, boot_eval$f1)
  })
  
  # Calculate confidence intervals
  ci_accuracy <- quantile(bootstrap_metrics[1,], c(0.025, 0.975))
  ci_sensitivity <- quantile(bootstrap_metrics[2,], c(0.025, 0.975))
  ci_specificity <- quantile(bootstrap_metrics[3,], c(0.025, 0.975))
  ci_auc <- quantile(bootstrap_metrics[4,], c(0.025, 0.975))
  ci_auprc <- quantile(bootstrap_metrics[5,], c(0.025, 0.975))
  ci_f1 <- quantile(bootstrap_metrics[6,], c(0.025, 0.975))
  
  # Store results
  cv_performance <- rbind(cv_performance, data.frame(
    Model = model_name,
    Accuracy = metrics$accuracy,
    Accuracy_CI_Lower = ci_accuracy[1],
    Accuracy_CI_Upper = ci_accuracy[2],
    Sensitivity = metrics$sensitivity,
    Sensitivity_CI_Lower = ci_sensitivity[1],
    Sensitivity_CI_Upper = ci_sensitivity[2],
    Specificity = metrics$specificity,
    Specificity_CI_Lower = ci_specificity[1],
    Specificity_CI_Upper = ci_specificity[2],
    AUC = metrics$auc,
    AUC_CI_Lower = ci_auc[1],
    AUC_CI_Upper = ci_auc[2],
    AUPRC = metrics$auprc,
    AUPRC_CI_Lower = ci_auprc[1],
    AUPRC_CI_Upper = ci_auprc[2],
    F1 = metrics$f1,
    F1_CI_Lower = ci_f1[1],
    F1_CI_Upper = ci_f1[2],
    Cost = metrics$cost,
    Optimal_Threshold = threshold,
    Training_Time_Minutes = cv_results[[model_name]]$training_time,
    stringsAsFactors = FALSE
  ))
}

# Save comprehensive performance results
save_with_timestamp(cv_performance, "model_performance_with_CI", output_dirs$tables)


# ============================================================================
# 5B. CROSS-VALIDATION STABILITY ANALYSIS
# ============================================================================

cat("Performing cross-validation stability analysis...\n")

# Load required libraries for this section
suppressMessages({
  library(grid)      # For textGrob
  library(cowplot)   # For plot_grid and get_legend
})

# Define colors for CV stability (local definition)
stability_colors <- c("#E31A1C", "#1F78B4", "#33A02C", "#FF7F00", "#6A3D9A")
names(stability_colors) <- model_names

# Extract fold-specific performance for stability analysis
fold_performance <- data.frame()

for(model_name in model_names) {
  for(fold_idx in 1:5) {
    fold_data <- cv_results[[model_name]]$cv_predictions
    fold_indices <- which(grepl(paste0("Fold.*", fold_idx), fold_data$Resample))
    
    if(length(fold_indices) > 0) {
      fold_actual <- fold_data$obs[fold_indices]
      fold_prob <- fold_data$Yes[fold_indices]
      threshold <- cv_results[[model_name]]$optimal_threshold
      
      fold_pred <- ifelse(fold_prob > threshold, "Yes", "No")
      fold_pred <- factor(fold_pred, levels = c("No", "Yes"))
      fold_actual <- factor(fold_actual, levels = c("No", "Yes"))
      
      # Calculate fold metrics
      fold_metrics <- evaluate_model_comprehensive(fold_actual, fold_pred, fold_prob)
      
      fold_performance <- rbind(fold_performance, data.frame(
        Model = model_name,
        Fold = fold_idx,
        Accuracy = fold_metrics$accuracy,
        Sensitivity = fold_metrics$sensitivity,
        Specificity = fold_metrics$specificity,
        AUC = fold_metrics$auc,
        F1 = fold_metrics$f1,
        stringsAsFactors = FALSE
      ))
    }
  }
}

# Create CV stability plots
stability_metrics <- c("AUC", "Accuracy", "Sensitivity", "F1")
stability_plots <- list()

for(metric in stability_metrics) {
  stability_plots[[metric]] <- ggplot(fold_performance, 
                                      aes(x = factor(Fold), y = .data[[metric]], 
                                          color = Model, group = Model)) +
    geom_line(linewidth = 1) +  # Fixed: changed from size to linewidth
    geom_point(size = 2.5) +
    scale_color_manual(values = stability_colors) +
    labs(
      title = paste(metric, "Stability Across Folds"),
      x = "Cross-Validation Fold",
      y = metric
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 11, face = "bold"),
      legend.position = "none"
    ) +
    ylim(min(fold_performance[[metric]]) * 0.95, 
         max(fold_performance[[metric]]) * 1.02)
}

# Combine stability plots
combined_stability <- plot_grid(
  plotlist = stability_plots,
  ncol = 2, nrow = 2,
  labels = c("A", "B", "C", "D")
)

# Extract legend properly (fixes the warning)
legend_plot <- stability_plots[[1]] + 
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 1, title = "Model"))

# Get legend with proper error handling
plot_legend <- get_legend(legend_plot)

# Add overall title and legend
stability_with_legend <- plot_grid(
  combined_stability,
  plot_legend,
  ncol = 1,
  rel_heights = c(0.9, 0.1)
)

# Add title using textGrob (now properly loaded)
title_stability <- textGrob("Cross-Validation Stability Analysis", 
                            gp = gpar(fontsize = 16, fontface = "bold"))

stability_final <- plot_grid(
  title_stability,
  stability_with_legend,
  ncol = 1,
  rel_heights = c(0.05, 0.95)
)

# Save plots
ggsave(file.path(output_dirs$plots, "Cross_Validation_Stability.png"), 
       stability_final, width = 14, height = 10, dpi = 300)
ggsave(file.path(output_dirs$plots, "Cross_Validation_Stability.pdf"), 
       stability_final, width = 14, height = 10)

# Calculate stability metrics (coefficient of variation)
stability_summary <- fold_performance %>%
  group_by(Model) %>%
  summarise(
    AUC_Mean = mean(AUC, na.rm = TRUE),
    AUC_SD = sd(AUC, na.rm = TRUE),
    AUC_CV = sd(AUC, na.rm = TRUE) / mean(AUC, na.rm = TRUE) * 100,
    Sensitivity_Mean = mean(Sensitivity, na.rm = TRUE),
    Sensitivity_SD = sd(Sensitivity, na.rm = TRUE),
    Sensitivity_CV = sd(Sensitivity, na.rm = TRUE) / mean(Sensitivity, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  arrange(AUC_CV)

save_with_timestamp(stability_summary, "cross_validation_stability", output_dirs$tables)
save_with_timestamp(fold_performance, "fold_by_fold_performance", output_dirs$tables)

cat("✓ Cross-validation stability analysis completed\n")


# ============================================================================
# 6. STATISTICAL SIGNIFICANCE TESTING
# ============================================================================

cat("Performing statistical significance tests...\n")

# McNemar's tests for pairwise comparisons
mcnemar_results <- data.frame()
model_combinations <- combn(model_names, 2, simplify = FALSE)

for(combo in model_combinations) {
  model1 <- combo[1]
  model2 <- combo[2]
  
  pred1_data <- cv_results[[model1]]$cv_predictions
  pred2_data <- cv_results[[model2]]$cv_predictions
  
  threshold1 <- cv_results[[model1]]$optimal_threshold
  threshold2 <- cv_results[[model2]]$optimal_threshold
  
  pred1 <- ifelse(pred1_data$Yes > threshold1, "Yes", "No")
  pred2 <- ifelse(pred2_data$Yes > threshold2, "Yes", "No")
  
  actual <- pred1_data$obs
  
  model1_correct <- (pred1 == actual)
  model2_correct <- (pred2 == actual)
  
  contingency_table <- table(model1_correct, model2_correct)
  
  if(nrow(contingency_table) == 2 && ncol(contingency_table) == 2) {
    mcnemar_test <- mcnemar.test(contingency_table, correct = TRUE)
    
    mcnemar_results <- rbind(mcnemar_results, data.frame(
      Model1 = model1,
      Model2 = model2,
      ChiSquare = mcnemar_test$statistic,
      p_value = mcnemar_test$p.value,
      stringsAsFactors = FALSE
    ))
  }
}

# Apply multiple testing correction
if(nrow(mcnemar_results) > 0) {
  mcnemar_results$p_bonferroni <- p.adjust(mcnemar_results$p_value, method = "bonferroni")
  mcnemar_results$p_fdr <- p.adjust(mcnemar_results$p_value, method = "fdr")
  mcnemar_results$significant_bonferroni <- mcnemar_results$p_bonferroni < 0.05
  mcnemar_results$significant_fdr <- mcnemar_results$p_fdr < 0.05
}

# Save McNemar results
save_with_timestamp(mcnemar_results, "mcnemar_test_results", output_dirs$statistical_tests)

# Friedman test for overall model ranking
auc_matrix <- matrix(NA, nrow = 5, ncol = 5)
rownames(auc_matrix) <- model_names

for(i in 1:5) {  # 5 folds
  for(j in 1:5) {  # 5 models
    model_name <- model_names[j]
    fold_data <- cv_results[[model_name]]$cv_predictions
    fold_indices <- which(fold_data$Resample == paste("Fold", sprintf("%02d", i)) | 
                            fold_data$Resample == paste("Fold", i))
    
    if(length(fold_indices) > 0) {
      fold_auc <- auc(roc(fold_data$obs[fold_indices], fold_data$Yes[fold_indices], 
                          levels = c("No", "Yes"), direction = "<", quiet = TRUE))
      auc_matrix[j, i] <- as.numeric(fold_auc)
    } else {
      auc_matrix[j, i] <- cv_performance$AUC[j]  # Use overall CV AUC as fallback
    }
  }
}

friedman_result <- friedman.test(auc_matrix)

# Save Friedman test results
friedman_summary <- data.frame(
  Test = "Friedman Test",
  ChiSquared = friedman_result$statistic,
  df = friedman_result$parameter,
  p_value = friedman_result$p.value,
  Interpretation = ifelse(friedman_result$p.value < 0.05, "Significant", "Not Significant")
)

save_with_timestamp(friedman_summary, "friedman_test_results", output_dirs$statistical_tests)

# Effect size calculations
effect_sizes <- data.frame()

for(combo in model_combinations) {
  model1 <- combo[1]
  model2 <- combo[2]
  
  auc1 <- cv_performance$AUC[cv_performance$Model == model1]
  auc2 <- cv_performance$AUC[cv_performance$Model == model2]
  
  set.seed(42)
  n_boot <- 1000
  
  pred1_data <- cv_results[[model1]]$cv_predictions
  pred2_data <- cv_results[[model2]]$cv_predictions
  
  boot_auc1 <- replicate(n_boot, {
    boot_idx <- sample(nrow(pred1_data), replace = TRUE)
    auc(roc(pred1_data$obs[boot_idx], pred1_data$Yes[boot_idx], 
            levels = c("No", "Yes"), direction = "<", quiet = TRUE))
  })
  
  boot_auc2 <- replicate(n_boot, {
    boot_idx <- sample(nrow(pred2_data), replace = TRUE)
    auc(roc(pred2_data$obs[boot_idx], pred2_data$Yes[boot_idx], 
            levels = c("No", "Yes"), direction = "<", quiet = TRUE))
  })
  
  pooled_sd <- sqrt((var(boot_auc1) + var(boot_auc2)) / 2)
  cohens_d <- (auc1 - auc2) / pooled_sd
  
  effect_sizes <- rbind(effect_sizes, data.frame(
    Model1 = model1,
    Model2 = model2,
    AUC_Difference = auc1 - auc2,
    Cohens_D = cohens_d,
    Effect_Size_Interpretation = case_when(
      abs(cohens_d) < 0.2 ~ "Negligible",
      abs(cohens_d) < 0.5 ~ "Small",
      abs(cohens_d) < 0.8 ~ "Medium",
      TRUE ~ "Large"
    ),
    stringsAsFactors = FALSE
  ))
}

save_with_timestamp(effect_sizes, "effect_sizes", output_dirs$statistical_tests)

# ============================================================================
# 7. COMPREHENSIVE VISUALIZATIONS (ALL REVIEWER REQUIREMENTS)
# ============================================================================

cat("\n=== CREATING COMPREHENSIVE VISUALIZATIONS ===\n")

# Define consistent color scheme
model_colors <- c("#E31A1C", "#1F78B4", "#33A02C", "#FF7F00", "#6A3D9A")
names(model_colors) <- model_names

# ============================================================================
# 7.1 DATA DISTRIBUTION PLOTS (REVIEWER 5 REQUEST)
# ============================================================================

cat("Creating data distribution plots...\n")

# Class distribution plot
class_dist_data <- data.frame(
  Class = names(table(synthetic_data$Malaria)),
  Count = as.numeric(table(synthetic_data$Malaria)),
  Percentage = as.numeric(prop.table(table(synthetic_data$Malaria)) * 100)
)

class_dist_plot <- ggplot(class_dist_data, aes(x = Class, y = Count, fill = Class)) +
  geom_col(alpha = 0.8, width = 0.6) +
  geom_text(aes(label = paste0(Count, "\n(", round(Percentage, 1), "%)")), 
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("No" = "#2E86C1", "Yes" = "#E74C3C")) +
  labs(
    title = "Class Distribution: Malaria Dataset",
    subtitle = paste0("Dataset shows ", round(class_dist_data$Percentage[2], 1), 
                      "% prevalence (", class_dist_data$Count[2], " positive cases)"),
    x = "Malaria Status",
    y = "Number of Samples"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "none"
  )

# Feature distribution plots by class
continuous_vars <- c("Age", "Temperature", "Rainfall")
cont_plot_list <- list()

for(var in continuous_vars) {
  cont_plot_list[[var]] <- ggplot(synthetic_data, aes(x = .data[[var]], fill = Malaria)) +
    geom_histogram(alpha = 0.7, position = "identity", bins = 30) +
    scale_fill_manual(values = c("No" = "#2E86C1", "Yes" = "#E74C3C")) +
    labs(
      title = paste("Distribution of", var, "by Malaria Status"),
      x = var,
      y = "Frequency"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      legend.position = "bottom"
    )
}

# Box plots for continuous variables
box_plot_list <- list()
for(var in continuous_vars) {
  box_plot_list[[var]] <- ggplot(synthetic_data, aes(x = Malaria, y = .data[[var]], fill = Malaria)) +
    geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
    geom_jitter(width = 0.2, alpha = 0.1, size = 0.5) +
    scale_fill_manual(values = c("No" = "#2E86C1", "Yes" = "#E74C3C")) +
    labs(
      title = paste(var, "by Malaria Status"),
      x = "Malaria Status",
      y = var
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      legend.position = "none"
    )
}

# Categorical variables distribution
categorical_vars <- c("Fever", "Chills", "Fatigue")
cat_plot_list <- list()

for(var in categorical_vars) {
  cont_table <- table(synthetic_data[[var]], synthetic_data$Malaria)
  cont_df <- as.data.frame(cont_table)
  names(cont_df) <- c(var, "Malaria", "Count")
  
  cont_df <- cont_df %>%
    group_by(Malaria) %>%
    mutate(Percentage = Count / sum(Count) * 100) %>%
    ungroup()
  
  cat_plot_list[[var]] <- ggplot(cont_df, aes(x = .data[[var]], y = Percentage, fill = Malaria)) +
    geom_col(position = position_dodge(width = 0.8), alpha = 0.8) +
    geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
              position = position_dodge(width = 0.8), vjust = -0.3, size = 3.5) +
    scale_fill_manual(values = c("No" = "#2E86C1", "Yes" = "#E74C3C")) +
    labs(
      title = paste(var, "Distribution by Malaria Status"),
      x = var,
      y = "Percentage within Malaria Group"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      legend.position = "bottom"
    )
}

# Combine all distribution plots
combined_distributions <- plot_grid(
  class_dist_plot,
  plot_grid(plotlist = cont_plot_list, ncol = 3, labels = c("B", "C", "D")),
  plot_grid(plotlist = box_plot_list, ncol = 3, labels = c("E", "F", "G")),
  plot_grid(plotlist = cat_plot_list, ncol = 3, labels = c("H", "I", "J")),
  ncol = 1,
  labels = c("A", "", "", ""),
  rel_heights = c(0.8, 1, 1, 1)
)

ggsave(file.path(output_dirs$plots, "Data_Distributions_Complete.png"), 
       combined_distributions, width = 16, height = 20, dpi = 300)
ggsave(file.path(output_dirs$plots, "Data_Distributions_Complete.pdf"), 
       combined_distributions, width = 16, height = 20)

# ============================================================================
# 7.2 FEATURE IMPORTANCE PLOTS (REVIEWER 5 REQUEST)
# ============================================================================

cat("Creating feature importance plots...\n")

# Extract feature importance from trained models
feature_importance_data <- list()

# XGBoost feature importance
if("XGBoost" %in% names(model_objects)) {
  xgb_importance <- xgb.importance(model = model_objects[["XGBoost"]]$finalModel)
  feature_importance_data[["XGBoost"]] <- data.frame(
    Feature = xgb_importance$Feature,
    Importance = xgb_importance$Gain,
    Model = "XGBoost",
    Metric = "Gain"
  )
}

# Random Forest feature importance
if("Random_Forest" %in% names(model_objects)) {
  rf_importance <- importance(model_objects[["Random_Forest"]]$finalModel)
  feature_importance_data[["Random_Forest"]] <- data.frame(
    Feature = rownames(rf_importance),
    Importance = rf_importance[, "MeanDecreaseGini"],
    Model = "Random Forest",
    Metric = "Mean Decrease Gini"
  )
}

# Enhanced Bayesian LR feature importance
if("Enhanced_Bayesian_LR" %in% names(cv_results) && 
   "feature_importance" %in% names(cv_results[["Enhanced_Bayesian_LR"]])) {
  
  bayes_importance <- cv_results[["Enhanced_Bayesian_LR"]]$feature_importance %>%
    filter(!grepl("Intercept|sd_|r_", Feature)) %>%
    filter(abs(Mean_Effect) > 0.01) %>%
    arrange(desc(abs(Mean_Effect))) %>%
    head(10)
  
  feature_importance_data[["Enhanced_Bayesian_LR"]] <- data.frame(
    Feature = gsub("b_", "", bayes_importance$Feature),
    Importance = abs(bayes_importance$Mean_Effect),
    Model = "Enhanced Bayesian LR",
    Metric = "Posterior Mean (Absolute)"
  )
}

# Create individual feature importance plots
importance_plots <- list()

for(model_name in names(feature_importance_data)) {
  data <- feature_importance_data[[model_name]]
  
  # Normalize importance to 0-100 scale for comparison
  data$Importance_Normalized <- (data$Importance / max(data$Importance)) * 100
  
  importance_plots[[model_name]] <- ggplot(data, 
                                           aes(x = reorder(Feature, Importance_Normalized), 
                                               y = Importance_Normalized)) +
    geom_col(fill = case_when(
      model_name == "XGBoost" ~ "#FF7F00",
      model_name == "Random_Forest" ~ "#33A02C", 
      TRUE ~ "#6A3D9A"
    ), alpha = 0.8) +
    geom_text(aes(label = round(Importance_Normalized, 1)), 
              hjust = -0.1, size = 3.5) +
    coord_flip() +
    labs(
      title = paste("Feature Importance:", model_name),
      subtitle = paste("Metric:", unique(data$Metric)),
      x = "Features",
      y = "Relative Importance (%)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 10)
    ) +
    ylim(0, max(data$Importance_Normalized) * 1.1)
}

# Combined feature importance plot
if(length(importance_plots) > 0) {
  combined_importance <- plot_grid(plotlist = importance_plots, 
                                   ncol = length(importance_plots),
                                   labels = LETTERS[1:length(importance_plots)])
  
  ggsave(file.path(output_dirs$plots, "Feature_Importance_Comparison.png"), 
         combined_importance, width = 5 * length(importance_plots), height = 8, dpi = 300)
  ggsave(file.path(output_dirs$plots, "Feature_Importance_Comparison.pdf"), 
         combined_importance, width = 5 * length(importance_plots), height = 8)
}

# ============================================================================
# 7.3 MODEL COMPARISON HEATMAP (REVIEWER 5 REQUEST)
# ============================================================================

cat("Creating model comparison heatmap...\n")

# Prepare data for heatmap using base R instead of tibble
heatmap_data <- cv_performance %>%
  select(Model, Accuracy, Sensitivity, Specificity, AUC, F1)

# Convert to matrix with rownames (base R approach)
heatmap_matrix <- as.matrix(heatmap_data[, -1])  # Remove Model column
rownames(heatmap_matrix) <- heatmap_data$Model   # Set rownames

# Create heatmap using pheatmap
model_heatmap <- pheatmap(
  t(heatmap_matrix),  # Transpose so models are columns
  display_numbers = round(t(heatmap_matrix), 3),  # Show actual values
  number_color = "white",
  fontsize_number = 10,
  cluster_rows = FALSE,
  cluster_cols = TRUE,
  color = colorRampPalette(c("#3498DB", "#F8F9FA", "#E74C3C"))(100),
  main = "Model Performance Comparison Heatmap",
  fontsize = 12,
  fontsize_row = 11,
  fontsize_col = 10,
  angle_col = 45,
  cellwidth = 60,
  cellheight = 40,
  filename = file.path(output_dirs$plots, "Model_Comparison_Heatmap.png"),
  width = 12,
  height = 8
)

# Save PDF version
pdf(file.path(output_dirs$plots, "Model_Comparison_Heatmap.pdf"), width = 12, height = 8)
print(model_heatmap)
dev.off()

# ============================================================================
# 7.4 ROC CURVES
# ============================================================================

cat("Creating ROC curves...\n")

# Calculate ROC curves for all models
roc_data_list <- list()
auc_data <- data.frame()

for(i in seq_along(model_names)) {
  model_name <- model_names[i]
  pred_data <- cv_results[[model_name]]$cv_predictions
  
  roc_obj <- roc(pred_data$obs, pred_data$Yes, levels = c("No", "Yes"), 
                 direction = "<", quiet = TRUE)
  
  roc_df <- data.frame(
    FPR = 1 - roc_obj$specificities,
    TPR = roc_obj$sensitivities,
    Model = model_name,
    AUC = as.numeric(auc(roc_obj)),
    stringsAsFactors = FALSE
  )
  
  roc_data_list[[model_name]] <- roc_df
  
  auc_data <- rbind(auc_data, data.frame(
    Model = model_name,
    AUC = as.numeric(auc(roc_obj)),
    Color = model_colors[model_name],
    stringsAsFactors = FALSE
  ))
}

roc_combined <- do.call(rbind, roc_data_list)

roc_plot <- ggplot(roc_combined, aes(x = FPR, y = TPR, color = Model)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", 
              color = "gray50", size = 0.8) +
  scale_color_manual(values = model_colors) +
  labs(
    title = "ROC Curves with Statistical Confidence",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 11, face = "bold"),
    legend.position = "bottom"
  )

# Add AUC annotations
auc_labels <- auc_data %>%
  mutate(
    Label = sprintf("%s: %.3f", gsub("_", " ", Model), AUC),
    x_pos = 0.6,
    y_pos = 0.1 + (seq_len(nrow(auc_data)) - 1) * 0.06
  )

roc_plot <- roc_plot +
  annotate("text", x = auc_labels$x_pos, y = auc_labels$y_pos,
           label = auc_labels$Label, hjust = 0, size = 3.5,
           color = auc_labels$Color, fontface = "bold")

ggsave(file.path(output_dirs$plots, "ROC_Curves.png"), 
       roc_plot, width = 10, height = 8, dpi = 300)
ggsave(file.path(output_dirs$plots, "ROC_Curves.pdf"), 
       roc_plot, width = 10, height = 8)

# ============================================================================
# 7.5 PRECISION-RECALL CURVES (REVIEWER 5 SPECIFIC REQUEST)
# ============================================================================

cat("Creating Precision-Recall curves (Reviewer 5 request)...\n")

# Function to calculate precision-recall curve data
calculate_pr_curve <- function(actual, probabilities) {
  actual <- factor(actual, levels = c("No", "Yes"))
  
  pr_result <- pr.curve(
    scores.class0 = probabilities[actual == "Yes"],
    scores.class1 = probabilities[actual == "No"],
    curve = TRUE
  )
  
  return(list(
    curve_data = data.frame(
      Recall = pr_result$curve[, 1],
      Precision = pr_result$curve[, 2]
    ),
    auprc = pr_result$auc.integral
  ))
}

# Calculate PR curves for all models
pr_data_list <- list()
auprc_summary <- data.frame()

for(i in seq_along(model_names)) {
  model_name <- model_names[i]
  pred_data <- cv_results[[model_name]]$cv_predictions
  
  pr_result <- calculate_pr_curve(pred_data$obs, pred_data$Yes)
  
  pr_df <- pr_result$curve_data
  pr_df$Model <- model_name
  pr_df$AUPRC <- pr_result$auprc
  
  pr_data_list[[model_name]] <- pr_df
  
  auprc_summary <- rbind(auprc_summary, data.frame(
    Model = model_name,
    AUPRC = pr_result$auprc,
    Color = model_colors[model_name],
    stringsAsFactors = FALSE
  ))
}

pr_combined <- do.call(rbind, pr_data_list)

# Create Precision-Recall plot
pr_plot <- ggplot(pr_combined, aes(x = Recall, y = Precision, color = Model)) +
  geom_line(size = 1.2, alpha = 0.8) +
  scale_color_manual(values = model_colors) +
  labs(
    title = "Precision-Recall Curves for Malaria Detection",
    subtitle = paste0("Class imbalance: ", round(prop.table(table(synthetic_data$Malaria))[2] * 100, 1), 
                      "% malaria prevalence (", sum(synthetic_data$Malaria == "Yes"), " positive cases)"),
    x = "Recall (Sensitivity)",
    y = "Precision (Positive Predictive Value)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 11, face = "bold"),
    legend.position = "bottom"
  ) +
  xlim(0, 1) + ylim(0, 1)

# Add AUPRC annotations
auprc_labels <- auprc_summary %>%
  arrange(desc(AUPRC)) %>%
  mutate(
    Label = sprintf("%s: %.3f", gsub("_", " ", Model), AUPRC),
    x_pos = 0.02,
    y_pos = 0.95 - (seq_len(nrow(auprc_summary)) - 1) * 0.07
  )

pr_plot <- pr_plot +
  annotate("text", x = auprc_labels$x_pos, y = auprc_labels$y_pos,
           label = auprc_labels$Label, hjust = 0, size = 3.5,
           color = auprc_labels$Color, fontface = "bold")

# Add baseline (random classifier performance for imbalanced data)
baseline_precision <- sum(synthetic_data$Malaria == "Yes") / nrow(synthetic_data)
pr_plot <- pr_plot +
  geom_hline(yintercept = baseline_precision, linetype = "dashed", 
             color = "gray50", size = 0.8, alpha = 0.7) +
  annotate("text", x = 0.5, y = baseline_precision + 0.02, 
           label = paste0("Random Classifier (", round(baseline_precision, 3), ")"), 
           hjust = 0.5, size = 3, color = "gray50")

ggsave(file.path(output_dirs$plots, "Precision_Recall_Curves.png"), 
       pr_plot, width = 10, height = 8, dpi = 300)
ggsave(file.path(output_dirs$plots, "Precision_Recall_Curves.pdf"), 
       pr_plot, width = 10, height = 8)

# ============================================================================
# 7.6 ROC vs PRECISION-RECALL COMPARISON
# ============================================================================

cat("Creating ROC vs PR comparison plot...\n")

# ROC subplot
roc_subplot <- ggplot(roc_combined, aes(x = FPR, y = TPR, color = Model)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", 
              color = "gray50", size = 0.8) +
  scale_color_manual(values = model_colors) +
  labs(
    title = "ROC Curves",
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    legend.position = "none"
  )

# PR subplot
pr_subplot <- ggplot(pr_combined, aes(x = Recall, y = Precision, color = Model)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_hline(yintercept = baseline_precision, linetype = "dashed", 
             color = "gray50", size = 0.8, alpha = 0.7) +
  scale_color_manual(values = model_colors) +
  labs(
    title = "Precision-Recall Curves",
    x = "Recall (Sensitivity)",
    y = "Precision (PPV)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    legend.position = "none"
  )

# Combined ROC vs PR plot
roc_vs_pr_comparison <- plot_grid(
  roc_subplot, pr_subplot, 
  ncol = 2, 
  labels = c("A", "B"),
  align = "h"
)

# Add overall title and legend
roc_vs_pr_final <- plot_grid(
  roc_vs_pr_comparison,
  get_legend(pr_plot + theme(legend.position = "bottom")),
  ncol = 1,
  rel_heights = c(0.9, 0.1)
)

# Add title
title_text <- textGrob("ROC vs Precision-Recall Curves: Class Imbalance Impact", 
                       gp = gpar(fontsize = 14, fontface = "bold"))

roc_vs_pr_final <- plot_grid(
  title_text,
  roc_vs_pr_final,
  ncol = 1,
  rel_heights = c(0.05, 0.95)
)

ggsave(file.path(output_dirs$plots, "ROC_vs_PR_Comparison.png"), 
       roc_vs_pr_final, width = 14, height = 8, dpi = 300)
ggsave(file.path(output_dirs$plots, "ROC_vs_PR_Comparison.pdf"), 
       roc_vs_pr_final, width = 14, height = 8)


# ============================================================================
# 7.6B ENHANCED THRESHOLD OPTIMIZATION CURVES
# ============================================================================

cat("Creating enhanced threshold optimization curves...\n")

# First, CREATE the threshold_analysis object
# Analyze precision-recall trade-off at different thresholds for best model
best_auc_model <- cv_performance$Model[which.max(cv_performance$AUC)]
best_model_data <- cv_results[[best_auc_model]]$cv_predictions

# Calculate precision and recall at different thresholds
thresholds <- seq(0.1, 0.9, by = 0.05)
threshold_analysis <- data.frame()

for(threshold in thresholds) {
  pred_class <- ifelse(best_model_data$Yes > threshold, "Yes", "No")
  pred_class <- factor(pred_class, levels = c("No", "Yes"))
  actual <- factor(best_model_data$obs, levels = c("No", "Yes"))
  
  cm <- confusionMatrix(pred_class, actual, positive = "Yes")
  
  threshold_analysis <- rbind(threshold_analysis, data.frame(
    Threshold = threshold,
    Precision = as.numeric(cm$byClass["Pos Pred Value"]),
    Recall = as.numeric(cm$byClass["Sensitivity"]),
    F1 = as.numeric(cm$byClass["F1"]),
    Cost = (cost_fn * cm$table[1,2]) + (cost_fp * cm$table[2,1])
  ))
}

# Remove rows with NA values
threshold_analysis <- threshold_analysis[complete.cases(threshold_analysis), ]

# Get optimal threshold
optimal_threshold <- cv_results[[best_auc_model]]$optimal_threshold

# Enhanced threshold analysis with cost overlay
threshold_enhanced_plot <- threshold_analysis %>%
  gather(key = "Metric", value = "Value", Precision, Recall, F1) %>%
  ggplot(aes(x = Threshold)) +
  geom_line(aes(y = Value, color = Metric), linewidth = 1.2) +
  geom_point(aes(y = Value, color = Metric), size = 2) +
  scale_color_manual(values = c("Precision" = "#E74C3C", "Recall" = "#3498DB", "F1" = "#27AE60")) +
  geom_vline(xintercept = optimal_threshold, linetype = "dashed", 
             color = "black", alpha = 0.7, linewidth = 1) +
  annotate("text", x = optimal_threshold + 0.05, y = 0.9, 
           label = paste0("Optimal threshold = ", round(optimal_threshold, 3)), 
           angle = 90, hjust = 1) +
  labs(
    title = paste("Enhanced Threshold Optimization:", gsub("_", " ", best_auc_model)),
    subtitle = "Clinical decision threshold selection with cost consideration",
    x = "Decision Threshold",
    y = "Performance Score",
    color = "Metric"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom"
  ) +
  ylim(0, 1)

# Add cost overlay as secondary axis
threshold_cost_plot <- ggplot(threshold_analysis, aes(x = Threshold)) +
  geom_line(aes(y = Cost/max(Cost)), color = "#9B59B6", linewidth = 1.2) +
  geom_vline(xintercept = optimal_threshold, linetype = "dashed", alpha = 0.7) +
  scale_y_continuous(name = "Normalized Cost", 
                     sec.axis = sec_axis(~.*max(threshold_analysis$Cost), name = "Actual Cost")) +
  labs(x = "Threshold", title = "Cost Function") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
  )

# Load required library
suppressMessages(library(cowplot))

# Combine plots
combined_threshold <- plot_grid(
  threshold_enhanced_plot, 
  threshold_cost_plot,
  ncol = 1,
  rel_heights = c(0.7, 0.3),
  labels = c("A", "B")
)

# Save plots
ggsave(file.path(output_dirs$plots, "Enhanced_Threshold_Optimization.png"), 
       combined_threshold, width = 12, height = 10, dpi = 300)
ggsave(file.path(output_dirs$plots, "Enhanced_Threshold_Optimization.pdf"), 
       combined_threshold, width = 12, height = 10)

# Save threshold analysis data
save_with_timestamp(threshold_analysis, "threshold_analysis_data", output_dirs$tables)

cat("✓ Enhanced threshold optimization curves completed\n")

# ============================================================================
# 7.7 ENHANCED PERFORMANCE COMPARISON
# ============================================================================

cat("Creating enhanced performance comparison...\n")

performance_with_ci <- cv_performance %>%
  select(Model, Accuracy, Accuracy_CI_Lower, Accuracy_CI_Upper,
         Sensitivity, Sensitivity_CI_Lower, Sensitivity_CI_Upper,
         Specificity, Specificity_CI_Lower, Specificity_CI_Upper,
         AUC, AUC_CI_Lower, AUC_CI_Upper) %>%
  gather(key = "Metric_Type", value = "Value", 
         Accuracy, Sensitivity, Specificity, AUC) %>%
  mutate(
    CI_Lower = case_when(
      Metric_Type == "Accuracy" ~ Accuracy_CI_Lower,
      Metric_Type == "Sensitivity" ~ Sensitivity_CI_Lower,
      Metric_Type == "Specificity" ~ Specificity_CI_Lower,
      Metric_Type == "AUC" ~ AUC_CI_Lower
    ),
    CI_Upper = case_when(
      Metric_Type == "Accuracy" ~ Accuracy_CI_Upper,
      Metric_Type == "Sensitivity" ~ Sensitivity_CI_Upper,
      Metric_Type == "Specificity" ~ Specificity_CI_Upper,
      Metric_Type == "AUC" ~ AUC_CI_Upper
    ),
    Model_Clean = gsub("_", " ", Model)
  )

performance_comparison_enhanced <- ggplot(performance_with_ci, 
                                          aes(x = reorder(Model_Clean, Value), 
                                              y = Value, fill = Metric_Type)) +
  geom_col(position = position_dodge(width = 0.8), alpha = 0.8, width = 0.7) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                position = position_dodge(width = 0.8), width = 0.2, alpha = 0.8) +
  geom_text(aes(label = round(Value, 3)), 
            position = position_dodge(width = 0.8), vjust = -0.5, size = 3) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  labs(
    title = "Model Performance Comparison with 95% Confidence Intervals",
    subtitle = "Error bars represent bootstrap confidence intervals",
    x = "Model",
    y = "Performance Score",
    fill = "Metric"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  coord_flip() +
  ylim(0, 1.1)

ggsave(file.path(output_dirs$plots, "Performance_Comparison_Enhanced.png"), 
       performance_comparison_enhanced, width = 14, height = 10, dpi = 300)
ggsave(file.path(output_dirs$plots, "Performance_Comparison_Enhanced.pdf"), 
       performance_comparison_enhanced, width = 14, height = 10)

# ============================================================================
# 7.8 FEATURE CORRELATION MATRIX
# ============================================================================

cat("Creating feature correlation matrix...\n")

# Prepare data for correlation
corr_data <- synthetic_data %>%
  select(all_of(features)) %>%
  mutate(
    Fever = as.numeric(Fever) - 1,
    Chills = as.numeric(Chills) - 1,
    Fatigue = as.numeric(Fatigue) - 1
  )

# Calculate correlation matrix
corr_matrix <- cor(corr_data, use = "complete.obs")

# Create correlation plot
png(file.path(output_dirs$plots, "Feature_Correlation_Matrix.png"), 
    width = 10, height = 10, units = "in", res = 300)
corrplot(corr_matrix, 
         method = "color",
         type = "upper",
         order = "hclust",
         tl.cex = 1.2,
         tl.col = "black",
         addCoef.col = "white",
         number.cex = 0.8,
         col = colorRampPalette(c("#3498DB", "white", "#E74C3C"))(100),
         title = "Feature Correlation Matrix",
         mar = c(0,0,2,0))
dev.off()

# PDF version
pdf(file.path(output_dirs$plots, "Feature_Correlation_Matrix.pdf"), width = 10, height = 10)
corrplot(corr_matrix, 
         method = "color",
         type = "upper",
         order = "hclust",
         tl.cex = 1.2,
         tl.col = "black",
         addCoef.col = "white",
         number.cex = 0.8,
         col = colorRampPalette(c("#3498DB", "white", "#E74C3C"))(100),
         title = "Feature Correlation Matrix")
dev.off()

# ============================================================================
# 7.9 COST-BENEFIT ANALYSIS
# ============================================================================

cat("Creating cost-benefit analysis plot...\n")

cost_comparison <- cv_performance %>%
  select(Model, Cost, Optimal_Threshold) %>%
  mutate(Model_Clean = gsub("_", " ", Model)) %>%
  arrange(Cost)

cost_plot <- ggplot(cost_comparison, aes(x = reorder(Model_Clean, -Cost), y = Cost)) +
  geom_col(fill = "#E74C3C", alpha = 0.8, width = 0.6) +
  geom_text(aes(label = paste0("$", Cost, "\n(T=", round(Optimal_Threshold, 2), ")")), 
            vjust = -0.3, size = 3.5, fontface = "bold") +
  labs(
    title = "Cost Analysis: Clinical Decision Thresholds",
    subtitle = paste0("Cost function: ", cost_fn, "×FN + ", cost_fp, "×FP (FN=False Negative, FP=False Positive)"),
    x = "Model",
    y = "Total Expected Cost",
    caption = "T = Optimal Decision Threshold"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(file.path(output_dirs$plots, "Cost_Benefit_Analysis.png"), 
       cost_plot, width = 12, height = 8, dpi = 300)
ggsave(file.path(output_dirs$plots, "Cost_Benefit_Analysis.pdf"), 
       cost_plot, width = 12, height = 8)

# ============================================================================
# 7.10 TRAINING TIME COMPARISON
# ============================================================================

cat("Creating training time comparison...\n")

training_time_plot <- ggplot(training_times, aes(x = reorder(Model, Training_Time_Minutes), 
                                                 y = Training_Time_Minutes)) +
  geom_col(fill = "#9B59B6", alpha = 0.8, width = 0.6) +
  geom_text(aes(label = paste0(round(Training_Time_Minutes, 1), " min")), 
            hjust = -0.1, size = 4, fontface = "bold") +
  coord_flip() +
  labs(
    title = "Model Training Time Comparison",
    subtitle = "Computational efficiency analysis",
    x = "Model",
    y = "Training Time (Minutes)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5)
  ) +
  ylim(0, max(training_times$Training_Time_Minutes) * 1.2)  # CHANGED: xlim to ylim

ggsave(file.path(output_dirs$plots, "Training_Time_Comparison.png"), 
       training_time_plot, width = 10, height = 8, dpi = 300)
ggsave(file.path(output_dirs$plots, "Training_Time_Comparison.pdf"), 
       training_time_plot, width = 10, height = 8)


# ============================================================================
# 7.11 CONFUSION MATRICES VISUALIZATION (REVIEWER 5 REQUEST)
# ============================================================================

cat("Creating confusion matrices visualization...\n")

# Create confusion matrices for all models
cm_plot_list <- list()
cm_data_list <- list()

for(model_name in model_names) {
  pred_data <- cv_results[[model_name]]$cv_predictions
  threshold <- cv_results[[model_name]]$optimal_threshold
  
  # Apply threshold
  pred_class <- ifelse(pred_data$Yes > threshold, "Yes", "No")
  pred_class <- factor(pred_class, levels = c("No", "Yes"))
  actual <- factor(pred_data$obs, levels = c("No", "Yes"))
  
  # Create confusion matrix
  cm <- confusionMatrix(pred_class, actual, positive = "Yes")
  cm_table <- as.data.frame(cm$table)
  
  # Store confusion matrix data
  cm_data_list[[model_name]] <- cm_table
  
  # Create heatmap visualization
  cm_plot_list[[model_name]] <- ggplot(cm_table, aes(x = Reference, y = Prediction, fill = Freq)) +
    geom_tile(alpha = 0.8, color = "white", size = 1) +
    geom_text(aes(label = paste0(Freq, "\n(", round(Freq/sum(Freq)*100, 1), "%)")), 
              size = 4, fontface = "bold", color = "white") +
    scale_fill_gradient(low = "#3498DB", high = "#E74C3C") +
    scale_x_discrete(position = "top") +
    labs(
      title = gsub("_", " ", model_name),
      subtitle = paste("Acc:", round(cm$overall["Accuracy"], 3), 
                       "| Sens:", round(cm$byClass["Sensitivity"], 3),
                       "| Spec:", round(cm$byClass["Specificity"], 3)),
      x = "Actual",
      y = "Predicted"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5),
      legend.position = "none",
      axis.text = element_text(size = 10, face = "bold")
    )
}

# Combine all confusion matrices
combined_cm <- plot_grid(plotlist = cm_plot_list, 
                         ncol = 3, nrow = 2,
                         labels = paste("Model", 1:5))

# Add overall title
cm_title <- textGrob("Confusion Matrices: All Models with Optimal Thresholds", 
                     gp = gpar(fontsize = 16, fontface = "bold"))

combined_cm_final <- plot_grid(
  cm_title,
  combined_cm,
  ncol = 1,
  rel_heights = c(0.05, 0.95)
)

ggsave(file.path(output_dirs$plots, "Confusion_Matrices_All_Models.png"), 
       combined_cm_final, width = 15, height = 10, dpi = 300)
ggsave(file.path(output_dirs$plots, "Confusion_Matrices_All_Models.pdf"), 
       combined_cm_final, width = 15, height = 10)

# Save confusion matrix data
save_with_timestamp(do.call(rbind, lapply(names(cm_data_list), function(x) {
  data.frame(Model = x, cm_data_list[[x]])
})), "confusion_matrices_data", output_dirs$tables)

cat("✓ Confusion matrices visualization completed\n")

# ============================================================================
# 7.11 COMBINED ANALYSIS PLOTS
# ============================================================================

cat("Creating combined analysis plots...\n")

# Combined ROC and Performance
combined_plot <- plot_grid(
  roc_plot + theme(legend.position = "bottom"),
  performance_comparison_enhanced + theme(legend.position = "bottom"),
  ncol = 2
)

ggsave(file.path(output_dirs$plots, "Combined_Analysis.png"), 
       combined_plot, width = 16, height = 8, dpi = 300)
ggsave(file.path(output_dirs$plots, "Combined_Analysis.pdf"), 
       combined_plot, width = 16, height = 8)

# Save PR performance table
pr_performance_table <- auprc_summary %>%
  left_join(
    cv_performance %>% select(Model, Sensitivity, Specificity, AUC, F1, 
                              Optimal_Threshold), 
    by = "Model"
  ) %>%
  arrange(desc(AUPRC)) %>%
  mutate(
    Rank_AUPRC = 1:n(),
    Rank_AUC = rank(-AUC),
    Model_Clean = gsub("_", " ", Model)
  ) %>%
  select(Rank_AUPRC, Model_Clean, AUPRC, AUC, F1, Sensitivity, 
         Specificity, Optimal_Threshold)

save_with_timestamp(pr_performance_table, "precision_recall_performance", output_dirs$tables)

cat("✓ All visualizations created and saved!\n")



# ============================================================================
# 8. COMPREHENSIVE RESULTS PRESENTATION AND SAVING
# ============================================================================

cat("\n=== COMPREHENSIVE RESULTS PRESENTATION ===\n")

# Create formatted results tables
results_table <- cv_performance %>%
  mutate(
    Accuracy_Formatted = sprintf("%.3f (%.3f-%.3f)", Accuracy, Accuracy_CI_Lower, Accuracy_CI_Upper),
    Sensitivity_Formatted = sprintf("%.3f (%.3f-%.3f)", Sensitivity, Sensitivity_CI_Lower, Sensitivity_CI_Upper),
    Specificity_Formatted = sprintf("%.3f (%.3f-%.3f)", Specificity, Specificity_CI_Lower, Specificity_CI_Upper),
    AUC_Formatted = sprintf("%.3f (%.3f-%.3f)", AUC, AUC_CI_Lower, AUC_CI_Upper),
    F1_Formatted = sprintf("%.3f (%.3f-%.3f)", F1, F1_CI_Lower, F1_CI_Upper),
    AUPRC_Formatted = sprintf("%.3f (%.3f-%.3f)", AUPRC, AUPRC_CI_Lower, AUPRC_CI_Upper)
  ) %>%
  select(Model, Accuracy_Formatted, Sensitivity_Formatted, Specificity_Formatted, 
         AUC_Formatted, AUPRC_Formatted, F1_Formatted, Cost, Optimal_Threshold, Training_Time_Minutes) %>%
  arrange(desc(cv_performance$AUC))

save_with_timestamp(results_table, "formatted_results_table", output_dirs$tables)

# Clinical significance assessment
clinical_thresholds <- list(
  auc_difference = 0.02,
  sensitivity_difference = 0.05,
  specificity_difference = 0.05
)

clinical_significance <- data.frame()

for(combo in model_combinations) {
  model1 <- combo[1]
  model2 <- combo[2]
  
  perf1 <- cv_performance[cv_performance$Model == model1, ]
  perf2 <- cv_performance[cv_performance$Model == model2, ]
  
  auc_diff <- abs(perf1$AUC - perf2$AUC)
  sens_diff <- abs(perf1$Sensitivity - perf2$Sensitivity)
  spec_diff <- abs(perf1$Specificity - perf2$Specificity)
  
  clinical_significance <- rbind(clinical_significance, data.frame(
    Model1 = model1,
    Model2 = model2,
    AUC_Difference = perf1$AUC - perf2$AUC,
    Clinically_Significant_AUC = auc_diff >= clinical_thresholds$auc_difference,
    Sensitivity_Difference = perf1$Sensitivity - perf2$Sensitivity,
    Clinically_Significant_Sensitivity = sens_diff >= clinical_thresholds$sensitivity_difference,
    Overall_Clinical_Significance = (auc_diff >= clinical_thresholds$auc_difference) | 
      (sens_diff >= clinical_thresholds$sensitivity_difference),
    stringsAsFactors = FALSE
  ))
}

save_with_timestamp(clinical_significance, "clinical_significance_assessment", output_dirs$tables)

# Final model ranking
final_ranking <- cv_performance %>%
  arrange(desc(AUC)) %>%
  mutate(
    Rank = 1:n(),
    Statistical_Support = "Bootstrap CI",
    Clinical_Relevance = case_when(
      AUC >= 0.9 ~ "Excellent",
      AUC >= 0.8 ~ "Good",
      AUC >= 0.7 ~ "Fair",
      TRUE ~ "Poor"
    )
  ) %>%
  select(Rank, Model, AUC, AUC_CI_Lower, AUC_CI_Upper, AUPRC, Sensitivity, Cost, 
         Statistical_Support, Clinical_Relevance, Training_Time_Minutes)

save_with_timestamp(final_ranking, "final_model_ranking", output_dirs$tables)

# ============================================================================
# 9. CREATE COMPREHENSIVE EXCEL WORKBOOK
# ============================================================================

cat("Creating comprehensive Excel workbook...\n")

wb <- createWorkbook()

# Add sheets
addWorksheet(wb, "Summary")
addWorksheet(wb, "Model_Performance")
addWorksheet(wb, "Precision_Recall")
addWorksheet(wb, "Statistical_Tests")
addWorksheet(wb, "Effect_Sizes") 
addWorksheet(wb, "Clinical_Significance")
addWorksheet(wb, "Training_Times")
addWorksheet(wb, "Data_Summary")

# Write data to sheets
writeData(wb, "Summary", final_ranking)
writeData(wb, "Model_Performance", cv_performance)
writeData(wb, "Precision_Recall", pr_performance_table)
writeData(wb, "Statistical_Tests", mcnemar_results)
writeData(wb, "Effect_Sizes", effect_sizes)
writeData(wb, "Clinical_Significance", clinical_significance)
writeData(wb, "Training_Times", training_times)
writeData(wb, "Data_Summary", data_summary)

# Save Excel file
excel_filename <- paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_Complete_Malaria_ML_Results.xlsx")
saveWorkbook(wb, file.path(output_dirs$reports, excel_filename), overwrite = TRUE)

cat("✓ Excel workbook saved:", excel_filename, "\n")

# ============================================================================
# 10. SAVE ANALYSIS LOG AND SESSION INFO
# ============================================================================

# Complete analysis log
analysis_log$end_time <- Sys.time()
analysis_log$total_runtime <- difftime(analysis_log$end_time, analysis_log$start_time, units = "hours")
analysis_log$models_trained <- model_names
analysis_log$best_model_auc <- cv_performance$Model[which.max(cv_performance$AUC)]
analysis_log$best_model_auprc <- pr_performance_table$Model_Clean[which.max(pr_performance_table$AUPRC)]
analysis_log$files_created <- list.files(output_base_dir, recursive = TRUE)

save_with_timestamp(analysis_log, "analysis_log", output_dirs$raw_results, "rds")

# Create visualization summary
visualization_summary <- data.frame(
  Plot_Name = c(
    "Data_Distributions_Complete",
    "Feature_Importance_Comparison", 
    "Model_Comparison_Heatmap",
    "ROC_Curves",
    "Precision_Recall_Curves",
    "ROC_vs_PR_Comparison",
    "Performance_Comparison_Enhanced",
    "Feature_Correlation_Matrix",
    "Cost_Benefit_Analysis",
    "Training_Time_Comparison",
    "Combined_Analysis"
  ),
  Description = c(
    "Class distribution + feature distributions by class (histograms & box plots)",
    "Feature importance for XGBoost, Random Forest, and Bayesian LR",
    "Heatmap showing all performance metrics across models",
    "ROC curves for all models with AUC values",
    "Precision-recall curves for all models with AUPRC values",
    "Side-by-side comparison of ROC and PR curves",
    "Performance comparison with confidence intervals",
    "Correlation matrix of all features",
    "Cost analysis with optimal thresholds",
    "Computational efficiency comparison",
    "Combined ROC and performance plots"
  ),
  Addresses_Reviewer_Request = c(
    "Reviewer 5: Data distribution plots (histograms, box plots)",
    "Reviewer 5: Feature importance plots for tree-based models",
    "Reviewer 5: Model comparison heatmap",
    "Standard ML evaluation",
    "Reviewer 5: Precision-Recall curves for class-imbalanced problem",
    "Reviewer 5: PR curves + ROC comparison", 
    "Reviewer 5: Model comparison bar chart with CI",
    "Feature relevance analysis",
    "Clinical decision support",
    "Computational analysis",
    "Summary visualization"
  ),
  Files_Created = rep("PNG + PDF", 11)
)

save_with_timestamp(visualization_summary, "visualization_summary", output_dirs$tables)

# Save session info for reproducibility
session_df <- data.frame(
  Item = c("R_Version", "Platform", "Date", "Total_Runtime_Hours"),
  Value = c(R.version.string, R.version$platform, 
            as.character(Sys.Date()), round(as.numeric(analysis_log$total_runtime), 2))
)

save_with_timestamp(session_df, "session_info", output_dirs$reports)

# ============================================================================
# 11. FINAL COMPREHENSIVE SUMMARY
# ============================================================================

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("=== COMPLETE ENHANCED ANALYSIS FINISHED SUCCESSFULLY ===\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

best_auc_model <- cv_performance$Model[which.max(cv_performance$AUC)]
best_auprc_model <- pr_performance_table$Model_Clean[which.max(pr_performance_table$AUPRC)]
best_sensitivity_model <- cv_performance$Model[which.max(cv_performance$Sensitivity)]
lowest_cost_model <- cv_performance$Model[which.min(cv_performance$Cost)]

cat("\nBEST PERFORMING MODELS:\n")
cat("- Highest AUC:", best_auc_model, "(AUC =", 
    round(max(cv_performance$AUC), 3), ")\n")
cat("- Highest AUPRC:", best_auprc_model, "(AUPRC =", 
    round(max(pr_performance_table$AUPRC), 3), ")\n")
cat("- Highest Sensitivity:", best_sensitivity_model, "(Sensitivity =", 
    round(max(cv_performance$Sensitivity), 3), ")\n")
cat("- Lowest Cost:", lowest_cost_model, "(Cost =", 
    min(cv_performance$Cost), ")\n")

cat("\nSTATISTICAL SIGNIFICANCE:\n")
cat("- Friedman test p-value:", sprintf("%.4f", friedman_result$p.value), "\n")
if(nrow(mcnemar_results) > 0) {
  significant_pairs <- sum(mcnemar_results$significant_bonferroni)
  cat("- Significant pairwise differences (Bonferroni):", significant_pairs, "out of", 
      nrow(mcnemar_results), "\n")
}

cat("\nALL REVIEWER REQUIREMENTS ADDRESSED:\n")
cat("✓ Enhanced Bayesian LR with clinical domain knowledge\n")
cat("✓ Feature importance plots (XGBoost, Random Forest, Bayesian LR)\n")
cat("✓ Model comparison heatmap with all metrics\n")
cat("✓ Data distribution plots (histograms, box plots, class imbalance)\n")
cat("✓ Precision-Recall curves for class-imbalanced problem\n")
cat("✓ ROC vs PR comparison showing class imbalance impact\n")
cat("✓ Performance comparison with confidence intervals\n")
cat("✓ Feature correlation matrix\n")
cat("✓ Cost-benefit analysis for clinical decisions\n")
cat("✓ Bootstrap confidence intervals\n")
cat("✓ McNemar's tests for pairwise comparisons\n")
cat("✓ Friedman test for overall ranking\n")
cat("✓ Effect size calculations\n")
cat("✓ Clinical significance assessment\n")

cat("\nTOTAL ANALYSIS TIME:", round(as.numeric(analysis_log$total_runtime), 2), "hours\n")

cat("\nFILES CREATED:\n")
all_files <- list.files(output_base_dir, recursive = TRUE, full.names = FALSE)
for(dir_name in names(output_dirs)) {
  if(dir_name != "main") {
    dir_files <- list.files(output_dirs[[dir_name]], full.names = FALSE)
    if(length(dir_files) > 0) {
      cat("\n", toupper(dir_name), "(", length(dir_files), "files ):\n")
      for(file in head(dir_files, 5)) {  # Show first 5 files per directory
        cat("  ✓", file, "\n")
      }
      if(length(dir_files) > 5) {
        cat("  ... and", length(dir_files) - 5, "more files\n")
      }
    }
  }
}

cat("\n📁 Main results directory:", output_base_dir, "\n")
cat("📊 Excel summary:", file.path(output_dirs$reports, excel_filename), "\n")

cat("\n🎉 COMPREHENSIVE ANALYSIS COMPLETE! 🎉\n")
cat("All results saved with timestamps for reproducibility.\n")
cat("Ready for Scientific Reports submission with ALL reviewer requirements addressed.\n")

# Print final file count
total_files <- length(list.files(output_base_dir, recursive = TRUE))
cat("\nTotal files generated:", total_files, "\n")

cat("\n", paste(rep("=", 50), collapse = ""), "\n")
cat("REVIEWER REQUIREMENTS SUMMARY\n")
cat(paste(rep("=", 50), collapse = ""), "\n")
cat("Reviewer 5 Visualizations: ✓ ALL COMPLETED\n")
cat("- Feature importance plots: ✓\n")
cat("- Model comparison heatmap: ✓\n") 
cat("- Data distribution plots: ✓\n")
cat("- Precision-Recall curves: ✓\n")
cat("\nEnhanced Bayesian LR: ✓ IMPLEMENTED\n")
cat("- Clinical domain interactions: ✓\n")
cat("- Hierarchical modeling: ✓\n")
cat("- Informed priors: ✓\n")
cat("- Uncertainty quantification: ✓\n")
cat("\nStatistical Rigor: ✓ COMPLETE\n")
cat("- Bootstrap confidence intervals: ✓\n")
cat("- McNemar's tests: ✓\n")
cat("- Friedman test: ✓\n")
cat("- Effect sizes: ✓\n")
cat("- Multiple testing corrections: ✓\n")

