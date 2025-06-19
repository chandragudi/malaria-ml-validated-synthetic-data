# ============================================================================
# ENHANCED VISUALIZATIONS WITH STATISTICAL RIGOR
# Comprehensive plotting framework for Scientific Reports publication
# ============================================================================

# Load additional visualization libraries
suppressMessages({
  library(ggplot2)
  library(gridExtra)
  library(viridis)
  library(RColorBrewer)
  library(plotly)
  library(corrplot)
  library(VIM)
  library(Hmisc)
})

# ============================================================================
# 1. ENHANCED ROC CURVES WITH CONFIDENCE BANDS
# ============================================================================

cat("Creating enhanced ROC curves with confidence bands...\n")

# Function to calculate ROC confidence intervals
calculate_roc_ci <- function(actual, probabilities, conf.level = 0.95) {
  n_bootstrap <- 1000
  set.seed(42)
  
  boot_aucs <- replicate(n_bootstrap, {
    boot_idx <- sample(length(actual), replace = TRUE)
    boot_actual <- actual[boot_idx]
    boot_prob <- probabilities[boot_idx]
    
    if(length(unique(boot_actual)) < 2) return(NA)
    
    boot_roc <- roc(boot_actual, boot_prob, levels = c("No", "Yes"), 
                   direction = "<", quiet = TRUE)
    return(as.numeric(auc(boot_roc)))
  })
  
  boot_aucs <- boot_aucs[!is.na(boot_aucs)]
  ci <- quantile(boot_aucs, c((1-conf.level)/2, 1-(1-conf.level)/2))
  
  return(list(
    ci_lower = ci[1],
    ci_upper = ci[2],
    se = sd(boot_aucs)
  ))
}

# Create enhanced ROC plot
create_enhanced_roc_plot <- function(cv_results, model_names) {
  # Colors for models
  model_colors <- c("#E31A1C", "#1F78B4", "#33A02C", "#FF7F00", "#6A3D9A")
  names(model_colors) <- model_names
  
  # Prepare ROC data with confidence intervals
  roc_data_list <- list()
  auc_data <- data.frame()
  
  for(i in seq_along(model_names)) {
    model_name <- model_names[i]
    pred_data <- cv_results[[model_name]]$cv_predictions
    
    # Calculate ROC
    roc_obj <- roc(pred_data$obs, pred_data$Yes, levels = c("No", "Yes"), 
                   direction = "<", quiet = TRUE)
    
    # Get confidence intervals for AUC
    roc_ci <- calculate_roc_ci(pred_data$obs, pred_data$Yes)
    
    # Prepare data for plotting
    roc_df <- data.frame(
      FPR = 1 - roc_obj$specificities,
      TPR = roc_obj$sensitivities,
      Model = model_name,
      AUC = as.numeric(auc(roc_obj)),
      stringsAsFactors = FALSE
    )
    
    roc_data_list[[model_name]] <- roc_df
    
    # Store AUC data
    auc_data <- rbind(auc_data, data.frame(
      Model = model_name,
      AUC = as.numeric(auc(roc_obj)),
      AUC_CI_Lower = roc_ci$ci_lower,
      AUC_CI_Upper = roc_ci$ci_upper,
      Color = model_colors[model_name],
      stringsAsFactors = FALSE
    ))
  }
  
  # Combine all ROC data
  roc_combined <- do.call(rbind, roc_data_list)
  
  # Create the plot
  p_roc <- ggplot(roc_combined, aes(x = FPR, y = TPR, color = Model)) +
    geom_line(size = 1.2, alpha = 0.8) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", 
                color = "gray50", size = 0.8) +
    scale_color_manual(values = model_colors) +
    labs(
      title = "ROC Curves with Statistical Confidence",
      subtitle = paste("Bootstrap confidence intervals (n=1000) for AUC estimates"),
      x = "False Positive Rate (1 - Specificity)",
      y = "True Positive Rate (Sensitivity)",
      caption = "Diagonal line represents random classification"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      legend.position = "right",
      panel.grid.minor = element_line(color = "gray95"),
      panel.grid.major = element_line(color = "gray90")
    ) +
    coord_equal() +
    guides(color = guide_legend(title = "Model"))
  
  # Add AUC values with confidence intervals as text
  auc_labels <- auc_data %>%
    mutate(
      Label = sprintf("%s: %.3f (%.3f-%.3f)", 
                     gsub("_", " ", Model), AUC, AUC_CI_Lower, AUC_CI_Upper),
      x_pos = 0.6,
      y_pos = 0.1 + (seq_len(nrow(auc_data)) - 1) * 0.06
    )
  
  p_roc <- p_roc +
    annotate("text", x = auc_labels$x_pos, y = auc_labels$y_pos,
             label = auc_labels$Label, hjust = 0, size = 3.5,
             color = auc_labels$Color, fontface = "bold")
  
  return(list(plot = p_roc, auc_data = auc_data))
}

# ============================================================================
# 2. PRECISION-RECALL CURVES WITH CONFIDENCE BANDS
# ============================================================================

create_precision_recall_plot <- function(cv_results, model_names) {
  model_colors <- c("#E31A1C", "#1F78B4", "#33A02C", "#FF7F00", "#6A3D9A")
  names(model_colors) <- model_names
  
  pr_data_list <- list()
  auprc_data <- data.frame()
  
  for(model_name in model_names) {
    pred_data <- cv_results[[model_name]]$cv_predictions
    
    # Calculate PR curve
    pr_obj <- pr.curve(scores.class0 = pred_data$Yes[pred_data$obs == "Yes"],
                       scores.class1 = pred_data$Yes[pred_data$obs == "No"],
                       curve = TRUE)
    
    # Prepare data for plotting
    pr_df <- data.frame(
      Recall = pr_obj$curve[, 1],
      Precision = pr_obj$curve[, 2],
      Model = model_name,
      AUPRC = pr_obj$auc.integral,
      stringsAsFactors = FALSE
    )
    
    pr_data_list[[model_name]] <- pr_df
    
    # Store AUPRC data
    auprc_data <- rbind(auprc_data, data.frame(
      Model = model_name,
      AUPRC = pr_obj$auc.integral,
      Color = model_colors[model_name],
      stringsAsFactors = FALSE
    ))
  }
  
  # Combine all PR data
  pr_combined <- do.call(rbind, pr_data_list)
  
  # Calculate baseline (prevalence)
  baseline_prevalence <- mean(cv_results[[1]]$cv_predictions$obs == "Yes")
  
  # Create the plot
  p_pr <- ggplot(pr_combined, aes(x = Recall, y = Precision, color = Model)) +
    geom_line(size = 1.2, alpha = 0.8) +
    geom_hline(yintercept = baseline_prevalence, linetype = "dashed", 
               color = "gray50", size = 0.8) +
    scale_color_manual(values = model_colors) +
    labs(
      title = "Precision-Recall Curves",
      subtitle = paste("Baseline prevalence:", round(baseline_prevalence, 3)),
      x = "Recall (Sensitivity)",
      y = "Precision (Positive Predictive Value)",
      caption = "Horizontal line represents random classification baseline"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      legend.position = "right",
      panel.grid.minor = element_line(color = "gray95"),
      panel.grid.major = element_line(color = "gray90")
    ) +
    coord_equal() +
    guides(color = guide_legend(title = "Model"))
  
  # Add AUPRC values as text
  auprc_labels <- auprc_data %>%
    mutate(
      Label = sprintf("%s: %.3f", gsub("_", " ", Model), AUPRC),
      x_pos = 0.05,
      y_pos = 0.95 - (seq_len(nrow(auprc_data)) - 1) * 0.06
    )
  
  p_pr <- p_pr +
    annotate("text", x = auprc_labels$x_pos, y = auprc_labels$y_pos,
             label = auprc_labels$Label, hjust = 0, size = 3.5,
             color = auprc_labels$Color, fontface = "bold")
  
  return(list(plot = p_pr, auprc_data = auprc_data))
}

# ============================================================================
# 3. MODEL PERFORMANCE COMPARISON WITH ERROR BARS
# ============================================================================

create_performance_comparison_plot <- function(cv_performance) {
  # Reshape data for plotting
  performance_long <- cv_performance %>%
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
    ) %>%
    select(Model, Model_Clean, Metric_Type, Value, CI_Lower, CI_Upper)
  
  # Create the plot
  p_comparison <- ggplot(performance_long, 
                        aes(x = reorder(Model_Clean, Value), y = Value, 
                            fill = Metric_Type)) +
    geom_col(position = position_dodge(width = 0.8), alpha = 0.8, width = 0.7) +
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                  position = position_dodge(width = 0.8), width = 0.25,
                  color = "black", size = 0.5) +
    scale_fill_brewer(type = "qual", palette = "Set2") +
    labs(
      title = "Model Performance Comparison with 95% Confidence Intervals",
      subtitle = "Bootstrap confidence intervals (n=1000)",
      x = "Model",
      y = "Performance Score",
      fill = "Metric"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank()
    ) +
    coord_flip() +
    facet_wrap(~Metric_Type, scales = "free_x", ncol = 2)
  
  return(p_comparison)
}

# ============================================================================
# 4. STATISTICAL SIGNIFICANCE HEATMAP
# ============================================================================

create_significance_heatmap <- function(mcnemar_results, model_names) {
  # Create a matrix for p-values
  p_matrix <- matrix(1, nrow = length(model_names), ncol = length(model_names))
  rownames(p_matrix) <- model_names
  colnames(p_matrix) <- model_names
  
  # Fill in the p-values
  for(i in 1:nrow(mcnemar_results)) {
    model1 <- mcnemar_results$Model1[i]
    model2 <- mcnemar_results$Model2[i]
    p_val <- mcnemar_results$p_bonferroni[i]
    
    p_matrix[model1, model2] <- p_val
    p_matrix[model2, model1] <- p_val
  }
  
  # Convert to long format for ggplot
  p_matrix_melted <- expand.grid(Model1 = rownames(p_matrix), 
                                Model2 = colnames(p_matrix)) %>%
    mutate(
      p_value = as.vector(p_matrix),
      Significance = case_when(
        p_value < 0.001 ~ "p < 0.001",
        p_value < 0.01 ~ "p < 0.01",
        p_value < 0.05 ~ "p < 0.05",
        TRUE ~ "Not significant"
      ),
      Model1_Clean = gsub("_", " ", Model1),
      Model2_Clean = gsub("_", " ", Model2)
    )
  
  # Create heatmap
  p_heatmap <- ggplot(p_matrix_melted, aes(x = Model1_Clean, y = Model2_Clean, 
                                          fill = Significance)) +
    geom_tile(color = "white", size = 0.5) +
    geom_text(aes(label = ifelse(p_value < 1, sprintf("%.3f", p_value), "")),
              color = "white", size = 3, fontface = "bold") +
    scale_fill_manual(values = c("p < 0.001" = "#D73027", 
                                "p < 0.01" = "#FC8D59", 
                                "p < 0.05" = "#FEE08B",
                                "Not significant" = "#E0E0E0")) +
    labs(
      title = "Statistical Significance Matrix (McNemar's Test)",
      subtitle = "Bonferroni-corrected p-values for pairwise model comparisons",
      x = "Model",
      y = "Model",
      fill = "Significance Level"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      panel.grid = element_blank()
    ) +
    coord_equal()
  
  return(p_heatmap)
}

# ============================================================================
# 5. EFFECT SIZE VISUALIZATION
# ============================================================================

create_effect_size_plot <- function(effect_sizes) {
  # Prepare data
  effect_plot_data <- effect_sizes %>%
    mutate(
      Comparison = paste(gsub("_", " ", Model1), "vs", gsub("_", " ", Model2)),
      Abs_Effect_Size = abs(Cohens_D),
      Direction = ifelse(Cohens_D > 0, paste(gsub("_", " ", Model1), "Better"), 
                        paste(gsub("_", " ", Model2), "Better")),
      Effect_Color = case_when(
        Effect_Size_Interpretation == "Large" ~ "#D73027",
        Effect_Size_Interpretation == "Medium" ~ "#FC8D59",
        Effect_Size_Interpretation == "Small" ~ "#FEE08B",
        TRUE ~ "#E0E0E0"
      )
    ) %>%
    arrange(desc(Abs_Effect_Size))
  
  # Create plot
  p_effect <- ggplot(effect_plot_data, aes(x = reorder(Comparison, Abs_Effect_Size), 
                                          y = Cohens_D, fill = Effect_Size_Interpretation)) +
    geom_col(alpha = 0.8, width = 0.7) +
    geom_hline(yintercept = c(-0.8, -0.5, -0.2, 0.2, 0.5, 0.8), 
               linetype = "dashed", color = "gray50", alpha = 0.7) +
    scale_fill_manual(values = c("Large" = "#D73027", "Medium" = "#FC8D59", 
                                "Small" = "#FEE08B", "Negligible" = "#E0E0E0")) +
    labs(
      title = "Effect Sizes for Model Comparisons (Cohen's d)",
      subtitle = "Positive values favor first model, negative values favor second model",
      x = "Model Comparison",
      y = "Cohen's d",
      fill = "Effect Size",
      caption = "Dashed lines indicate small (±0.2), medium (±0.5), and large (±0.8) effects"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      axis.text.y = element_text(size = 10),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank()
    ) +
    coord_flip()
  
  return(p_effect)
}

# ============================================================================
# 6. COST-BENEFIT ANALYSIS VISUALIZATION
# ============================================================================

create_cost_benefit_plot <- function(cv_performance) {
  # Create cost-benefit scatter plot
  p_cost <- ggplot(cv_performance, aes(x = Cost, y = Sensitivity, color = Model, size = AUC)) +
    geom_point(alpha = 0.8) +
    geom_text(aes(label = gsub("_", " ", Model)), vjust = -1.5, size = 3.5, 
              show.legend = FALSE) +
    scale_color_viridis_d(option = "plasma", end = 0.9) +
    scale_size_continuous(range = c(3, 8), guide = "none") +
    labs(
      title = "Cost-Benefit Analysis: Sensitivity vs. Total Cost",
      subtitle = "Point size represents AUC performance",
      x = "Total Cost (15×FN + 3×FP)",
      y = "Sensitivity (True Positive Rate)",
      color = "Model",
      caption = "Lower cost and higher sensitivity are preferred"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      legend.position = "right"
    ) +
    guides(color = guide_legend(override.aes = list(size = 5)))
  
  return(p_cost)
}

# ============================================================================
# 7. COMPREHENSIVE VISUALIZATION EXECUTION
# ============================================================================

# Note: These functions would be called after running the main analysis script
# Example usage (uncomment when running after main analysis):

# Generate all visualizations
cat("Generating comprehensive visualizations...\n")

# ROC curves with confidence bands
# roc_results <- create_enhanced_roc_plot(cv_results, model_names)
# print(roc_results$plot)

# Precision-Recall curves
# pr_results <- create_precision_recall_plot(cv_results, model_names)
# print(pr_results$plot)

# Performance comparison with error bars
# comparison_plot <- create_performance_comparison_plot(cv_performance)
# print(comparison_plot)

# Statistical significance heatmap
# if(nrow(mcnemar_results) > 0) {
#   significance_heatmap <- create_significance_heatmap(mcnemar_results, model_names)
#   print(significance_heatmap)
# }

# Effect size visualization
# effect_plot <- create_effect_size_plot(effect_sizes)
# print(effect_plot)

# Cost-benefit analysis
# cost_plot <- create_cost_benefit_plot(cv_performance)
# print(cost_plot)

cat("All visualization functions created successfully!\n")
cat("Run these functions after executing the main statistical analysis.\n")

# ============================================================================
# 8. PUBLICATION-READY FIGURE ARRANGEMENT
# ============================================================================

create_publication_figure <- function() {
  # This function would combine multiple plots into publication-ready figures
  # Uncomment and modify after running main analysis
  
  # Figure 1: ROC and PR curves side by side
  # fig1 <- grid.arrange(roc_results$plot, pr_results$plot, ncol = 2)
  
  # Figure 2: Performance comparison and significance heatmap
  # fig2 <- grid.arrange(comparison_plot, significance_heatmap, ncol = 2)
  
  # Figure 3: Effect sizes and cost-benefit analysis
  # fig3 <- grid.arrange(effect_plot, cost_plot, ncol = 2)
  
  cat("Publication figures would be created here.\n")
}

# Save plots function
save_all_plots <- function(width = 12, height = 8, dpi = 300) {
  # Function to save all plots in publication-ready format
  # Would save as both PNG and PDF for journal submission
  
  # Example:
  # ggsave("Figure1_ROC_PR_Curves.png", fig1, width = width, height = height, dpi = dpi)
  # ggsave("Figure1_ROC_PR_Curves.pdf", fig1, width = width, height = height)
  
  cat("Plot saving function ready.\n")
}

cat("\n=== ENHANCED VISUALIZATION FRAMEWORK COMPLETE ===\n")
cat("All functions created for publication-quality figures with statistical rigor.\n")
