 Malaria Detection Using Clinically Validated Synthetic Data

[![R Version](https://img.shields.io/badge/R-4.3.0+-blue.svg)](https://www.r-project.org/)
[![Status](https://img.shields.io/badge/Status-Under%20Review-yellow.svg)](https://github.com)
[![License](https://img.shields.io/badge/License-Academic%20Use-green.svg)](LICENSE)

## 📋 Project Overview

This repository contains the complete computational pipeline for our machine learning analysis of malaria detection using clinically validated synthetic data, supporting our submission to **Scientific Reports**.

### 🎯 Key Achievements
- **87% clinical representativeness** validated against WHO epidemiological data
- **Comprehensive statistical analysis** with bootstrap confidence intervals and significance testing  
- **Five ML algorithms compared** with rigorous cross-validation
- **XGBoost optimal performance**: AUC 0.956 (95% CI: 0.943-0.969)
- **Class imbalance addressed** with precision-recall analysis (19.5% prevalence)

### 📊 Main Results Summary
| Model | Accuracy | Sensitivity | Specificity | AUC |
|-------|----------|-------------|-------------|-----|
| XGBoost | 0.899 (0.886-0.911) | 0.897 (0.880-0.914) | 0.899 (0.886-0.912) | 0.956 (0.951-0.961) |
| Naive Bayes | 0.901 (0.888-0.914) | 0.895 (0.878-0.912) | 0.903 (0.890-0.916) | 0.952 (0.947-0.957) |
| Enhanced Bayesian LR | 0.900 (0.887-0.913) | 0.896 (0.879-0.913) | 0.901 (0.888-0.914) | 0.954 (0.949-0.959) |

## 🔬 Addressing Reviewer Requirements

✅ **Statistical Rigor**: Bootstrap confidence intervals, McNemar's tests, Friedman test  
✅ **Synthetic Data Validation**: 87% clinical representativeness vs. published epidemiological data  
✅ **Feature Importance**: Comprehensive analysis across all models  
✅ **Class Imbalance**: Precision-recall curves and cost-sensitive optimization  
✅ **Reproducibility**: Complete code with exact package versions  
✅ **Clinical Foundation**: Domain knowledge integrated into Bayesian models  

## 🚀 Quick Start

1. **System Requirements**: R 4.3.0+, 8GB RAM recommended
2. **Install Dependencies**: `source("requirements.txt")`
3. **Run Complete Analysis**: `source("analysis/enhanced_malaria_ml_v2.r")`
4. **Generate Visualizations**: `source("visualizations/enhanced_visualizations.r")`

See [`REPRODUCIBILITY.md`](REPRODUCIBILITY.md) for detailed instructions.

## 🔬 Validation Methodology

**Statistical Approach**: 5-fold cross-validation (not simple train/test split)

- **Each iteration**: 80% training, 20% testing data
- **Total validation**: Every sample tested exactly once across all folds
- **Final metrics**: Averaged performance across all 5 folds with bootstrap confidence intervals
- **Statistical rigor**: McNemar's tests, Friedman test, and effect size calculations
- **Advantages**: Maximizes data utilization, reduces overfitting, provides robust performance estimates

This approach provides more statistically sound validation than traditional single-split methods and directly addresses reviewer requirements for rigorous model evaluation.

## 📁 Repository Structure
├── data_generation/          # Synthetic data generation with clinical validation
├── analysis/                 # Main ML analysis scripts
├── visualizations/           # Publication-quality plots and functions
└── Malaria_ML_Results_Complete/  # Generated automatically when scripts run
├── Tables/               # CSV results and statistical summaries
├── Plots/                # All figures (PNG + PDF formats)
├── Models/               # Saved model objects (.rds files)
├── Statistical_Tests/    # McNemar, Friedman test results
├── Reports/              # Excel workbooks and summaries
└── Raw_Results/          # Complete analysis logs

**Note**: Only the first three directories need to be created manually. The `Malaria_ML_Results_Complete/` directory and all subdirectories are created automatically when you run the analysis scripts.

## 📞 For Reviewers

This repository provides **complete reproducibility** for all analyses reported in our manuscript. All scripts are documented and validated. 

**Estimated runtime**: 15-20 minutes for complete analysis on standard hardware.

## 👥 Contact

**Corresponding Author**: Mr. Chekol Alemu  
**Institution**: Gambella University, Gambella, Ethiopia  
**Email**: chekol2011@gmail.com

*Private repository - reviewer access available upon request during peer review process.*