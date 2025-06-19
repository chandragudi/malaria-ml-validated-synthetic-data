\# Reproducibility Guide

\## Complete Step-by-Step Analysis Reproduction



---



\## 🔧 System Requirements



\### R Environment

\- \*\*R Version\*\*: 4.3.0 or higher

\- \*\*RStudio\*\*: Recommended (2023.06.0 or higher)

\- \*\*Operating System\*\*: Windows 10+, macOS 10.15+, or Linux Ubuntu 18.04+

\- \*\*Memory\*\*: Minimum 8GB RAM recommended

\- \*\*Storage\*\*: 2GB free space for outputs



\### Required R Packages

Execute this installation script:

```r

\# Install required packages with exact versions

required\_packages <- c(

&nbsp; "caret",           # 6.0-94

&nbsp; "randomForest",    # 4.7-1.1  

&nbsp; "xgboost",         # 1.7.5.1

&nbsp; "e1071",           # 1.7-13

&nbsp; "pROC",            # 1.18.4

&nbsp; "ROCR",            # 1.0-11

&nbsp; "ggplot2",         # 3.4.4

&nbsp; "dplyr",           # 1.1.3

&nbsp; "corrplot",        # 0.92

&nbsp; "gridExtra",       # 2.3

&nbsp; "RColorBrewer",    # 1.1-3

&nbsp; "viridis",         # 0.6.4

&nbsp; "knitr",           # 1.44

&nbsp; "rmarkdown",       # 2.25

&nbsp; "boot",            # 1.3-28.1

&nbsp; "rstanarm",        # 2.21.4

&nbsp; "bayesplot",       # 1.10.0

&nbsp; "loo"              # 2.6.0

)



install.packages(required\_packages, dependencies = TRUE)



🚀 Complete Reproduction Steps

Step 1: Repository Setup

bashgit clone https://github.com/YOUR\_USERNAME/malaria-ml-validated-synthetic-data.git

cd malaria-ml-validated-synthetic-data

Step 2: R Environment Preparation

Open R/RStudio and set working directory:

rsetwd("path/to/malaria-ml-validated-synthetic-data")

source("requirements.txt")  # Install packages

Step 3: Generate Synthetic Data (Optional)

r# Generate fresh synthetic dataset

source("data\_generation/enhanced\_synthetic\_data.r")

\# Output: synthetic\_malaria\_data\_validated.csv (10,100 samples)

\# Runtime: ~3-5 minutes

Step 4: Main Analysis Execution

r# Run comprehensive ML analysis

source("analysis/enhanced\_malaria\_ml\_v2.r")

\# Runtime: ~10-15 minutes

Expected Console Output:

\[1] "Starting Enhanced Malaria ML Analysis..."

\[1] "Loading and preprocessing data..."

\[1] "Splitting data: 80% training, 20% testing..."

\[1] "Training models with 5-fold cross-validation..."

\[1] "Naive Bayes training completed"

\[1] "Logistic Regression training completed"  

\[1] "Random Forest training completed"

\[1] "XGBoost training completed"

\[1] "Enhanced Bayesian LR training completed"

\[1] "Generating bootstrap confidence intervals..."

\[1] "Performing statistical significance tests..."

\[1] "Analysis completed successfully!"

Step 5: Generate Visualizations

r# Create publication-quality plots

source("visualizations/enhanced\_visualizations.r")

\# Runtime: ~2-3 minutes

Step 6: Alternative Streamlined Workflow

r# Alternative: Run complete streamlined analysis

source("analysis/complete\_workflow\_script.r")

\# Runtime: ~8-12 minutes



📊 Expected Outputs

Generated Files Structure

results/

├── tables/

│   ├── model\_performance\_with\_CI.csv

│   ├── statistical\_significance\_tests.csv

│   └── feature\_importance\_summary.csv

├── figures/

│   ├── ROC\_curves\_with\_CI.pdf

│   ├── precision\_recall\_curves.pdf

│   ├── model\_comparison\_heatmap.pdf

│   ├── feature\_importance\_plots.pdf

│   └── data\_distributions.pdf

└── models/

&nbsp;   ├── naive\_bayes\_model.rds

&nbsp;   ├── logistic\_regression\_model.rds

&nbsp;   ├── random\_forest\_model.rds

&nbsp;   ├── xgboost\_model.rds

&nbsp;   └── bayesian\_lr\_model.rds

Key Results Validation

Verify these main results match manuscript:

Table 1: Model Performance (95% CI)



XGBoost AUC: 0.956 (0.943-0.969)

Naive Bayes AUC: 0.952 (0.939-0.965)

Enhanced Bayesian LR AUC: 0.954 (0.941-0.967)



Statistical Tests



McNemar's test p-values: All < 0.001

Friedman test for model ranking: p < 0.001





🛠️ Troubleshooting

Common Issues

Issue 1: Package Installation Failures

r# Solution: Use CRAN snapshot

options(repos = c(CRAN = "https://cran.rstudio.com/"))

install.packages("package\_name", dependencies = TRUE)

Issue 2: Memory Issues During Analysis

r# Solution: Increase memory limit (Windows)

memory.limit(size = 8000)  # 8GB



\# For all systems: Clear workspace regularly

rm(list = ls())

gc()

Issue 3: RStanArm Installation Issues

r# Solution: Install compilation tools first

\# Windows: Install Rtools

\# macOS: Install Xcode command line tools

\# Linux: Install build-essential

Performance Optimization



Parallel Processing: Scripts automatically detect cores and use parallel processing

Memory Management: Large objects are cleaned up automatically

Progress Tracking: All long-running operations show progress bars





📞 Support

If you encounter issues during reproduction:



Check R version: R.version.string

Check package versions: sessionInfo()

Clear workspace: rm(list = ls()); gc()

Restart R session: Ctrl+Shift+F10 (RStudio)



Contact: chekol2011@gmail.com for reproduction support during review process.

Estimated Total Runtime: 15-20 minutes on standard hardware (Intel i5, 8GB RAM)

