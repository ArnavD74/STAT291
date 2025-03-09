# Predicting Baseball Player Salaries Using LARS and LM Models

## Overview

This project examines how player performance metrics influence salaries in professional baseball. By applying **Least Angle Regression (LARS)** and **Linear Regression (LM)**, we compare model accuracy, feature selection, and salary distribution across different league divisions.

## Research Objectives

-   Identify the **minimal Cp** LARS model for each division.
-   Compare **LARS and LM models** in predicting player salaries.
-   Visualize salary distributions across **American East (AE), American West (AW), National East (NE), and National West (NW) divisions**.
-   Evaluate feature selection and model performance.

## Dataset

-   **Source:** _Hitters dataset from ISLR package in R_
-   **Features Used:** Player performance metrics such as **hits, home runs, at-bats, and errors**.
-   **Processed Data:** Cleaned player salary information across four divisions.

## Methodology

1. **Data Preprocessing**:

    - Filtered out missing salary values.
    - Standardized categorical variables for division and league classification.

2. **Model Selection**:

    - Applied **LARS regression** to identify the best model using the **Cp statistic**.
    - Compared predictions of **LARS vs. LM** models.

3. **Visualizations**:
    - **Scatterplots:** Predicted vs. actual salaries.
    - **Boxplots:** Salary distributions across divisions.
    - **Histograms:** Salary frequency distribution for each division.

## Key Findings

-   **LARS vs. LM:**
    -   LARS **performs feature selection**, reducing unnecessary predictors and preventing overfitting.
    -   LM **includes all variables**, which may introduce collinearity and higher variance.
-   **Salary Trends Across Divisions:**
    -   Some divisions exhibit **tighter clustering**, indicating stronger salary-performance relationships.
    -   Other divisions show **more variance**, suggesting external salary factors beyond performance.

## Files in This Repository

-   `Project.R` – R script for data preprocessing, modeling, and visualization.
-   `project.Rproj` – R project file.
-   `Hitters data in ISLR.pdf` – Research report summarizing methodology and findings.
-   **Images**:
    -   `LARS.png` – LARS model predictions vs. actual salaries.
    -   `LM.png` – LM model predictions vs. actual salaries.
    -   `boxplot.png`, `boxplot2.png` – Boxplots comparing predicted and actual salaries.
    -   `histogram.png` – Salary distribution across divisions.
