# Analyzing Trends in Climate Disasters Using NOAA Data

## Overview

This project explores climate-related disasters using data from NOAA and GISS, focusing on changes in **droughts, floods, storms, wildfires, and other extreme weather events** over time. By leveraging statistical smoothing techniques and regression analysis, we investigate potential correlations between **temperature changes and disaster frequency**.

## Research Objectives

-   Analyze trends in different climate disasters (droughts, floods, storms, wildfires, etc.).
-   Investigate correlations between **temperature changes** and extreme weather events.
-   Apply **bootstrapped regression models** to identify long-term patterns.

## Dataset

-   **Source:** NOAA & GISS Climate Data
-   **File:** `NOAAGISSWeatherDisasters.csv`
-   **Features Used:** Yearly counts of **droughts, floods, wildfires, storms**, and **temperature change (delta.temp).**

## Methodology

1. **Data Preprocessing**:

    - Loaded NOAA data and checked for missing values.
    - Standardized column names and prepared dataset for analysis.

2. **Statistical Analysis**:

    - Applied **bootstrap linear regression** (`bootmatlin`) and **smoothing techniques** (`bootmatsmooth`) to assess trends.
    - Examined **correlations** between temperature changes and disaster occurrences.

3. **Key Climate Events Modeled**:
    - **Droughts**
    - **Flooding**
    - **Freezes**
    - **Severe Storms**
    - **Tropical Cyclones**
    - **Wildfires**
    - **Winter Storms**
    - **Overall Disaster Counts**

## Key Findings

-   **Rising Disasters & Temperature Correlation:**
    -   Certain disasters, such as wildfires and severe storms, exhibit a strong correlation with rising temperatures.
-   **Droughts & Flooding Trends:**
    -   Variability in **drought** and **flood** frequency suggests regional climate dependencies.
-   **Storm & Wildfire Increase Over Time:**
    -   Regression models indicate an upward trend in **severe storms and wildfires**, aligning with observed climate shifts.

## Files in This Repository

-   `Model Analysis.pdf` – A detailed analysis comparing using different models to fit the data, uses AIC score for comparisons.
-   `Regression Functions.pdf` – A study using logistic and linear regression to model the relationship between temperature changes and the frequency of wildfires, droughts, severe storms, and overall climate disasters, selecting the best models based on statistical significance and AIC comparisons.
-   `Project.R` – R script for data analysis and regression modeling.
-   `project.Rproj` – R project file.
-   `NOAAGISSWeatherDisasters.csv` – NOAA dataset on weather-related disasters.
-   `models.pck`, `models2.pck` – Additional statistical functions used in analysis.
