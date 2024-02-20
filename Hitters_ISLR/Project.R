library(ggplot2)
library(dplyr)
library(ISLR)
library(lars)
library(crayon)
library(reshape2)

data("Hitters")
str(Hitters)
Hitters_clean <- Hitters %>%
  filter(!is.na(Salary))
str (Hitters_clean)

AE_data <- subset(Hitters_clean, FullDivision == "AE")
AW_data <- subset(Hitters_clean, FullDivision == "AW")
NE_data <- subset(Hitters_clean, FullDivision == "NE")
NW_data <- subset(Hitters_clean, FullDivision == "NW")
Hitters_clean$FullDivision <- paste0(Hitters_clean$League, Hitters_clean$Division)

# Salary Distributions - Boxplots
ggplot(Hitters_clean, aes(x = FullDivision, y = Salary)) + 
  geom_boxplot() +
  xlab("Division") +
  ylab("Salary") +
  ggtitle("Salary Distributions Across Divisions")

# Histograms for Each Division
ggplot(Hitters_clean, aes(x = Salary)) + 
  geom_histogram(binwidth = 100, fill = "blue", color = "black") +
  facet_wrap(~ FullDivision) +
  xlab("Salary") +
  ylab("Count") +
  ggtitle("Histogram of Salaries by Division")

aggregate(Salary ~ FullDivision, data = Hitters_clean, function(x) c(Mean = mean(x), Median = median(x), Variance = var(x)))

######################################################
# STEP 1
# Look at all 4 divisions. For each division choose the minimal Cp lars model.

find_minimal_cp_model <- function(data) {
  
  # fitting the LARS model
  division_lars <- lars(x = as.matrix(data[, !names(data) %in% c("Salary", "League", "Division", "NewLeague", "FullDivision")]), 
                        y = data$Salary, type = "lasso", max.steps = 100)
  
  # extract Cp values and locate indices  of minimal Cps
  min_cp_index <- which.min(division_lars$Cp)
  
  return(list(index = min_cp_index, Cp_value = division_lars$Cp[min_cp_index]))
}


# applying the function to each division
minimal_cp_AE <- find_minimal_cp_model(AE_data)
minimal_cp_AW <- find_minimal_cp_model(AW_data)
minimal_cp_NE <- find_minimal_cp_model(NE_data)
minimal_cp_NW <- find_minimal_cp_model(NW_data)

# summarizing the findings for each divison
summary_AE <- paste("AE Division: Minimal Cp is at model index", minimal_cp_AE$index, "with Cp value of", minimal_cp_AE$Cp_value)
summary_AW <- paste("AW Division: Minimal Cp is at model index", minimal_cp_AW$index, "with Cp value of", minimal_cp_AW$Cp_value)
summary_NE <- paste("NE Division: Minimal Cp is at model index", minimal_cp_NE$index, "with Cp value of", minimal_cp_NE$Cp_value)
summary_NW <- paste("NW Division: Minimal Cp is at model index", minimal_cp_NW$index, "with Cp value of", minimal_cp_NW$Cp_value)

summaries <- list(AE = summary_AE, AW = summary_AW, NE = summary_NE, NW = summary_NW)
summaries

######################################################
#STEP 2
# Fit the LARS model on division data using minimal Cp values

subset_plots <- function(data, division_name, optimal_model_index) {
  
  # Fit LARS model
  division_lars <- lars(x = as.matrix(data[, !names(data) %in% c("Salary", "League", "Division", "NewLeague", "FullDivision")]), 
                        y = data$Salary, type = "lasso")
  
  # Predict salaries using the optimal LARS model
  optimal_predictions <- predict(division_lars, 
                                 newx = as.matrix(data[, !names(data) %in% c("Salary", "League", "Division", "NewLeague", "FullDivision")]), 
                                 s = optimal_model_index)$fit
  
  # Create scatterplots based on different data subsets
  for (i in 1:4) {
    quartiles <- quantile(data$Years, probs = c(0.25, 0.5, 0.75))
    if (i == 1) {
      subset_indices <- which(data$Years <= quartiles[1])
    } else if (i == 2) {
      subset_indices <- which(data$Years > quartiles[1] & data$Years <= quartiles[2])
    } else if (i == 3) {
      subset_indices <- which(data$Years > quartiles[2] & data$Years <= quartiles[3])
    } else {
      subset_indices <- which(data$Years > quartiles[3])
    }
    subset_data <- data[subset_indices, ]
    subset_predictions <- optimal_predictions[subset_indices]
    
    # plotting
    plot_title <- paste("Division", division_name, "- Scatterplot", i)
    filename <- paste("Division_", division_name, "_Scatterplot_", i, ".png", sep = "") #saving to disk
    png(filename)
    plot(subset_predictions, subset_data$Salary, 
         main = plot_title, 
         xlab = "Predicted Salary", 
         ylab = "Actual Salary")
    dev.off()
  }
}

# applying the function to each division
subset_plots(AE_data, "AE", minimal_cp_AE$index)
subset_plots(AW_data, "AW", minimal_cp_AW$index)
subset_plots(NE_data, "NE", minimal_cp_NE$index)
subset_plots(NW_data, "NW", minimal_cp_NW$index)

######################################################
#STEP 3
# Fit the lm model on division data

fit_and_compare_models <- function(data) {
  
  # Similar to the LARS model, extract the numerical data only
  numeric_predictors <- data[, sapply(data, is.numeric)]
  numeric_predictors <- numeric_predictors[, !names(numeric_predictors) %in% "Salary"]
  
  # Fit LARS model
  lars_fit <- lars(x = as.matrix(numeric_predictors), y = data$Salary, type = "lasso")
  
  # Find smallest Cp value
  cp_index <- which.min(lars_fit$Cp)
  
  # Extract the coefficients
  lars_coefs <- coef(lars_fit)[cp_index, ]
  selected_vars <- names(lars_coefs[lars_coefs != 0])
  
  # Fit the lm model
  lm_formula <- as.formula(paste("Salary ~", paste(selected_vars, collapse = " + ")))
  lm_fit <- lm(lm_formula, data = data)
  
  return(list(lars_model = lars_fit, lm_model = lm_fit, selected_vars = selected_vars))
}

# (AE used as an example)
models_AE <- fit_and_compare_models(AE_data)
cp_values <- models_AE$lars_model$Cp
min_cp_index <- which.min(cp_values)
lars_coefs <- coef(models_AE$lars_model)[min_cp_index, ]

print(lars_coefs)
print(lm_coefs)

# Plotting LARS and LM (AE used as an example)
cp_values <- models_AE$lars_model$Cp
min_cp_index <- which.min(cp_values)
predictor_vars <- names(coef(models_AE$lars_model)[min_cp_index, ])
predictor_vars <- predictor_vars[predictor_vars != "(Intercept)"]  # exclude intercept

lars_predictions_AE <- predict(models_AE$lars_model, newx = as.matrix(AE_data[, predictor_vars]), s = min_cp_index)$fit
lm_predictions_AE <- predict(models_AE$lm_model, newdata = AE_data)

plot(lars_predictions_AE, AE_data$Salary, main = "LARS Predictions vs Actual Salaries", xlab = "Predicted Salary (LARS)", ylab = "Actual Salary", col = "blue")
plot(lm_predictions_AE, AE_data$Salary, main = "LM Predictions vs Actual Salaries", xlab = "Predicted Salary (LM)", ylab = "Actual Salary", col = "red")

######################################################
#STEP 4
# Bonus: Box plots

# Combine actual and predicted salaries from both models
combined_data <- data.frame(
  Actual = AE_data$Salary,
  LARS_Predicted = lars_predictions_AE,
  LM_Predicted = lm_predictions_AE
)

reshape_data <- melt(combined_data, variable.name = "Type", value.name = "Salary") # reshape

# Boxplots
ggplot(reshape_data, aes(x = Type, y = Salary, fill = Type)) + 
  geom_boxplot() +
  ggtitle("Comparison of Actual and Predicted Salaries (AE Division)") +
  xlab("Salary Type") +
  ylab("Salary")
