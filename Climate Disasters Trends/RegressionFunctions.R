library(dplyr)
library(ggplot2)
library(broom)
library(stats)
library(tidyverse)
library(moderndive)
library(skimr)
library(boot)
weather_data <- read.csv("NOAAGISSWeatherDisasters.csv")

wildfire_model <- glm(Wildfire.Count ~ delta.temp + Year + delta.temp:Year + I(delta.temp^2) + I(Year^2),
                      data = weather_data, family = binomial(link = "logit"))

drought_model <- glm(Drought.Count ~ delta.temp + Year + delta.temp:Year + I(delta.temp^2) + I(Year^2),
                     data = weather_data, family = binomial(link = "logit"))

all_disasters_model <- lm(All.Disasters.Count ~ delta.temp + Year + delta.temp:Year + I(delta.temp^2) + I(Year^2),
                          data = weather_data)

severe_storm_model <- lm(Severe.Storm.Count ~ delta.temp + Year + delta.temp:Year + I(delta.temp^2) + I(Year^2),
                          data = weather_data)

summary(wildfire_model)
summary(drought_model)
summary(all_disasters_model)
summary(severe_storm_model)

AIC(wildfire_model)
AIC(drought_model)
AIC(all_disasters_model)
AIC(severe_storm_model)

# Testing alternative models
# wildfire_model_v2 <- lm(Wildfire.Count ~ delta.temp + Year + delta.temp:Year + I(delta.temp^2) + I(Year^2),
#                         data = weather_data)
# 
# drought_model_v2 <- lm(Drought.Count ~ delta.temp + Year + delta.temp:Year + I(delta.temp^2) + I(Year^2),
#                        data = weather_data)
# 
# all_disasters_model_v2 <- glm(All.Disasters.Count ~ delta.temp + Year + delta.temp:Year + I(delta.temp^2) + I(Year^2),
#                               data = weather_data, family = binomial(link = "logit"))
# 
# severe_storm_model_v2 <- glm(Severe.Storm.Count ~ delta.temp + Year + delta.temp:Year + I(delta.temp^2) + I(Year^2),
#                              data = weather_data, family = binomial(link = "logit"))
# 
# AIC(wildfire_model_v2)
# AIC(drought_model_v2)
# 
# All_Disasters_Binary <- ifelse(weather_data$All.Disasters.Count > 0, 1, 0)
# all_disasters_model_v2 <- glm(All_Disasters_Binary ~ delta.temp + Year + delta.temp:Year + I(delta.temp^2) + I(Year^2),
#                               data = weather_data, family = binomial(link = "logit"))
# 
# Severe_Storm_Binary <- ifelse(weather_data$Severe.Storm.Count > 0, 1, 0)
# severe_storm_model_v2 <- glm(Severe_Storm_Binary ~ delta.temp + Year + delta.temp:Year + I(delta.temp^2) + I(Year^2),
#                              data = weather_data, family = binomial(link = "logit"))
# 
# AIC(all_disasters_model_v2)
# AIC(severe_storm_model_v2)

# Nested Model Comparisons
wildfire_model_reduced <- glm(Wildfire.Count ~ delta.temp + Year, data = weather_data, family = binomial(link = "logit"))
dev_diff_wildfire <- deviance(wildfire_model_reduced) - deviance(wildfire_model)
p_value_wildfire <- pchisq(dev_diff_wildfire, df = 3, lower.tail = FALSE)

drought_model_reduced <- glm(Drought.Count ~ delta.temp + Year, data = weather_data, family = binomial(link = "logit"))
dev_diff_drought <- deviance(drought_model_reduced) - deviance(drought_model)
p_value_drought <- pchisq(dev_diff_drought, df = 3, lower.tail = FALSE)

all_disasters_model_reduced <- lm(All.Disasters.Count ~ delta.temp + Year, data = weather_data)
dev_diff_all_disasters <- deviance(all_disasters_model_reduced) - deviance(all_disasters_model)
p_value_all_disasters <- pchisq(dev_diff_all_disasters, df = 3, lower.tail = FALSE)

severe_storm_model_reduced <- lm(Severe.Storm.Count ~ delta.temp + Year, data = weather_data)
dev_diff_severe_storm <- deviance(severe_storm_model_reduced) - deviance(severe_storm_model)
p_value_severe_storm <- pchisq(dev_diff_severe_storm, df = 3, lower.tail = FALSE)

# Print results with if statements
if (p_value_wildfire < 0.01) {
  cat("higher-order terms significantly improves the model fit\n")
} else {
  cat("higher-order terms does not significantly improve the model fit\n")
}

if (p_value_drought < 0.01) {
  cat("higher-order terms significantly improves the model fit\n")
} else {
  cat("higher-order terms does not significantly improve the model fit\n")
}

if (p_value_all_disasters < 0.01) {
  cat("higher-order terms significantly improves the model fit\n")
} else {
  cat("higher-order terms does not significantly improve the model fit\n")
}

if (p_value_severe_storm < 0.01) {
  cat("higher-order terms significantly improves the model fit\n")
} else {
  cat("higher-order terms does not significantly improve the model fit\n")
}

# lmboot and logitboot
wildfire_model <- glm(Wildfire.Count ~ delta.temp + Year + delta.temp:Year + I(delta.temp^2) + I(Year^2),
                      data = weather_data, family = binomial(link = "logit"))
drought_model <- glm(Drought.Count ~ delta.temp + Year + delta.temp:Year + I(delta.temp^2) + I(Year^2),
                     data = weather_data, family = binomial(link = "logit"))
all_disasters_model <- lm(All.Disasters.Count ~ delta.temp + Year + delta.temp:Year + I(delta.temp^2) + I(Year^2),
                          data = weather_data)
severe_storm_model <- lm(Severe.Storm.Count ~ delta.temp + Year + delta.temp:Year + I(delta.temp^2) + I(Year^2),
                         data = weather_data)


wildfire_boot <- logitboot(wildfire_model, DF = weather_data, nboot = 10000, alpha = 0.05)
drought_boot <- logitboot(drought_model, DF = weather_data, nboot = 10000, alpha = 0.05)
all_disasters_boot <- lmboot(all_disasters_model, DF = weather_data, nboot = 10000, alpha = 0.05)
severe_storm_boot <- lmboot(severe_storm_model, DF = weather_data, nboot = 10000, alpha = 0.05)

print(wildfire_boot$coef)
print(drought_boot$coef)
print(all_disasters_boot$coef)
print(severe_storm_boot$coef)

wildfire_boot_ci <- wildfire_boot$pointwiseCI
drought_boot_ci <- drought_boot$pointwiseCI
all_disasters_boot_ci <- all_disasters_boot$coef.point
severe_storm_boot_ci <- severe_storm_boot$coef.point

print(wildfire_boot_ci)
print(drought_boot_ci)
print(all_disasters_boot_ci)
print(severe_storm_boot_ci)
