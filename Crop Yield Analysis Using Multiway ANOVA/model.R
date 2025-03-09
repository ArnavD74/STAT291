library(readr)
library(car)

crop.data <- read_csv("crop.data.csv", 
                      col_types = cols(density = col_factor(), 
                                       block = col_factor(),
                                       fertilizer = col_factor(),
                                       yield = col_double()))

crop.aov <- aov(yield ~ density * fertilizer * block, data = crop.data)

summary(crop.aov)

TukeyHSD(crop.aov, which = "fertilizer", conf.level = 0.95)