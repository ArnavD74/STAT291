
library(tidyverse)
library(moderndive)
library(skimr)

noaa_data <- read.csv("NOAAGISSWeatherDisasters.csv")
source("models.pck")
# source("regday2.pck")

skim(noaa_data)

# Drought.Count
# bootmatsmooth(x = noaa_data$Year, y = noaa_data$Drought.Count, lambda=0.95)
bootmatlin(x = noaa_data$Year, y = noaa_data$Drought.Count, lambda=0.95)
bootmatsmooth(x = noaa_data$delta.temp, y = noaa_data$Drought.Count, lambda=0.9)
# bootmatlin(x = noaa_data$delta.temp, y = noaa_data$Drought.Count, lambda=0.9)


# Flooding.Count
# bootmatsmooth(x = noaa_data$Year, y = noaa_data$Flooding.Count)
bootmatlin(x = noaa_data$Year, y = noaa_data$Flooding.Count, lambda=0.7)
# bootmatsmooth(x = noaa_data$delta.temp, y = noaa_data$Flooding.Count, lambda=0.5)
# bootmatlin(x = noaa_data$delta.temp, y = noaa_data$Flooding.Count, lambda=0.5)


# Freeze.Count
# bootmatsmooth(x = noaa_data$Year, y = noaa_data$Freeze.Count, lambda=0.9)
bootmatlin(x = noaa_data$Year, y = noaa_data$Freeze.Count, lambda=0.4)
bootmatsmooth(x = noaa_data$delta.temp, y = noaa_data$Freeze.Count)
bootmatlin(x = noaa_data$delta.temp, y = noaa_data$Freeze.Count)


# Severe.Storm.Count
bootmatsmooth(x = noaa_data$Year, y = noaa_data$Severe.Storm.Count, lambda=0.8)
# bootmatlin(x = noaa_data$Year, y = noaa_data$Severe.Storm.Count)
bootmatsmooth(x = noaa_data$delta.temp, y = noaa_data$Severe.Storm.Count, lambda=0.8)
# bootmatlin(x = noaa_data$delta.temp, y = noaa_data$Severe.Storm.Count, lambda=0.8)


# Tropical.Cyclone.Count
# bootmatsmooth(x = noaa_data$Year, y = noaa_data$Tropical.Cyclone.Count)
bootmatlin(x = noaa_data$Year, y = noaa_data$Tropical.Cyclone.Count)
bootmatsmooth(x = noaa_data$delta.temp, y = noaa_data$Tropical.Cyclone.Count)
# bootmatlin(x = noaa_data$delta.temp, y = noaa_data$Tropical.Cyclone.Count)


# Wildfire.Count
# bootmatsmooth(x = noaa_data$Year, y = noaa_data$Wildfire.Count)
bootmatlin(x = noaa_data$Year, y = noaa_data$Wildfire.Count)
# bootmatsmooth(x = noaa_data$delta.temp, y = noaa_data$Wildfire.Count)
bootmatlin(x = noaa_data$delta.temp, y = noaa_data$Wildfire.Count)


# Winter.Storm.Count
# bootmatsmooth(x = noaa_data$Year, y = noaa_data$Winter.Storm.Count, lambda=0.9)
bootmatlin(x = noaa_data$Year, y = noaa_data$Winter.Storm.Count, lambda=0.7)
# bootmatsmooth(x = noaa_data$delta.temp, y = noaa_data$Winter.Storm.Count, lambda=0.7)
bootmatlin(x = noaa_data$delta.temp, y = noaa_data$Winter.Storm.Count, lambda=0.7)


# All.Disasters.Count
bootmatsmooth(x = noaa_data$Year, y = noaa_data$All.Disasters.Count)
# bootmatlin(x = noaa_data$Year, y = noaa_data$All.Disasters.Count)
bootmatsmooth(x = noaa_data$delta.temp, y = noaa_data$All.Disasters.Count)
bootmatlin(x = noaa_data$delta.temp, y = noaa_data$All.Disasters.Count)



