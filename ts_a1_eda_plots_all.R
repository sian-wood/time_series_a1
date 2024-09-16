# Sian Wood
# 20/08/2024
# Time Series Assignment 1
# Exploratory Data Analysis

# List of packages to install and load
packages = readRDS("packages_needed.RDS")

# Function to check if a package is installed
install_if_missing <- function(p) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}

# Install and load packages
lapply(packages, install_if_missing)

# Read in cleaned data
tsib = readRDS("tsib.RDS")

################################################################################
# Univariate Analysis
################################################################################
nrow(tsib)

# Date Time Hour Beginning
# hist(tsib$Date.Time.Hour.Beginning)

# RSA Contracted Forecast
summary(tsib$RSA.Contracted.Forecast)
sd(tsib$RSA.Contracted.Forecast)
hist(tsib$RSA.Contracted.Forecast) # Bimodal

# RSA Contracted Demand
summary(tsib$RSA.Contracted.Demand)
sd(tsib$RSA.Contracted.Demand, na.rm = TRUE)
hist(tsib$RSA.Contracted.Demand) # Bimodal

# International Exports
summary(tsib$International.Exports)
sd(tsib$International.Exports, na.rm = TRUE)
hist(tsib$International.Exports) # Slight skew to the right

# International Imports
summary(tsib$International.Imports)
sd(tsib$International.Imports, na.rm = TRUE)
hist(tsib$International.Imports) # Skewed to the left

# Thermal Generation
summary(tsib$Thermal.Generation)
sd(tsib$Thermal.Generation, na.rm = TRUE)
hist(tsib$Thermal.Generation) # Slight skew to the left

# Nuclear Generation
summary(tsib$Nuclear.Generation)
sd(tsib$Nuclear.Generation, na.rm = TRUE)
hist(tsib$Nuclear.Generation) # Bimodal

# Wind – Total contracted Wind generation.
summary(tsib$Wind)
sd(tsib$Wind, na.rm = TRUE)
hist(tsib$Wind) # Skewed to the right

# PV - Total contracted Photovoltaic generation.
summary(tsib$PV)
sd(tsib$PV, na.rm = TRUE)
hist(tsib$PV) # Very skewed to the right

# CSP – Total contracted Concentrated Solar Power generation.
summary(tsib$CSP)
sd(tsib$CSP, na.rm = TRUE)
hist(tsib$CSP) # Very skewed to the right

# Other RE
summary(tsib$Other.RE)
sd(tsib$Other.RE, na.rm = TRUE)
hist(tsib$Other.RE) # Bimodal

# Total RE
summary(tsib$Total.RE)
sd(tsib$Total.RE, na.rm = TRUE)
hist(tsib$Total.RE) # Skewed to the right

# Wind Installed Capacity
summary(tsib$Wind.Installed.Capacity)
sd(tsib$Wind.Installed.Capacity, na.rm = TRUE)
hist(tsib$Wind.Installed.Capacity) # Multimodal

# PV Installed Capacity
summary(tsib$PV.Installed.Capacity)
sd(tsib$PV.Installed.Capacity, na.rm = TRUE)
hist(tsib$PV.Installed.Capacity) # Multimodal

# CSP Installed Capacity
summary(tsib$CSP.Installed.Capacity)
sd(tsib$CSP.Installed.Capacity, na.rm = TRUE)
hist(tsib$CSP.Installed.Capacity) # Perfectly uniform
unique(tsib$CSP.Installed.Capacity)

# Other RE Installed Capacity
summary(tsib$Other.RE.Installed.Capacity)
sd(tsib$Other.RE.Installed.Capacity, na.rm = TRUE)
hist(tsib$Other.RE.Installed.Capacity) # Only 3 values recorded
unique(tsib$Other.RE.Installed.Capacity)

# Total RE Installed Capacity
summary(tsib$Total.RE.Installed.Capacity)
sd(tsib$Total.RE.Installed.Capacity, na.rm = TRUE)
hist(tsib$Total.RE.Installed.Capacity)
unique(tsib$Total.RE.Installed.Capacity) # Only 28 different values recorded

# Installed Eskom Capacity
summary(tsib$Installed.Eskom.Capacity)
sd(tsib$Installed.Eskom.Capacity, na.rm = TRUE)
hist(tsib$Installed.Eskom.Capacity)
unique(tsib$Installed.Eskom.Capacity) # Only 17 different values recorded

# Total PCLF
summary(tsib$Total.PCLF)
sd(tsib$Total.PCLF, na.rm = TRUE)
hist(tsib$Total.PCLF) # Skewed to the right

# Total UCLF

# UCLF: Unplanned Capability Loss Factor of Eskom plant. It is the ratio between
# the unavailable energy of the units that are out on unplanned outages over a
# period compared to the total net installed capacity of all units over the same
# period.

summary(tsib$Total.UCLF)
sd(tsib$Total.UCLF, na.rm = TRUE)
hist(tsib$Total.UCLF, breaks = 100)

# Total OCLF
summary(tsib$Total.OCLF)
sd(tsib$Total.OCLF, na.rm = TRUE)
hist(tsib$Total.OCLF) # Skewed to the right

# Total UCLF OCLF
summary(tsib$Total.UCLF.OCLF)
sd(tsib$Total.UCLF.OCLF, na.rm = TRUE)
hist(tsib$Total.UCLF.OCLF)

################################################################################
# Bivariate Analysis
################################################################################
tsib_no_scpic = tsib[,-c(which(names(tsib) == "CSP.Installed.Capacity"), 
                         which(names(tsib) == "Date.Time.Hour.Beginning" ))]
cormat = cor(tsib_no_scpic)
ggcorrplot(cormat)

################################################################################
# Exploratory Time Series Analysis
################################################################################

# RSA Contracted Forecast
autoplot(tsib, RSA.Contracted.Forecast) +
  labs(y = "RSA Contracted Forecast",
       title = "Time Series of RSA Contracted Forecast Data",
       x = "Date")
#' Seasonal, no overall trend. Interesting event beginning of 2020 (Covid)

# RSA Contracted Demand
autoplot(tsib, RSA.Contracted.Demand) +
  labs(y = "RSA Contracted Forecast",
       title = "Time Series of RSA Contracted Demand Data",
       x = "Date")
#' Seasonal, no overall trend. Interesting event beginning of 2020 (Covid)

# International Exports
autoplot(tsib, International.Exports) +
  labs(y = "International Exports",
       title = "Time Series of International Exports Data",
       x = "Date")
#' Seasonal, overall downtrend. Interesting event beginning of 2020 (Covid)

# International Imports
autoplot(tsib, International.Imports) +
  labs(y = "International Imports",
       title = "Time Series of International Imports Data",
       x = "Date")
#' Maybe slight uptrend?

# Thermal Generation
autoplot(tsib, Thermal.Generation) +
  labs(y = "Thermal Generation",
       title = "Time Series of Thermal Generation Data",
       x = "Date")
#' Seasonal, downtrend. Interesting event beginning of 2020 (Covid)

# Nuclear Generation
autoplot(tsib, Nuclear.Generation) +
  labs(y = "Nuclear Generation",
       title = "Time Series of Nuclear Generation Data",
       x = "Date")
#' Very little white noise

# Wind
autoplot(tsib, Wind) +
  labs(y = "Wind", title = "Time Series of Wind Data", x = "Date")
#' Lots of white noise, upwards trend.

# PV
autoplot(tsib, PV) +
  labs(y = "Total contracted Photovoltaic generation",
       title = "Time Series of PV Data", x = "Date")
#' Loads of white noise

# CSP
autoplot(tsib, CSP) +
  labs(y = "Total contracted Concentrated Solar Power generation",
       title = "Time Series of CSP Data", x = "Date")
#' Seasonal, no overall trend.

# Other RE
autoplot(tsib, Other.RE) +
  labs(y = "Generation from other smaller contracted renewables",
       title = "Time Series of Other RE Data", x = "Date")
#' Seasonal, upward trend.

# Total RE
autoplot(tsib, Total.RE) +
  labs(y = "The total residual demand that is summated over a period of time",
       title = "Time Series of Total RE Data", x = "Date")
#' Seasonal, upward trend.

# Wind Installed Capacity
autoplot(tsib, Wind.Installed.Capacity) +
  labs(y = "Wind Installed Capacity",
       title = "Time Series of Wind Installed Capacity Data", x = "Date")
#' Upward trend. No white noise?

# PV Installed Capacity
autoplot(tsib, PV.Installed.Capacity) +
  labs(y = "PV Installed Capacity",
       title = "Time Series of PV Installed Capacity Data", x = "Date")
#' Upward trend. No white noise?

# CSP Installed Capacity
autoplot(tsib, CSP.Installed.Capacity) +
  labs(y = "CSP Installed Capacity",
       title = "Time Series of CSP Installed Capacity Data", x = "Date")
#' Constant over time

# Other RE Installed Capacity
autoplot(tsib, Other.RE.Installed.Capacity) +
  labs(y = "Other RE Installed Capacity",
       title = "Time Series of Other RE Installed Capacity Data", x = "Date")
#' Upward trend. No white noise?

# Total RE Installed Capacity
autoplot(tsib, Total.RE.Installed.Capacity) +
  labs(y = "Total RE Installed Capacity",
       title = "Time Series of Total RE Installed Capacity Data", x = "Date")
#' Upward trend. No white noise?

# Installed Eskom Capacity
autoplot(tsib, Installed.Eskom.Capacity) +
  labs(y = "Installed Eskom Capacity",
       title = "Time Series of Installed Eskom Capacity Data", x = "Date")
#' Peaks around mid 2022

# Total PCLF
autoplot(tsib, Total.PCLF) +
  labs(y = "Planned Capability Loss Factor of Eskom plant",
       title = "Time Series of Total PCLF Data", x = "Date")
#' Seasonal, no overall trend. Interesting event beginning of 2020 (Covid?)

# Total UCLF
autoplot(tsib, Total.UCLF) +
  labs(y = "Total Unplanned Capability Loss Factor",
       title = "Time Series of Total UCLF Data", x = "Date")
#' Upward trend. Interesting event beginning of 2020 (Covid)

# Total OCLF
autoplot(tsib, Total.OCLF) +
  labs(y = "Other Capability Loss Factor of Eskom plant",
       title = "Time Series of Total OCLF Data", x = "Date")
#' No idea. White noise. No clear trends.

# Total UCLF OCLF
autoplot(tsib, Total.UCLF.OCLF) +
  labs(y = "Total UCLF and OCLF",
       title = "Time Series of Total UCLF OCLF Data", x = "Date")
#' Upward trend. Interesting event beginning of 2020 (Covid)



# Interesting GGplots
gg_season(tsibble::fill_gaps(tsib_daily), Peak.Daily.Demand)
gg_season(tsibble::fill_gaps(tsib_no_covid), Unplanned.Generation.Outages)
gg_subseries(tsibble::fill_gaps(tsib_daily), Peak.Daily.Demand)
gg_subseries(tsibble::fill_gaps(tsib_no_covid), Unplanned.Generation.Outages)



