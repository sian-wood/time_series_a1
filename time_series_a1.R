# Sian Wood
# 20/08/2024
# Time Series Assignment 1
# Exploratory Data Analysis

# Load Packages
library(fpp3)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggcorrplot)

# Read in Data
data = read.csv("ESK6816.csv")
head(data)

# Testing as.POSIXct
# datetime = "2019/04/01 12:00"
# as.POSIXct(datetime,format="%Y/%m/%d %H:%M",tz=Sys.timezone())

# Convert dates from char to POSIXct
data$Date.Time.Hour.Beginning = sapply(data$Date.Time.Hour.Beginning,
                                       FUN = as.POSIXct,
                                       format="%Y/%m/%d %H:%M",
                                       tz=Sys.timezone())

# Create tsibble (time series tibble)
tib = tibble(data)
tsib <- as_tsibble(tib, index = Date.Time.Hour.Beginning)

################################################################################
# Univariate Analysis
################################################################################
nrow(tsib)

# Date Time Hour Beginning
hist(tsib$Date.Time.Hour.Beginning)

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
# Clean Data
################################################################################

# 4800 NA's in all but Date and RSA Contracted Forecast
which(is.na(tsib$RSA.Contracted.Demand))
tsib[which(is.na(tsib$RSA.Contracted.Demand)),]
# All the same rows are NA's
# NA rmved:
tsib_na_rmv = na.omit(tsib)

################################################################################
# Bivariate Analysis
################################################################################
tsib_no_scpic = tsib_na_rmv[,-which(names(tsib_na_rmv) == "CSP.Installed.Capacity")]
cormat = cor(tsib_no_scpic)
ggcorrplot(cormat)

################################################################################
# Exploratory Time Series Analysis
################################################################################

# RSA Contracted Forecast
autoplot(tsib, RSA.Contracted.Forecast)+
  labs(y = " ", title = "Time Series of RSA Contracted Forecast Data")

# RSA Contracted Demand
autoplot(tsib, International.Imports)+
  labs(y = " ", title = " Time Series of RSA Contracted Demand Data")

# International Exports
autoplot(tsib, International.Imports)+
  labs(y = " ", title = " Time Series of International Exports Data")

# International Imports
autoplot(tsib, International.Imports)+
  labs(y = " ", title = " Time Series of International Imports Data")

# Thermal Generation
autoplot(tsib, International.Imports)+
  labs(y = " ", title = " Time Series of Thermal Generation Data")

# Nuclear Generation
autoplot(tsib, International.Imports)+
  labs(y = " ", title = " Time Series of Nuclear Generation Data")

# Wind
autoplot(tsib, International.Imports)+
  labs(y = " ", title = " Time Series of Wind Data")

# PV
autoplot(tsib, International.Imports)+
  labs(y = " ", title = " Time Series of PV Data")

# CSP
autoplot(tsib, International.Imports)+
  labs(y = " ", title = " Time Series of CSP Data")

# Other RE
autoplot(tsib, International.Imports)+
  labs(y = " ", title = " Time Series of Other RE Data")

# Total RE
autoplot(tsib, International.Imports)+
  labs(y = " ", title = " Time Series of Total RE Data")

# Wind Installed Capacity
autoplot(tsib, International.Imports)+
  labs(y = " ", title = " Time Series of Wind Installed Capacity Data")

# PV Installed Capacity
autoplot(tsib, International.Imports)+
  labs(y = " ", title = " Time Series of PV Installed Capacity Data")

# CSP Installed Capacity
autoplot(tsib, International.Imports)+
  labs(y = " ", title = " Time Series of CSP Installed Capacity Data")

# Other RE Installed Capacity
autoplot(tsib, International.Imports)+
  labs(y = " ", title = " Time Series of Other RE Installed Capacity Data")

# Total RE Installed Capacity
autoplot(tsib, International.Imports)+
  labs(y = " ", title = " Time Series of Total RE Installed Capacity Data")

# Installed Eskom Capacity
autoplot(tsib, International.Imports)+
  labs(y = " ", title = " Time Series of Installed Eskom Capacity Data")

# Total PCLF
autoplot(tsib, International.Imports)+
  labs(y = " ", title = " Time Series of Total PCLF Data")

# Total UCLF
autoplot(tsib, International.Imports)+
  labs(y = " ", title = " Time Series of Total UCLF Data")

# Total OCLF
autoplot(tsib, International.Imports)+
  labs(y = " ", title = " Time Series of Total OCLF Data")

# Total UCLF OCLF
autoplot(tsib, International.Imports)+
  labs(y = " ", title = " Time Series of Total UCLF OCLF Data")











