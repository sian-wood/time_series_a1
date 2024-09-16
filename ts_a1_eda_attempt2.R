# Sian Wood
# 20/08/2024
# Time Series Assignment 1
# Exploratory Data Analysis

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
tsib_no_covid = readRDS("tsib_no_covid.RDS")

ugo  = tsib_no_covid[,c(1,24)]

tsib_daily = readRDS("tsib_daily.rds")
pdd  = tsib_daily[,c(1,3)]
################################################################################
# Time Plots of Variables of Interest
################################################################################

# Peak Daily Demand
autoplot(pdd, Peak.Daily.Demand) +
  labs(y = "Peak Daily Demand",
       title = "", x = "Day") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=28))
#' Major seasonality, with the same patterns as for Total Daily Demand

# Unplanned Generation Outages
autoplot(ugo, Unplanned.Generation.Outages) +
  labs(y = "Unplanned Generation Outages",
       title = "", x = "Day") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=28))
#' Trend increasing, with no clear seasonality. Outlying peak during initial
#' lockdown

#' Peak Daily Demand is not stationary. Should be differenced.
#'

################################################################################
# Investigate Complex Seasonality
################################################################################

avoid_missing <- pdd |>
  dplyr::mutate(t = row_number()) |>
  update_tsibble(index = t, regular = TRUE)

avoid_missing %>%
  model(STL(sqrt(Peak.Daily.Demand) ~ season(period = 7) + season(period = 365))) %>%
  components() %>% autoplot()

avoid_missing_no_cov <- ugo |>
  dplyr::mutate(t = row_number()) |>
  update_tsibble(index = t, regular = TRUE)

avoid_missing_no_cov %>%
  model(STL(sqrt(Unplanned.Generation.Outages) ~ season(period = 24)+ season(period = 168)+ season(period = 6570)+ season(period = 8760))) %>%
  components() %>% autoplot()

################################################################################
# Transform
################################################################################
# Variances differ
# Peak Daily Demand
autoplot(pdd, log(Peak.Daily.Demand)) +
  labs(y = "Peak Daily Demand",
       title = "", x = "Day") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=28))
#' Major seasonality, with the same patterns as for Total Daily Demand

# Unplanned Generation Outages
autoplot(ugo, log(Unplanned.Generation.Outages)) +
  labs(y = "Unplanned Generation Outages",
       title = "", x = "Day") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=28))

################################################################################
# Difference logged data
################################################################################
pdd = bind_cols(pdd, log = log(pdd$Peak.Daily.Demand))
acf(pdd$log, lag.max = 100000)
pacf(pdd$log, lag.max = 100)

pdd_lag1 = diff(pdd$log, differences = 1, lag = 365)
# plot(pdd_lag1, type="l", main="First Difference")
acf(pdd_lag1, lag.max = 1000)
pacf(pdd_lag1, lag.max = 1000)

pdd_lag2 = diff(pdd$log, differences = 2, lag = 365)
# plot(pdd_lag2, type="l", main="Second Difference")
acf(pdd_lag2, lag.max = 1000)
pacf(pdd_lag2, lag.max = 1000)

pdd_lag3 = diff(pdd$log, differences = 3, lag = 365)
# plot(pdd_lag3, type="l", main="Second Difference")
acf(pdd_lag3, lag.max = 1000)
pacf(pdd_lag3, lag.max = 1000)

# logging pdd makes it worse

# UGO ##########################################################################

ugo = bind_cols(ugo, log = log(ugo$Unplanned.Generation.Outages))
acf(ugo$log, lag.max = 1000)
pacf(ugo$log, lag.max = 1000)

ugo_lag1 = diff(ugo$log, differences = 1, lag = 24*365)
# plot(ugo_lag1, type="l", main="First Difference")
acf(ugo_lag1, lag.max = 1000)
pacf(ugo_lag1, lag.max = 1000)

# dif 1 lag 24 - ARMA(1,1)

# Second is necessary
ugo_lag2 = diff(ugo$log, differences = 2, lag = 24*365)
# plot(ugo_lag2, type="l", main="Second Difference")
acf(ugo_lag2, lag.max = 1000)
pacf(ugo_lag2, lag.max = 1000)

# Third not needed
ugo_lag3 = diff(ugo$log, differences = 3, lag = 24*365)
# plot(ugo_lag3, type="l", main="Second Difference")
acf(ugo_lag3, lag.max = 1000)
pacf(ugo_lag3, lag.max = 1000)

ugo_lag_all = ugo$Unplanned.Generation.Outages |>
  diff(differences = 1, lag = 1) |>
  diff(differences = 1, lag = 24)|> 
  diff(differences = 1, lag = 24*7)


acf(ugo_lag_all, lag.max = 15000)
pacf(ugo_lag_all, lag.max = 15000)

################################################################################
# Investigate Complex Seasonality
################################################################################

################################################################################
# Investigate Complex Seasonality
################################################################################