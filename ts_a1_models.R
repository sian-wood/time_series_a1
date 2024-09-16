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
ugo = readRDS("ugo_train.RDS")
pdd = readRDS("pdd_train.RDS")

# residual plot:
res_plot = function(data, data_augment){
  plots = gg_tsresiduals(data)
  plots[[1]] = autoplot(data_augment,.innov, na.rm = TRUE) +
    labs(x = "Day", y = "Innovation Residuals")+
    theme(axis.text=element_text(size=15),
          axis.title=element_text(size=18))
  plots[[2]] = plots[[2]] + labs(x = "Lag", y = "ACF")+
    theme(axis.text=element_text(size=15),
          axis.title=element_text(size=18))
  plots[[3]] = plots[[3]] + labs(x = "Residuals", y = "Count")+
    theme(axis.text=element_text(size=15),
          axis.title=element_text(size=18))+
    scale_x_continuous(breaks = seq(-2000, 3000, by = 2000))
  plots
}



################################################################################
# UGO Benchmark
################################################################################
snaive_ugo <- ugo |>
  model(
    `Drift` = NAIVE(Unplanned.Generation.Outages~ drift()),
    `Seasonal naïve 6570` = SNAIVE(Unplanned.Generation.Outages~ lag(6570)),
    `Seasonal naïve 8760` = SNAIVE(Unplanned.Generation.Outages~ lag(8760))
  )

# Generate forecasts for 31 days
snaive_ugo_fc <- snaive_ugo |> forecast(h = 31*24*3)

# Plot forecasts against actual values
snaive_ugo_fc |>
  autoplot(ugo, level = NULL)  +
  theme(legend.position = "bottom")+
  labs(y = "UGO",
       x = "Date") +
  guides(colour = guide_legend(title = "Forecast")) +
  scale_color_manual(values=c("#3D2965", "#FF72AA", "#5F91ED")) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=28),
        legend.title=element_text(size=28),
        legend.text=element_text(size=15))+
  theme(plot.margin = unit(c(0, 1.6, 0, 0), "cm"))

###

snaive_ugo_6750 <- ugo |>
  model(
    `Seasonal naïve 6570` = SNAIVE(Unplanned.Generation.Outages~ lag(6570))
  )

snaive_ugo_8760 <- ugo |>
  model(
    `Seasonal naïve 8760` = SNAIVE(Unplanned.Generation.Outages~ lag(8760))
  )

drift_ugo <- ugo |>
  model(
    `Drift` = NAIVE(Unplanned.Generation.Outages~ drift())
  )

snaive_ugo_6750_augment = augment(snaive_ugo_6750)
snaive_ugo_8760_augment = augment(snaive_ugo_8760)
drift_ugo_augment = augment(drift_ugo)

#' .fitted contains the fitted values;
#' .resid contains the residuals;
#' .innov contains the “innovation residuals” which, in this case,
#' are identical to the regular residuals.

res_plot(snaive_ugo_6750, snaive_ugo_6750_augment)
# mean is not zero
# are correlated
snaive_ugo_6750_augment |> features(.innov, box_pierce, lag = 2*6750)
snaive_ugo_6750_augment |> features(.innov, ljung_box, lag = 2*6750)

res_plot(snaive_ugo_8760, snaive_ugo_8760_augment)
# mean is not zero
# are correlated
# not normal
snaive_ugo_8760_augment |> features(.innov, box_pierce, lag = 2*8760)
snaive_ugo_8760_augment |> features(.innov, ljung_box, lag = 2*8760)

res_plot(drift_ugo, drift_ugo_augment)
# mean is zero
# are correlated
# not normally distributed
# residuals look most like random noise
drift_ugo_augment |> features(.innov, box_pierce, lag = 10)
drift_ugo_augment |> features(.innov, ljung_box, lag = 10)

################################################################################
# PDD Benchmark
################################################################################

snaive_pdd <- pdd |>
  model(
    `Seasonal naïve 7` = SNAIVE(Peak.Daily.Demand~ lag(7)),
    `Seasonal naïve 365` = SNAIVE(Peak.Daily.Demand~ lag(365)),
  )

# Generate forecasts for 31 days
snaive_pdd_fc <- snaive_pdd |> forecast(h = 31*3)

# Plot forecasts against actual values
snaive_pdd_fc |>
  autoplot(pdd, level = NULL) +
  labs(y = "PDD") +
  guides(colour = guide_legend(title = "Forecast")) +
  scale_color_manual(values=c("#FF72AA", "#5F91ED")) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=28),
        legend.title=element_text(size=28),
        legend.text=element_text(size=15)) +
  theme(legend.position = "bottom")+
  theme(plot.margin = unit(c(0, 1.6, 0, 0), "cm"))

###

snaive_pdd_7 <- pdd |>
  model(
    `Seasonal naïve 7` = SNAIVE(Peak.Daily.Demand~ lag(7))
  )

snaive_pdd_365 <- pdd |>
  model(
    `Seasonal naïve 365` = SNAIVE(Peak.Daily.Demand~ lag(365))
  )

snaive_pdd_7_augment = augment(snaive_pdd_7)
snaive_pdd_365_augment = augment(snaive_pdd_365)

res_plot(snaive_pdd_7, snaive_pdd_7_augment)
# mean is not zero (slightly above)
# are correlated
# not quite normal
snaive_pdd_7_augment |> features(.innov, box_pierce, lag = 2*7)
snaive_pdd_7_augment |> features(.innov, ljung_box, lag = 2*7)

res_plot(snaive_pdd_365, snaive_pdd_365_augment)
# mean is zero
# are correlated
# not normally distributed
snaive_pdd_365_augment |> features(.innov, box_pierce, lag = 2*365)
snaive_pdd_365_augment |> features(.innov, ljung_box, lag = 2*365)

################################################################################
# UGO forecasted using STL and ETS
################################################################################

# Forecasts from STL+ETS decomposition
my_dcmp_spec <- decomposition_model(
  STL(sqrt(Unplanned.Generation.Outages) ~
        season(period = 6570) +
        season(period = 8760),
      robust = TRUE),
  ETS(season_adjust ~ season("N"))
)

# Forecast for 31 days
fc <- ugo |>
  model(my_dcmp_spec) |>
  forecast(h = 24 * 31)

# Plot results with last year of data
fc |>
  fill_gaps() |>
  autoplot(ugo |> tail(24*365*4) |> fill_gaps()) +
  labs(y = "Unplanned.Generation.Outages",
       title = "Hourly UGO forecasted for one year")


################################################################################
# PDD forecasted using STL and ETS
################################################################################


# Forecasts from STL+ETS decomposition
my_dcmp_spec <- decomposition_model(
  STL(sqrt(Peak.Daily.Demand) ~ season(period = 7) +
        season(period = 365),
      robust = TRUE),
  ETS(season_adjust ~ season("N"))
)

# Forecast for 1 year
fc <- pdd |>
  model(my_dcmp_spec) |>
  forecast(h = 31)

# Plot results with last year of data

fc |>
  fill_gaps() |>
  autoplot(pdd |> tail(365*4) |> fill_gaps()) +
  labs(y = "PDD",
       title = "Daily PDD forecasted for one year")

################################################################################
# UGO dynamic harmonic regression model with an ARIMA error structure
################################################################################

fit <- ugo |>
  model(
    dhr = ARIMA(sqrt(Unplanned.Generation.Outages) ~ PDQ(0, 0, 0) + pdq(d = 0) +
                  fourier(period = 24, K = 10) +
                  fourier(period = 7*24, K = 5)))

fc <- fit |> forecast(h = 24*31)

fc |>
  fill_gaps() |>
  autoplot(ugo |> tail(24*365*4) |> fill_gaps()) +
  labs(y = "UGO",
       title = "Hourly UGO forecasted for one year")

################################################################################
# PDD dynamic harmonic regression model with an ARIMA error structure
################################################################################

fit <- pdd |>
  model(
    dhr = ARIMA(sqrt(Peak.Daily.Demand) ~ PDQ(0, 0, 0) + pdq(d = 0) +
                  fourier(period = 7, K = 10) +
                  fourier(period = 365, K = 5)))

fc <- fit |> forecast(h = 365)

fc |>
  fill_gaps() |>
  autoplot(pdd |> tail(365*4) |> fill_gaps()) +
  labs(y = "PDD",
       title = "Daily PDD forecasted for one year")
?ACF
autoplot(ACF(ugo, y = Unplanned.Generation.Outages, lag_max = 29000))
acf(ugo$Unplanned.Generation.Outages, lag.max = 29000, main = "")
# slowly decays over time
pacf(ugo$Unplanned.Generation.Outages, lag.max = 500, main = "")
# sinusoidal
acf(pdd$Peak.Daily.Demand, lag.max = 100000, main = "")
# sinusoidal
pacf(pdd$Peak.Daily.Demand, lag.max = 250, main = "")
# 0 after about 50

################################################################################
# AR UGO
################################################################################
pacf(ugo$Unplanned.Generation.Outages)
# bc = bind_cols(ugo,as_tibble(append(rep(NA, nrow(ugo)-length(best_stat_ugo)), best_stat_ugo)))

# First 2 very sig
report(ar_2_ugo  <- model(ugo, AR(Unplanned.Generation.Outages~order(2))))
ar_2_ugo_augment = augment(ar_2_ugo)
res_plot(ar_2_ugo, ar_2_ugo_augment)
# mean is 0
# are correlated but not hugely
ar_2_ugo_augment |> features(.innov, box_pierce)
ar_2_ugo_augment |> features(.innov, ljung_box)

# Then 10
report(ar_10_ugo <- model(ugo, AR(Unplanned.Generation.Outages~order(10))))
ar_10_ugo_augment = augment(ar_10_ugo)
res_plot(ar_10_ugo, ar_10_ugo_augment)
# mean is 0
# little correlation
ar_10_ugo_augment |> features(.innov, box_pierce)
ar_10_ugo_augment |> features(.innov, ljung_box)
# good!
?Box.test

# 25
report(ar_25_ugo  <- model(ugo, AR(Unplanned.Generation.Outages~order(25))))
ar_25_ugo_augment = augment(ar_25_ugo)
res_plot(ar_25_ugo, ar_25_ugo_augment)
# mean is 0
# are correlated but not hugely
ar_25_ugo_augment |> features(.innov, box_pierce)
ar_25_ugo_augment |> features(.innov, ljung_box)

# Can't pick up trend
forecast(ar_25_ugo, h = 24*31) |> autoplot(ugo)

# 2000
report(ar_2000_ugo  <- model(ugo, AR(Unplanned.Generation.Outages~order(2000))))
ar_2000_ugo_augment = augment(ar_2000_ugo)
res_plot(ar_2000_ugo, ar_2000_ugo_augment)
# mean is 0
# are correlated but not hugely
ar_2000_ugo_augment |> features(.innov, box_pierce)
ar_2000_ugo_augment |> features(.innov, ljung_box)

# 500
report(ar_500_ugo  <- model(ugo, AR(Unplanned.Generation.Outages~order(500))))
ar_500_ugo_augment = augment(ar_500_ugo)
res_plot(ar_500_ugo, ar_500_ugo_augment)
# mean is 0
# are correlated but not hugely
ar_500_ugo_augment |> features(.innov, box_pierce)
ar_500_ugo_augment |> features(.innov, ljung_box)

# 200
report(ar_200_ugo  <- model(ugo, AR(Unplanned.Generation.Outages~order(200))))
ar_200_ugo_augment = augment(ar_200_ugo)
res_plot(ar_200_ugo, ar_200_ugo_augment)
# mean is 0
# are correlated but not hugely
ar_200_ugo_augment |> features(.innov, box_pierce)
ar_200_ugo_augment |> features(.innov, ljung_box)

################################################################################
# AR PDD
################################################################################
pacf(pdd$Peak.Daily.Demand)
# best_stat_pdd

# First 1 very sig
report(ar_1_pdd <- model(pdd, AR(Peak.Daily.Demand~order(p = 1))))
ar_1_pdd_augment = augment(ar_1_pdd)
res_plot(ar_1_pdd, ar_1_pdd_augment)
# mean is 0
# lots of correlation
ar_1_pdd_augment |> features(.innov, box_pierce)
ar_1_pdd_augment |> features(.innov, ljung_box)

# Then 8
report(ar_8_pdd <- model(pdd, AR(Peak.Daily.Demand~order(8))))
ar_8_pdd_augment = augment(ar_8_pdd)
res_plot(ar_8_pdd, ar_8_pdd_augment)
# mean is 0
# little correlation
ar_8_pdd_augment |> features(.innov, box_pierce)
ar_8_pdd_augment |> features(.innov, ljung_box)
# good!

# Then 15
report(ar_15_pdd <- model(pdd, AR(Peak.Daily.Demand~order(15))))
ar_15_pdd_augment = augment(ar_15_pdd)
res_plot(ar_15_pdd, ar_15_pdd_augment)
# mean is 0
# little correlation
ar_15_pdd_augment |> features(.innov, box_pierce)
ar_15_pdd_augment |> features(.innov, ljung_box)

# Then 50
report(ar_50_pdd <- model(pdd, AR(Peak.Daily.Demand~order(50))))
ar_50_pdd_augment = augment(ar_50_pdd)
res_plot(ar_50_pdd, ar_50_pdd_augment)
# mean(ar_50_pdd_augment$.innov, na.rm = TRUE)
# mean is 0
# little correlation
ar_50_pdd_augment |> features(.innov, box_pierce)
ar_50_pdd_augment |> features(.innov, ljung_box)
# good!

# Then 225
report(ar_225_pdd <- model(pdd, AR(Peak.Daily.Demand~order(225))))
ar_225_pdd_augment = augment(ar_225_pdd)
res_plot(ar_225_pdd, ar_225_pdd_augment)
# mean is 0
# little correlation
ar_225_pdd_augment |> features(.innov, box_pierce)
ar_225_pdd_augment |> features(.innov, ljung_box)

forecast(ar_15_pdd, h = 31) |> autoplot(pdd)
# not terrible

################################################################################
# MA UGO
################################################################################
acf(ugo$Unplanned.Generation.Outages, lag.max = 500)

# 24
report(ma_24_ugo    <- model(ugo, ARIMA(Unplanned.Generation.Outages~ 0 + pdq(0, 0,24) + PDQ(0, 0, 0))))
ma_24_ugo_augment = augment(ma_24_ugo)
res_plot(ma_24_ugo, ma_24_ugo_augment)
mean(ma_24_ugo_augment$.innov, na.rm = TRUE)
# mean is 0
# little correlation
ma_24_ugo_augment |> features(.innov, box_pierce)
ma_24_ugo_augment |> features(.innov, ljung_box)
# saveRDS(ma_50_ugo, file = "ma_50_ugo.RDS")

# 48
report(ma_48_ugo  <- model(ugo, ARIMA(Unplanned.Generation.Outages~ 0 + pdq(0, 0, 48) + PDQ(0, 0, 0))))
ma_48_ugo_augment = augment(ma_48_ugo)
res_plot(ma_48_ugo, ma_48_ugo_augment)
# mean is 0
# little correlation
ma_48_ugo_augment |> features(.innov, box_pierce)
ma_48_ugo_augment |> features(.innov, ljung_box)
saveRDS(ma_48_ugo, file = "ma_48_ugo.RDS")
#
# # 350 - highest can be fitted
# report(ma_350_ugo  <- model(ugo, ARIMA(Unplanned.Generation.Outages~ 0 + pdq(0, 0, 350) + PDQ(0, 0, 0))))
# ma_350_ugo_augment = augment(ma_350_ugo)
# res_plot(ma_350_ugo, ma_350_ugo_augment)
# # mean is 0
# # little correlation
# ma_350_ugo_augment |> features(.innov, box_pierce)
# ma_350_ugo_augment |> features(.innov, ljung_box)
# saveRDS(ma_350_ugo, file = "ma_350_ugo.RDS")

# Can't pick up trend
# forecast(MA_7_pdd, h = 365) |> autoplot()

################################################################################
# MA PDD
################################################################################
acf(pdd$Peak.Daily.Demand, lag.max = 500)

# 7 - first close to zero
report(ma_7_pdd    <- model(pdd, ARIMA(Peak.Daily.Demand~ 0 + pdq(0, 0, 7) + PDQ(0, 0, 0))))
ma_7_pdd_augment = augment(ma_7_pdd)
res_plot(ma_7_pdd, ma_7_pdd_augment)
# mean is 0
# little correlation
ma_7_pdd_augment |> features(.innov, box_pierce)
ma_7_pdd_augment |> features(.innov, ljung_box)
# saveRDS(ma_90_pdd, file = "ma_90_pdd.RDS")

# 14 - first under sig
report(ma_14_pdd  <- model(pdd, ARIMA(Peak.Daily.Demand~ 0 + pdq(0, 0, 14) + PDQ(0, 0, 0))))
ma_14_pdd_augment = augment(ma_14_pdd)
res_plot(ma_14_pdd, ma_14_pdd_augment)
# mean is 0
# little correlation
ma_14_pdd_augment |> features(.innov, box_pierce)
ma_14_pdd_augment |> features(.innov, ljung_box)
# saveRDS(ma_66_pdd, file = "ma_66_pdd.RDS")

# 275 - second close to zero
# report(ma_275_pdd  <- model(pdd, ARIMA(Peak.Daily.Demand~ 0 + pdq(0, 0, 275) + PDQ(0, 0, 0))))
# ma_275_pdd_augment = augment(ma_275_pdd)
# res_plot(ma_275_pdd, ma_275_pdd_augment)
# # mean is 0
# # little correlation
# ma_275_pdd_augment |> features(.innov, box_pierce)
# ma_275_pdd_augment |> features(.innov, ljung_box)
# saveRDS(ma_275_pdd, file = "ma_275_pdd.RDS")

# Can't pick up trend
# forecast(, h = 365) |> autoplot()

################################################################################
# ARMA UGO STAT
################################################################################
acf(ugo$Unplanned.Generation.Outages, lag.max = 100)
pacf(ugo$Unplanned.Generation.Outages, lag.max = 100)

report(ARMA_1_1_ugo  <- model(ugo_stat_no_gaps, ARIMA(stat~0 + pdq(1,0,1))))

report(ARMA_24_25_ugo  <- model(ugo_stat_no_gaps, ARIMA(stat~0 + pdq(24,0,25))))

# Can't pick up trend
forecast(ARMA_1_1_ugo, h = 24*365) |> autoplot(ugo_stat_no_gaps)

################################################################################
# ARMA PDD STAT
################################################################################
acf(pdd_stat_no_gaps$stat, lag.max = 10)
pacf(pdd_stat_no_gaps$stat, lag.max = 10)

# First 2 very sig
report(ARMA_1_1_pdd  <- model(pdd_stat_no_gaps, ARIMA(stat~0 + pdq(1,0,1))))

report(ARMA_21_7_pdd  <- model(pdd_stat_no_gaps, ARIMA(stat~0 + pdq(1,0,7))))

# Can't pick up trend
forecast(ARMA_1_1_pdd, h = 365) |> autoplot(pdd_stat_no_gaps)

################################################################################
# ARIMA UGO
################################################################################

ARIMA_UGO <- ugo |>
  model(stepwise = ARIMA(Unplanned.Generation.Outages),
        search = ARIMA(Unplanned.Generation.Outages, stepwise=FALSE))

forecast(ARIMA_UGO, h = 24*365) |> autoplot(ugo)

################################################################################
# ARIMA UGO STAT
################################################################################

ARIMA_UGO_stat <- ugo_stat_no_gaps |>
  model(stepwise = ARIMA(stat),
        search = ARIMA(stat, stepwise=FALSE))

forecast(ARIMA_UGO_stat, h = 24*365) |> autoplot(ugo_stat_no_gaps)

################################################################################
# ARIMA PDD STAT
################################################################################

ARIMA_PDD <- pdd |>
  model(stepwise = ARIMA(Peak.Daily.Demand),
        search = ARIMA(Peak.Daily.Demand, stepwise=FALSE))

forecast(ARIMA_PDD, h = 365) |> autoplot(pdd)

################################################################################
# ARIMA PDD STAT
################################################################################

ARIMA_PDD_stat <- pdd_stat_no_gaps |>
  model(stepwise = ARIMA(stat),
        search = ARIMA(stat, stepwise=FALSE))

forecast(ARIMA_PDD_stat, h = 365) |> autoplot(pdd_stat_no_gaps)

ARIMA_PDD_stat |>
  select(search) |>
  gg_tsresiduals()

augment(ARIMA_PDD_stat) |>
  filter(.model=='search') |>
  features(.innov, ljung_box, lag = 200, dof = 3)

gg_arma(ARIMA_PDD_stat |> select(search))

?select
