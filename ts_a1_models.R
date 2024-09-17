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

res_plot2 = function(data, resids){
  if(colnames(data)[1] == "Day"){
    df = data$Day |> cbind(as.data.frame(matrix(c(resids), ncol = 1)))
  }
  else{
    df = data$Date.Time.Hour.Beginning |> cbind(as.data.frame(matrix(c(resids), ncol = 1)))
  }
  colnames(df) = c("Day", "Residual")
  tsib = as_tsibble(as_tibble(df))
  time_plot = autoplot(tsib) +
    labs(x = "Day", y = "Residuals")+
    theme(axis.text=element_text(size=15),
          axis.title=element_text(size=18))

  acf_plot  = tsib |>
    ACF(Residual) |>
    autoplot() + labs(x = "Lag", y = "ACF")+
    theme(axis.text=element_text(size=15),
          axis.title=element_text(size=18))

  hist = ggplot(tsib, aes(x = Residual)) +
    geom_histogram() +
    labs(x = "Residuals", y = "Count") +
    theme(axis.text=element_text(size=15),
          axis.title=element_text(size=18)) +
    scale_x_continuous(breaks = seq(-2000, 3000, by = 2000))

  grid.arrange(time_plot,
               arrangeGrob(acf_plot, hist, ncol = 2))
}

qq = function(dat){
  qqnorm(dat, main = "",
         cex.axis = 1.4,
         cex.lab = 1.3)
  qqline(dat)
}

aic = function(loglik, p, q, type, samp_size){
  k = 2
  if(type == "bic"){
    k = log(p+q)
  }
  aic = -2*loglik + k*(p+q)
  if(type == "aicc"){
    return(aic+(2*(p+q)*(p+q+1))/(samp_size-p-q-1))
  }
  aic
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
# First 2 very sig
# report(ar_2_ugo  <- model(ugo, AR(Unplanned.Generation.Outages~order(2))))
ar_2_ugo = readRDS(file = "ar_2_ugo.RDS")
report(ar_2_ugo)
ar_2_ugo_augment = augment(ar_2_ugo)
res_plot(ar_2_ugo, ar_2_ugo_augment)
mean(ar_2_ugo_augment$.innov, na.rm = TRUE)
ar_2_ugo_augment |> features(.innov, ljung_box)
qq(ar_2_ugo_augment$.innov)
# saveRDS(ar_2_ugo, file = "ar_2_ugo.RDS")

# 25
# report(ar_25_ugo  <- model(ugo, AR(Unplanned.Generation.Outages~order(25))))
ar_25_ugo = readRDS(file = "ar_25_ugo.RDS")
report(ar_25_ugo)
ar_25_ugo_augment = augment(ar_25_ugo)
res_plot(ar_25_ugo, ar_25_ugo_augment)
mean(ar_25_ugo_augment$.innov, na.rm = TRUE)
ar_25_ugo_augment |> features(.innov, ljung_box)
qq(ar_25_ugo_augment$.innov)
# saveRDS(ar_25_ugo, file = "ar_25_ugo.RDS")

# 2000
# report(ar_2000_ugo  <- model(ugo, AR(Unplanned.Generation.Outages~order(2000))))
ar_2000_ugo = readRDS(file = "ar_2000_ugo.RDS")
report(ar_2000_ugo)
ar_2000_ugo_augment = augment(ar_2000_ugo)
res_plot(ar_2000_ugo, ar_2000_ugo_augment)
mean(ar_2000_ugo_augment$.innov, na.rm = TRUE)
ar_2000_ugo_augment |> features(.innov, ljung_box)
qq(ar_2000_ugo_augment$.innov)
# saveRDS(ar_2000_ugo, file = "ar_2000_ugo.RDS")

# 500
# report(ar_500_ugo  <- model(ugo, AR(Unplanned.Generation.Outages~order(500))))
ar_500_ugo = readRDS(file = "ar_500_ugo.RDS")
report(ar_500_ugo)
ar_500_ugo_augment = augment(ar_500_ugo)
res_plot(ar_500_ugo, ar_500_ugo_augment)
mean(ar_500_ugo_augment$.innov, na.rm = TRUE)
ar_500_ugo_augment |> features(.innov, ljung_box)
qq(ar_500_ugo_augment$.innov)
# saveRDS(ar_500_ugo, file = "ar_500_ugo.RDS")

# 200
# report(ar_200_ugo  <- model(ugo, AR(Unplanned.Generation.Outages~order(200))))
ar_200_ugo = readRDS(file = "ar_200_ugo.RDS")
report(ar_200_ugo)
ar_200_ugo_augment = augment(ar_200_ugo)
res_plot(ar_200_ugo, ar_200_ugo_augment)
mean(ar_200_ugo_augment$.innov, na.rm = TRUE)
ar_200_ugo_augment |> features(.innov, ljung_box)
qq(ar_200_ugo_augment$.innov)
# saveRDS(ar_200_ugo, file = "ar_200_ugo.RDS")

################################################################################
# AR PDD
################################################################################
# First 1 very sig
# report(ar_1_pdd <- model(pdd, AR(Peak.Daily.Demand~order(p = 1))))
ar_1_pdd = readRDS(file = "ar_1_pdd.RDS")
report(ar_1_pdd)
ar_1_pdd_augment = augment(ar_1_pdd)
res_plot(ar_1_pdd, ar_1_pdd_augment)
mean(ar_1_pdd_augment$.innov, na.rm = TRUE)
ar_1_pdd_augment |> features(.innov, ljung_box)
qq(ar_1_ugo_augment$.innov)
# saveRDS(ar_1_pdd, file = "ar_1_pdd.RDS")

# Then 8
# report(ar_8_pdd <- model(pdd, AR(Peak.Daily.Demand~order(8))))
ar_8_pdd = readRDS(file = "ar_8_pdd.RDS")
report(ar_8_pdd)
ar_8_pdd_augment = augment(ar_8_pdd)
res_plot(ar_8_pdd, ar_8_pdd_augment)
mean(ar_8_pdd_augment$.innov, na.rm = TRUE)
ar_8_pdd_augment |> features(.innov, ljung_box)
qq(ar_8_pdd_augment$.innov)
# saveRDS(ar_8_pdd, file = "ar_8_pdd.RDS")
# good!

# Then 15
# report(ar_15_pdd <- model(pdd, AR(Peak.Daily.Demand~order(15))))
ar_15_pdd = readRDS(file = "ar_15_pdd.RDS")
report(ar_15_pdd)
ar_15_pdd_augment = augment(ar_15_pdd)
res_plot(ar_15_pdd, ar_15_pdd_augment)
mean(ar_15_pdd_augment$.innov, na.rm = TRUE)
ar_15_pdd_augment |> features(.innov, ljung_box)
qq(ar_15_pdd_augment$.innov)
# saveRDS(ar_15_pdd, file = "ar_15_pdd.RDS")

# Then 50
# report(ar_50_pdd <- model(pdd, AR(Peak.Daily.Demand~order(50))))
ar_50_pdd = readRDS(file = "ar_50_pdd.RDS")
report(ar_50_pdd)
ar_50_pdd_augment = augment(ar_50_pdd)
res_plot(ar_50_pdd, ar_50_pdd_augment)
mean(ar_50_pdd_augment$.innov, na.rm = TRUE)
ar_50_pdd_augment |> features(.innov, ljung_box)
qq(ar_50_pdd_augment$.innov)
# saveRDS(ar_50_pdd, file = "ar_50_pdd.RDS")
# good!

# Then 225
# report(ar_225_pdd <- model(pdd, AR(Peak.Daily.Demand~order(225))))
ar_225_pdd = readRDS(file = "ar_225_pdd.RDS")
report(ar_225_pdd)
ar_225_pdd_augment = augment(ar_225_pdd)
res_plot(ar_225_pdd, ar_225_pdd_augment)
mean(ar_225_pdd_augment$.innov, na.rm = TRUE)
ar_225_pdd_augment |> features(.innov, ljung_box)
qq(ar_225_pdd_augment$.innov)
# saveRDS(ar_225_pdd, file = "ar_225_pdd.RDS")


################################################################################
# MA UGO
################################################################################
# 24
# report(ma_24_ugo    <- model(ugo, ARIMA(Unplanned.Generation.Outages~ 0 + pdq(0, 0,24) + PDQ(0, 0, 0))))
ma_24_ugo = readRDS(file = "ma_24_ugo.RDS")
report(ma_24_ugo)
ma_24_ugo_augment = augment(ma_24_ugo)
res_plot(ma_24_ugo, ma_24_ugo_augment)
mean(ma_24_ugo_augment$.innov, na.rm = TRUE)
ma_24_ugo_augment |> features(.innov, ljung_box)
qq(ma_24_ugo_augment$.innov)
# saveRDS(ma_24_ugo, file = "ma_24_ugo.RDS")

# 48
# report(ma_48_ugo  <- model(ugo, ARIMA(Unplanned.Generation.Outages~ 0 + pdq(0, 0, 48) + PDQ(0, 0, 0))))
ma_48_ugo = readRDS(file = "ma_48_ugo.RDS")
report(ma_48_ugo)
ma_48_ugo_augment = augment(ma_48_ugo)
res_plot(ma_48_ugo, ma_48_ugo_augment)
mean(ma_48_ugo_augment$.innov, na.rm = TRUE)
ma_48_ugo_augment |> features(.innov, ljung_box)
qq(ma_48_ugo_augment$.innov)
# ma_48_ugo = saveRDS(ma_48_ugo, file = "ma_48_ugo.RDS")

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
# 7 - first close to zero
# report(ma_7_pdd    <- model(pdd, ARIMA(Peak.Daily.Demand~ 0 + pdq(0, 0, 7) + PDQ(0, 0, 0))))
ma_7_pdd = readRDS(file = "ma_7_pdd.RDS")
report(ma_7_pdd)
ma_7_pdd_augment = augment(ma_7_pdd)
res_plot(ma_7_pdd, ma_7_pdd_augment)
mean(ma_7_pdd_augment$.innov, na.rm = TRUE)
ma_7_pdd_augment |> features(.innov, ljung_box)
qq(ma_7_pdd_augment$.innov)
# saveRDS(ma_7_pdd, file = "ma_7_pdd.RDS")

# 14 - first under sig
# report(ma_14_pdd  <- model(pdd, ARIMA(Peak.Daily.Demand~ 0 + pdq(0, 0, 14) + PDQ(0, 0, 0))))
ma_14_pdd = readRDS(file = "ma_14_pdd.RDS")
report(ma_14_pdd)
ma_14_pdd_augment = augment(ma_14_pdd)
res_plot(ma_14_pdd, ma_14_pdd_augment)
mean(ma_14_pdd_augment$.innov, na.rm = TRUE)
ma_14_pdd_augment |> features(.innov, ljung_box)
qq(ma_14_pdd_augment$.innov)
# saveRDS(ma_14_pdd, file = "ma_14_pdd.RDS")

# 275 - second close to zero
# report(ma_275_pdd  <- model(pdd, ARIMA(Peak.Daily.Demand~ 0 + pdq(0, 0, 275) + PDQ(0, 0, 0))))
# ma_275_pdd_augment = augment(ma_275_pdd)
# res_plot(ma_275_pdd, ma_275_pdd_augment)
# # mean is 0
# # little correlation
# ma_275_pdd_augment |> features(.innov, box_pierce)
# ma_275_pdd_augment |> features(.innov, ljung_box)
# saveRDS(ma_275_pdd, file = "ma_275_pdd.RDS")

################################################################################
# ARMA PDD
################################################################################

# ARMA_2_7_pdd = Arima(pdd$Peak.Daily.Demand, order=c(2,0,7), seasonal=list(order=c(0,0,0),period=NA),
#              method="ML")
ARMA_2_7_pdd = readRDS(file = "ARMA_2_7_pdd.RDS")
ARMA_2_7_pdd
ARMA_2_7_pdd_augment = resid(ARMA_2_7_pdd)
res_plot2(pdd, ARMA_2_7_pdd_augment)
mean(resid(ARMA_2_7_pdd))
Box.test(resid(ARMA_2_7_pdd), type = "Ljung-Box")
qq(ARMA_2_7_pdd_augment)
# saveRDS(ARMA_2_7_pdd, file = "ARMA_2_7_pdd.RDS")

# ARMA_2_14_pdd = Arima(pdd$Peak.Daily.Demand, order=c(2,0,14), seasonal=list(order=c(0,0,0),period=NA),
#       method="ML")
ARMA_2_14_pdd = readRDS(file = "ARMA_2_14_pdd.RDS")
ARMA_2_14_pdd
ARMA_2_14_pdd_augment = resid(ARMA_2_14_pdd)
res_plot2(pdd, ARMA_2_14_pdd_augment)
mean(resid(ARMA_2_14_pdd))
Box.test(resid(ARMA_2_14_pdd), type = "Ljung-Box")
qq(ARMA_2_14_pdd_augment)
# saveRDS(ARMA_2_14_pdd, file = "ARMA_2_14_pdd.RDS")


# report(ARMA_25_7_pdd  <- model(pdd, ARIMA(Peak.Daily.Demand ~ 0 + pdq(25,0,7) + PDQ(0, 0, 0))))
ARMA_25_7_pdd = readRDS(file = "ARMA_25_7_pdd.RDS")
report(ARMA_25_7_pdd)
ARMA_25_7_pdd_augment = augment(ARMA_25_7_pdd)
res_plot(ARMA_25_7_pdd, ARMA_25_7_pdd_augment)
mean(ARMA_25_7_pdd_augment$.innov, na.rm = TRUE)
ARMA_25_7_pdd_augment |> features(.innov, ljung_box)
qq(ARMA_25_7_pdd_augment$.innov)
# saveRDS(ARMA_25_7_pdd, file = "ARMA_25_7_pdd.RDS")


# report(ARMA_25_14_pdd  <- model(pdd, ARIMA(Peak.Daily.Demand ~ 0 + pdq(25,0,14) + PDQ(0, 0, 0))))
ARMA_25_14_pdd = readRDS(file = "ARMA_25_14_pdd.RDS")
report(ARMA_25_14_pdd)
ARMA_25_14_pdd_augment = augment(ARMA_25_14_pdd)
res_plot(ARMA_25_14_pdd, ARMA_25_14_pdd_augment)
mean(ARMA_25_14_pdd_augment$.innov, na.rm = TRUE)
ARMA_25_14_pdd_augment |> features(.innov, ljung_box)
qq(ARMA_25_14_pdd_augment$.innov)
# saveRDS(ARMA_25_14_pdd, file = "ARMA_25_14_pdd.RDS")


# (ARMA_6_6_pdd = Arima(pdd$Peak.Daily.Demand, order=c(6,0,6), seasonal=list(order=c(0,0,0),period=NA),
#                       method="ML"))
ARMA_6_6_pdd = readRDS(file = "ARMA_6_6_pdd.RDS")
ARMA_6_6_pdd
ARMA_6_6_pdd_augment = resid(ARMA_6_6_pdd)
res_plot2(pdd, ARMA_6_6_pdd_augment)
mean(ARMA_6_6_pdd_augment)
Box.test(ARMA_6_6_pdd_augment, type = "Ljung-Box")
qq(ARMA_6_6_pdd_augment)
# saveRDS(ARMA_6_6_pdd, file = "ARMA_6_6_pdd.RDS")

# ARMA_7_7_pdd = Arima(pdd$Peak.Daily.Demand, order=c(7,0,7), seasonal=list(order=c(0,0,0),period=NA),
#                       method="ML")
ARMA_7_7_pdd = readRDS(file = "ARMA_7_7_pdd.RDS")
ARMA_7_7_pdd
ARMA_7_7_pdd_augment = resid(ARMA_7_7_pdd)
res_plot2(pdd, ARMA_7_7_pdd_augment)
mean(ARMA_7_7_pdd_augment)
Box.test(ARMA_7_7_pdd_augment, type = "Ljung-Box")
qq(ARMA_7_7_pdd_augment)
# saveRDS(ARMA_7_7_pdd, file = "ARMA_7_7_pdd.RDS")


################################################################################
# ARMA UGO
################################################################################
# report(ARMA_5_1_ugo    <- model(ugo, ARIMA(Unplanned.Generation.Outages ~ 0 + pdq(5,0,1) + PDQ(0, 0, 0))))
ARMA_5_1_ugo = readRDS(file = "ARMA_5_1_ugo.RDS")
report(ARMA_5_1_ugo)
ARMA_5_1_ugo_augment = augment(ARMA_5_1_ugo)
res_plot(ARMA_5_1_ugo, ARMA_5_1_ugo_augment)
mean(ARMA_5_1_ugo_augment$.innov, na.rm = TRUE)
ARMA_5_1_ugo_augment |> features(.innov, ljung_box)
qq(ARMA_5_1_ugo_augment$.innov)
# saveRDS(ARMA_5_1_ugo, file = "ARMA_5_1_ugo.RDS")


# report(ARMA_12_2_ugo   <- model(ugo, ARIMA(Unplanned.Generation.Outages ~ 0 + pdq(12,0,2) + PDQ(0, 0, 0))))
ARMA_12_2_ugo = readRDS(file = "ARMA_12_2_ugo.RDS")
report(ARMA_12_2_ugo)
ARMA_12_2_ugo_augment = augment(ARMA_12_2_ugo)
res_plot(ARMA_12_2_ugo, ARMA_12_2_ugo_augment)
mean(ARMA_12_2_ugo_augment$.innov, na.rm = TRUE)
ARMA_12_2_ugo_augment |> features(.innov, ljung_box)
qq(ARMA_12_2_ugo_augment$.innov)
# saveRDS(ARMA_12_2_ugo, file = "ARMA_12_2_ugo.RDS")


# report(ARMA_12_12_ugo  <- model(ugo, ARIMA(Unplanned.Generation.Outages ~ 0 + pdq(12,0,12) + PDQ(0, 0, 0))))
ARMA_12_12_ugo = readRDS(file = "ARMA_12_12_ugo.RDS")
report(ARMA_12_12_ugo)
ARMA_12_12_ugo_augment = augment(ARMA_12_12_ugo)
res_plot(ARMA_12_12_ugo, ARMA_12_12_ugo_augment)
mean(ARMA_12_12_ugo_augment$.innov, na.rm = TRUE)
ARMA_12_12_ugo_augment |> features(.innov, ljung_box)
qq(ARMA_12_12_ugo_augment$.innov)
# saveRDS(ARMA_12_12_ugo, file = "ARMA_12_12_ugo.RDS")


# report(ARMA_24_2_ugo   <- model(ugo, ARIMA(Unplanned.Generation.Outages ~ 0 + pdq(24,0,2) + PDQ(0, 0, 0))))
ARMA_24_2_ugo = readRDS(file = "ARMA_24_2_ugo.RDS")
report(ARMA_24_2_ugo)
ARMA_24_2_ugo_augment = augment(ARMA_24_2_ugo)
res_plot(ARMA_24_2_ugo, ARMA_24_2_ugo_augment)
mean(ARMA_24_2_ugo_augment$.innov, na.rm = TRUE)
ARMA_24_2_ugo_augment |> features(.innov, ljung_box)
qq(ARMA_24_2_ugo_augment$.innov)
# saveRDS(ARMA_24_2_ugo, file = "ARMA_24_2_ugo.RDS")


# report(ARMA_24_12_ugo  <- model(ugo, ARIMA(Unplanned.Generation.Outages ~ 0 + pdq(24,0,12) + PDQ(0, 0, 0))))
ARMA_24_12_ugo = readRDS(file = "ARMA_24_12_ugo.RDS")
report(ARMA_24_12_ugo)
ARMA_24_12_ugo_augment = augment(ARMA_24_12_ugo)
res_plot(ARMA_24_12_ugo, ARMA_24_12_ugo_augment)
mean(ARMA_24_12_ugo_augment$.innov, na.rm = TRUE)
ARMA_24_12_ugo_augment |> features(.innov, ljung_box)
qq(ARMA_24_12_ugo_augment$.innov)
# saveRDS(ARMA_24_12_ugo, file = "ARMA_24_12_ugo.RDS")


# report(ARMA_24_24_ugo  <- model(ugo, ARIMA(Unplanned.Generation.Outages ~ 0 + pdq(24,0,24) + PDQ(0, 0, 0))))
ARMA_24_24_ugo = readRDS(file = "ARMA_24_24_ugo.RDS")
report(ARMA_24_24_ugo)
ARMA_24_24_ugo_augment = augment(ARMA_24_24_ugo)
res_plot(ARMA_24_24_ugo, ARMA_24_24_ugo_augment)
mean(ARMA_24_24_ugo_augment$.innov, na.rm = TRUE)
ARMA_24_24_ugo_augment |> features(.innov, ljung_box)
qq(ARMA_24_24_ugo_augment$.innov)
# saveRDS(ARMA_24_24_ugo, file = "ARMA_24_24_ugo.RDS")

################################################################################
# ARIMA UGO
################################################################################
report(model(ugo, ARIMA(Unplanned.Generation.Outages ~ 0 + pdq(6,0,2) + PDQ(0, 0, 0))))
# report(ARIMA_5_1_ugo    <- model(ugo, ARIMA(Unplanned.Generation.Outages ~ 0 + pdq(5,1,1) + PDQ(0, 0, 0))))
ARIMA_5_1_ugo = readRDS(file = "ARIMA_5_1_ugo.RDS")
report(ARIMA_5_1_ugo)
ARIMA_5_1_ugo_augment = augment(ARIMA_5_1_ugo)
res_plot(ARIMA_5_1_ugo, ARIMA_5_1_ugo_augment)
mean(ARIMA_5_1_ugo_augment$.innov, na.rm = TRUE)
ARIMA_5_1_ugo_augment |> features(.innov, ljung_box)
qq(ARIMA_5_1_ugo_augment$.innov)
# saveRDS(ARIMA_5_1_ugo, file = "ARIMA_5_1_ugo.RDS")


# report(ARIMA_12_2_ugo   <- model(ugo, ARIMA(Unplanned.Generation.Outages ~ 0 + pdq(12,1,2) + PDQ(0, 0, 0))))
ARIMA_12_2_ugo = readRDS(file = "ARIMA_12_2_ugo.RDS")
report(ARIMA_12_2_ugo)
ARIMA_12_2_ugo_augment = augment(ARIMA_12_2_ugo)
res_plot(ARIMA_12_2_ugo, ARIMA_12_2_ugo_augment)
mean(ARIMA_12_2_ugo_augment$.innov, na.rm = TRUE)
ARIMA_12_2_ugo_augment |> features(.innov, ljung_box)
qq(ARIMA_12_2_ugo_augment$.innov)
# saveRDS(ARIMA_12_2_ugo, file = "ARIMA_12_2_ugo.RDS")


# report(ARIMA_12_12_ugo   <- model(ugo, ARIMA(Unplanned.Generation.Outages ~ 0 + pdq(12,1,12) + PDQ(0, 0, 0))))
ARIMA_12_12_ugo = readRDS(file = "ARIMA_12_12_ugo.RDS")
report(ARIMA_12_12_ugo)
ARIMA_12_12_ugo_augment = augment(ARIMA_12_12_ugo)
res_plot(ARIMA_12_12_ugo, ARIMA_12_12_ugo_augment)
mean(ARIMA_12_12_ugo_augment$.innov, na.rm = TRUE)
ARIMA_12_12_ugo_augment |> features(.innov, ljung_box)
qq(ARIMA_12_12_ugo_augment$.innov)
# saveRDS(ARIMA_12_12_ugo, file = "ARIMA_12_12_ugo.RDS")


# report(ARIMA_24_2_ugo   <- model(ugo, ARIMA(Unplanned.Generation.Outages ~ 0 + pdq(24,1,2) + PDQ(0, 0, 0))))
ARIMA_24_2_ugo = readRDS(file = "ARIMA_24_2_ugo.RDS")
report(ARIMA_24_2_ugo)
ARIMA_24_2_ugo_augment = augment(ARIMA_24_2_ugo)
res_plot(ARIMA_24_2_ugo, ARIMA_24_2_ugo_augment)
mean(ARIMA_24_2_ugo_augment$.innov, na.rm = TRUE)
ARIMA_24_2_ugo_augment |> features(.innov, ljung_box)
qq(ARIMA_24_2_ugo_augment$.innov)
# saveRDS(ARIMA_24_2_ugo, file = "ARIMA_24_2_ugo.RDS")


# report(ARIMA_12_12_ugo  <- model(ugo, ARIMA(Unplanned.Generation.Outages ~ 0 + pdq(12,1,12) + PDQ(0, 0, 0))))
# ARIMA_12_12_ugo_augment = augment(ARIMA_12_12_ugo)
# res_plot(ARIMA_12_12_ugo, ARIMA_12_12_ugo_augment)
# mean(ARIMA_12_12_ugo_augment$.innov, na.rm = TRUE)
# ARIMA_12_12_ugo_augment |> features(.innov, ljung_box)
# saveRDS(ARIMA_12_12_ugo, file = "ARIMA_12_12_ugo.RDS")


# report(ARIMA_24_12_ugo  <- model(ugo, ARIMA(Unplanned.Generation.Outages ~ 0 + pdq(24,1,12) + PDQ(0, 0, 0))))
ARIMA_24_12_ugo = readRDS(file = "ARIMA_24_12_ugo.RDS")
report(ARIMA_24_12_ugo)
ARIMA_24_12_ugo_augment = augment(ARIMA_24_12_ugo)
res_plot(ARIMA_24_12_ugo, ARIMA_24_12_ugo_augment)
mean(ARIMA_24_12_ugo_augment$.innov, na.rm = TRUE)
ARIMA_24_12_ugo_augment |> features(.innov, ljung_box)
qq(ARIMA_24_12_ugo_augment$.innov)
# saveRDS(ARIMA_24_12_ugo, file = "ARIMA_24_12_ugo.RDS")


# report(ARIMA_24_24_ugo  <- model(ugo, ARIMA(Unplanned.Generation.Outages ~ 0 + pdq(24,1,24) + PDQ(0, 0, 0))))
ARIMA_24_24_ugo = readRDS(file = "ARIMA_24_24_ugo.RDS")
report(ARIMA_24_24_ugo)
ARIMA_24_24_ugo_augment = augment(ARIMA_24_24_ugo)
res_plot(ARIMA_24_24_ugo, ARIMA_24_24_ugo_augment)
mean(ARIMA_24_24_ugo_augment$.innov, na.rm = TRUE)
ARIMA_24_24_ugo_augment |> features(.innov, ljung_box)
qq(ARIMA_24_24_ugo_augment$.innov)
# saveRDS(ARIMA_24_24_ugo, file = "ARIMA_24_24_ugo.RDS")


################################################################################
# ARIMA PDD
################################################################################

# report(ARIMA_25_7_pdd  <- model(pdd, ARIMA(Peak.Daily.Demand ~ 0 + pdq(25,1,7) + PDQ(0, 0, 0))))
# ARIMA_25_7_pdd = readRDS(file = "ARIMA_25_7_pdd.RDS")
# report(ARIMA_25_7_pdd)
# ARIMA_25_7_pdd_augment = augment(ARIMA_25_7_pdd)
# res_plot(ARIMA_25_7_pdd, ARIMA_25_7_pdd_augment)
# mean(ARIMA_25_7_pdd_augment$.innov, na.rm = TRUE)
# ARIMA_25_7_pdd_augment |> features(.innov, ljung_box)
# saveRDS(ARIMA_25_7_pdd, file = "ARIMA_25_7_pdd.RDS")

# ARIMA_25_7_pdd = Arima(pdd$Peak.Daily.Demand, order=c(25,1,7), seasonal=list(order=c(0,0,0),period=NA),
#                  method="CSS")
ARIMA_25_7_pdd = readRDS(file = "ARIMA_25_7_pdd.RDS")
ARIMA_25_7_pdd
aic(ARIMA_25_7_pdd$loglik, 25, 7, "aic" ,nrow(pdd))
aic(ARIMA_25_7_pdd$loglik, 25, 7, "aicc",nrow(pdd))
aic(ARIMA_25_7_pdd$loglik, 25, 7, "bic" ,nrow(pdd))
ARIMA_25_7_pdd_augment = resid(ARIMA_25_7_pdd)
res_plot2(pdd, ARIMA_25_7_pdd_augment)
mean(resid(ARIMA_25_7_pdd))
Box.test(resid(ARIMA_25_7_pdd), type = "Ljung-Box")
qq(ARIMA_25_7_pdd_augment)
# saveRDS(ARIMA_25_7_pdd, file = "ARIMA_25_7_pdd.RDS")

# report(ARIMA_25_24_pdd  <- model(pdd, ARIMA(Peak.Daily.Demand ~ 0 + pdq(25,1,24) + PDQ(0, 0, 0))))
# ARIMA_25_24_pdd = readRDS(file = "ARIMA_25_24_pdd.RDS")
# report(ARIMA_25_24_pdd)
# ARIMA_25_24_pdd_augment = augment(ARIMA_25_24_pdd)
# res_plot(ARIMA_25_24_pdd, ARIMA_25_24_pdd_augment)
# mean(ARIMA_25_24_pdd_augment$.innov, na.rm = TRUE)
# ARIMA_25_24_pdd_augment |> features(.innov, ljung_box)
# saveRDS(ARIMA_25_24_pdd, file = "ARIMA_25_24_pdd.RDS")

# ARIMA_25_14_pdd = Arima(pdd$Peak.Daily.Demand, order=c(25,1,14), seasonal=list(order=c(0,0,0),period=NA),
#                        method="ML")
ARIMA_25_14_pdd = readRDS(file = "ARIMA_25_14_pdd.RDS")
ARIMA_25_14_pdd
ARIMA_25_14_pdd_augment = resid(ARIMA_25_14_pdd)
res_plot2(pdd, ARIMA_25_14_pdd_augment)
mean(resid(ARIMA_25_14_pdd))
Box.test(resid(ARIMA_25_14_pdd), type = "Ljung-Box")
qq(ARIMA_25_14_pdd_augment)
# saveRDS(ARIMA_25_14_pdd, file = "ARIMA_25_14_pdd.RDS")

# report(ARIMA_7_7_pdd  <- model(pdd, ARIMA(Peak.Daily.Demand ~ 0 + pdq(7,1,7) + PDQ(0, 0, 0))))
# ARIMA_7_7_pdd = readRDS(file = "ARIMA_7_7_pdd.RDS")
# report(ARIMA_7_7_pdd)
# ARIMA_7_7_pdd_augment = augment(ARIMA_7_7_pdd)
# res_plot(ARIMA_7_7_pdd, ARIMA_7_7_pdd_augment)
# mean(ARIMA_7_7_pdd_augment$.innov, na.rm = TRUE)
# ARIMA_7_7_pdd_augment |> features(.innov, ljung_box)
# saveRDS(ARIMA_7_7_pdd, file = "ARIMA_7_7_pdd.RDS")

# ARIMA_7_7_pdd = Arima(pdd$Peak.Daily.Demand, order=c(7,1,7), seasonal=list(order=c(0,0,0),period=NA),
#                         method="ML")
ARIMA_7_7_pdd = readRDS(file = "ARIMA_7_7_pdd.RDS")
ARIMA_7_7_pdd
ARIMA_7_7_pdd_augment = resid(ARIMA_7_7_pdd)
res_plot2(pdd, ARIMA_7_7_pdd_augment)
mean(resid(ARIMA_7_7_pdd))
Box.test(resid(ARIMA_7_7_pdd), type = "Ljung-Box")
qq(ARIMA_7_7_pdd_augment)
# saveRDS(ARIMA_7_7_pdd, file = "ARIMA_7_7_pdd.RDS")

# ARIMA_6_6_pdd = Arima(pdd$Peak.Daily.Demand, order=c(6,1,6), seasonal=list(order=c(0,0,0),period=NA),
#                       method="ML")
ARIMA_6_6_pdd = readRDS(file = "ARIMA_6_6_pdd.RDS")
ARIMA_6_6_pdd
ARIMA_6_6_pdd_augment = resid(ARIMA_6_6_pdd)
res_plot2(pdd, ARIMA_6_6_pdd_augment)
mean(resid(ARIMA_6_6_pdd))
Box.test(resid(ARIMA_6_6_pdd), type = "Ljung-Box")
qq(ARIMA_6_6_pdd_augment)
# saveRDS(ARIMA_6_6_pdd, file = "ARIMA_6_6_pdd.RDS")













