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
ugo  = readRDS("ugo_train.RDS")
ugo  = ugo[,c(1,24)]

pdd  = readRDS("pdd_train.RDS")
pdd  = pdd[,c(1,3)]

################################################################################
# Time Plots of Variables of Interest
################################################################################

# Peak Daily Demand
autoplot(pdd, Peak.Daily.Demand) +
  labs(y = "Peak Daily Demand",
       title = "", x = "Day") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=26))

# Major seasonality, with the same patterns as for Total Daily Demand

# Display
plots = pdd |> gg_tsdisplay(Peak.Daily.Demand, plot_type='partial')


plots[[1]] = autoplot(pdd, Peak.Daily.Demand) +
  labs(x = "Date", y = "PDD")+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=18))

plots[[2]] = plots[[2]] + labs(x = "Lag", y = "ACF")+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=18))
plots[[3]] = plots[[3]] + labs(x = "Lag", y = "PACF")+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=18))

plots

# ACF
pdd |>
  ACF(Peak.Daily.Demand,lag_max = 275) |>
  autoplot() +
  labs(y = "ACF",
       title = "", x = "Lag") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=26))+
  scale_x_continuous(breaks = seq(0, 1000, by = 50))
# PACF
pdd |>
  PACF(Peak.Daily.Demand,lag_max = 15000) |>
  autoplot() +
  labs(y = "PACF",
       title = "", x = "Lag") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=26))+
  scale_x_continuous(breaks = seq(0, 1000, by = 500))

# Unplanned Generation Outages
autoplot(ugo, Unplanned.Generation.Outages) +
  labs(y = "Unplanned Generation Outages",
       title = "", x = "Day") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=26))
# Trend increasing, with no clear seasonality. Outlying peak during initial
# lockdown

# Display
plots = ugo |> gg_tsdisplay(Unplanned.Generation.Outages, plot_type='partial')

plots[[1]] = autoplot(ugo, Unplanned.Generation.Outages) +
  labs(x = "Date", y = "UGO")+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=18))

plots[[2]] = plots[[2]] + labs(x = "Lag", y = "ACF")+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=18))
plots[[3]] = plots[[3]] + labs(x = "Lag", y = "PACF")+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=18))

plots

# Peak Daily Demand is not stationary. Should be differenced.
# ACF
ugo |>
  ACF(Unplanned.Generation.Outages,lag_max = 29000) |>
  autoplot() +
  labs(y = "ACF",
       title = "", x = "Lag") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=26))+
  scale_x_continuous(breaks = seq(0, 29000, by = 7500))
# PACF
ugo |>
  PACF(Unplanned.Generation.Outages,lag_max = 300000) |>
  autoplot() +
  labs(y = "PACF",
       title = "", x = "Lag") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=26))+
  scale_x_continuous(breaks = seq(0, 29000, by = 7500))

################################################################################
# Investigate Complex Seasonality
################################################################################
sep = pdd %>%
  model(STL(Peak.Daily.Demand ~ season(period = 7) + season(period = 365))) %>%
  components() %>% autoplot() + title("")


ugo %>%
  model(STL(Unplanned.Generation.Outages ~ season(period = 24)+ season(period = 168)+ season(period = 6570)+ season(period = 8760))) %>%
  components() %>% autoplot()

################################################################################
# Detrending UGO
################################################################################

# covid in
# summary(fit1 <- lm(data = tsib, Unplanned.Generation.Outages~Date.Time.Hour.Beginning))
# autoplot(tsib, Unplanned.Generation.Outages) +
#   labs(y = "Unplanned Generation Outages",
#        title = "", x = "Day") +
#   theme(axis.text=element_text(size=20),
#         axis.title=element_text(size=28)) +
#   geom_abline(slope = fit1$coefficients[2], intercept = fit1$coefficients[1], colour = "#3D3C9F", linewidth = 2)
#
# tsib$rownum = 1:nrow(tsib)
# tsib2 = tsib %>%
#   dplyr::mutate(detrended_UGO = Peak.Daily.Demand -fit1$coefficients[1] - fit1$coefficients[2]*rownum)
#
# autoplot(tsib2, detrended_UGO) +
#   labs(y = "Detrended UGO",
#        title = "", x = "Day") +
#   theme(axis.text=element_text(size=20),
#         axis.title=element_text(size=28))

# covid removed - poly 1
summary(fit <- lm(data = ugo, Unplanned.Generation.Outages~Date.Time.Hour.Beginning))
autoplot(ugo, Unplanned.Generation.Outages) +
  labs(y = "Unplanned Generation Outages",
       title = "", x = "Day") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=28)) +
  geom_abline(slope = fit$coefficients[2], intercept = fit$coefficients[1], colour = "#3D3C9F", linewidth = 2)


ugo$rownum = 1:nrow(ugo)
tsib2 = ugo %>%
  dplyr::mutate(detrended_UGO_no_covid = Unplanned.Generation.Outages - predict(fit))

Box.test(tsib2$detrended_UGO_no_covid, type = "Ljung-Box", fitdf = 0)

autoplot(tsib2, detrended_UGO_no_covid) +
  labs(y = "Detrended UGO",
       title = "", x = "Day") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=28))

# ACF
tsib2 |>
  ACF(detrended_UGO_no_covid,lag_max = 500) |>
  autoplot() +
  labs(y = "ACF",
       title = "", x = "Lag") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=26))+
  scale_x_continuous(breaks = seq(0, 500, by = 100))
# PACF
tsib2 |>
  PACF(detrended_UGO_no_covid,lag_max = 500) |>
  autoplot() +
  labs(y = "PACF",
       title = "", x = "Lag") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=26))+
  scale_x_continuous(breaks = seq(0, 500, by = 100))

# covid removed - poly 2
summary(fit <- lm(data = ugo, Unplanned.Generation.Outages~poly(Date.Time.Hour.Beginning,1)))
summary(fit2 <- lm(data = ugo, Unplanned.Generation.Outages~poly(Date.Time.Hour.Beginning,2)))
summary(fit3 <- lm(data = ugo, Unplanned.Generation.Outages~poly(Date.Time.Hour.Beginning,3)))
summary(fit4 <- lm(data = ugo, Unplanned.Generation.Outages~poly(Date.Time.Hour.Beginning,4)))
summary(fit5 <- lm(data = ugo, Unplanned.Generation.Outages~poly(Date.Time.Hour.Beginning,5)))
summary(fit6 <- lm(data = ugo, Unplanned.Generation.Outages~poly(Date.Time.Hour.Beginning,6)))
summary(fit7 <- lm(data = ugo, Unplanned.Generation.Outages~poly(Date.Time.Hour.Beginning,7)))
summary(fit8 <- lm(data = ugo, Unplanned.Generation.Outages~poly(Date.Time.Hour.Beginning,8)))
summary(fit9 <- lm(data = ugo, Unplanned.Generation.Outages~poly(Date.Time.Hour.Beginning,9)))
summary(fit10 <- lm(data = ugo, Unplanned.Generation.Outages~poly(Date.Time.Hour.Beginning,10)))
summary(fit11 <- lm(data = ugo, Unplanned.Generation.Outages~poly(Date.Time.Hour.Beginning,11)))
summary(fit12 <- lm(data = ugo, Unplanned.Generation.Outages~poly(Date.Time.Hour.Beginning,12)))
summary(fit13 <- lm(data = ugo, Unplanned.Generation.Outages~poly(Date.Time.Hour.Beginning,13)))
summary(fit14 <- lm(data = ugo, Unplanned.Generation.Outages~poly(Date.Time.Hour.Beginning,14)))
summary(fit15 <- lm(data = ugo, Unplanned.Generation.Outages~poly(Date.Time.Hour.Beginning,15)))
summary(fit16 <- lm(data = ugo, Unplanned.Generation.Outages~poly(Date.Time.Hour.Beginning,16)))
summary(fit17 <- lm(data = ugo, Unplanned.Generation.Outages~poly(Date.Time.Hour.Beginning,17)))
summary(fit18 <- lm(data = ugo, Unplanned.Generation.Outages~poly(Date.Time.Hour.Beginning,18)))
summary(fit19 <- lm(data = ugo, Unplanned.Generation.Outages~poly(Date.Time.Hour.Beginning,19)))
summary(fit20 <- lm(data = ugo, Unplanned.Generation.Outages~poly(Date.Time.Hour.Beginning,20)))
summary(fit21 <- lm(data = ugo, Unplanned.Generation.Outages~poly(Date.Time.Hour.Beginning,21)))
summary(fit22 <- lm(data = ugo, Unplanned.Generation.Outages~poly(Date.Time.Hour.Beginning,22)))
summary(fit50 <- lm(data = ugo, Unplanned.Generation.Outages~poly(Date.Time.Hour.Beginning,25)))
aic_ugo = AIC(fit, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9, fit10,
              fit11, fit12, fit13, fit14, fit15, fit16, fit17, fit18, fit19,
              fit20, fit21, fit22) # fit20 lowest AIC
bic_ugo = BIC(fit, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9, fit10,
              fit11, fit12, fit13, fit14, fit15, fit16, fit17, fit18, fit19,
              fit20, fit21, fit22) # fit20 lowest AIC
aicc_ugo = as.matrix(c(AICc(fit), AICc(fit2), AICc(fit3), AICc(fit4), AICc(fit5), AICc(fit6), AICc(fit7), AICc(fit8), AICc(fit9), AICc(fit10),
             AICc(fit11), AICc(fit12), AICc(fit13), AICc(fit14), AICc(fit15), AICc(fit16), AICc(fit17), AICc(fit18), AICc(fit19),
             AICc(fit20), AICc(fit21), AICc(fit22)),ncol = 1)


ugo$pred = predict(fit20)

autoplot(ugo, Unplanned.Generation.Outages) +
  labs(y = "Unplanned Generation Outages",
       title = "", x = "Day") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=28)) +
  geom_line(aes(Date.Time.Hour.Beginning, pred), colour = "#3D3C9F", linewidth = 2)


tsib2 = ugo %>%
  dplyr::mutate(detrended_UGO_no_covid = Unplanned.Generation.Outages - pred)

Box.test(tsib2$detrended_UGO_no_covid, type = "Ljung-Box", fitdf = 0)

autoplot(tsib2, detrended_UGO_no_covid) +
  labs(y = "Detrended UGO",
       title = "", x = "Day") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=28))

# ACF
tsib2 |>
  ACF(detrended_UGO_no_covid,lag_max = 500) |>
  autoplot() +
  labs(y = "ACF",
       title = "", x = "Lag") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=26))+
  scale_x_continuous(breaks = seq(0, 500, by = 100))

# PACF
tsib2 |>
  PACF(detrended_UGO_no_covid,lag_max = 500) |>
  autoplot() +
  labs(y = "PACF",
       title = "", x = "Lag") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=26))+
  scale_x_continuous(breaks = seq(0, 500, by = 100))

################################################################################
# Detrending PDD
################################################################################


# covid removed - poly 2
summary(fit   <- lm(data = pdd, Peak.Daily.Demand~poly(Day,1)))
summary(fit2  <- lm(data = pdd, Peak.Daily.Demand~poly(Day,2)))
summary(fit3  <- lm(data = pdd, Peak.Daily.Demand~poly(Day,3)))
summary(fit4  <- lm(data = pdd, Peak.Daily.Demand~poly(Day,4)))
summary(fit5  <- lm(data = pdd, Peak.Daily.Demand~poly(Day,5)))
summary(fit6  <- lm(data = pdd, Peak.Daily.Demand~poly(Day,6)))
summary(fit7  <- lm(data = pdd, Peak.Daily.Demand~poly(Day,7)))
summary(fit8  <- lm(data = pdd, Peak.Daily.Demand~poly(Day,8)))
summary(fit9  <- lm(data = pdd, Peak.Daily.Demand~poly(Day,9)))
summary(fit10 <- lm(data = pdd, Peak.Daily.Demand~poly(Day,10)))
summary(fit11 <- lm(data = pdd, Peak.Daily.Demand~poly(Day,11)))
summary(fit12 <- lm(data = pdd, Peak.Daily.Demand~poly(Day,12)))
summary(fit13 <- lm(data = pdd, Peak.Daily.Demand~poly(Day,13)))
summary(fit14 <- lm(data = pdd, Peak.Daily.Demand~poly(Day,14)))
summary(fit15 <- lm(data = pdd, Peak.Daily.Demand~poly(Day,15)))
summary(fit16 <- lm(data = pdd, Peak.Daily.Demand~poly(Day,16)))
summary(fit17 <- lm(data = pdd, Peak.Daily.Demand~poly(Day,17)))
summary(fit18 <- lm(data = pdd, Peak.Daily.Demand~poly(Day,18)))
summary(fit19 <- lm(data = pdd, Peak.Daily.Demand~poly(Day,19)))
summary(fit20 <- lm(data = pdd, Peak.Daily.Demand~poly(Day,20)))
summary(fit21 <- lm(data = pdd, Peak.Daily.Demand~poly(Day,21)))
summary(fit22 <- lm(data = pdd, Peak.Daily.Demand~poly(Day,22)))
summary(fit23 <- lm(data = pdd, Peak.Daily.Demand~poly(Day,23)))
summary(fit24 <- lm(data = pdd, Peak.Daily.Demand~poly(Day,24)))
summary(fit25 <- lm(data = pdd, Peak.Daily.Demand~poly(Day,25)))
summary(fit26 <- lm(data = pdd, Peak.Daily.Demand~poly(Day,26)))
library(AICcmodavg)
aic_pdd = AIC(fit, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9, fit10,
              fit11, fit12, fit13, fit14, fit15, fit16, fit17, fit18, fit19,
              fit20, fit21, fit22, fit23, fit24, fit25, fit26)

bic_pdd = BIC(fit, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9, fit10,
              fit11, fit12, fit13, fit14, fit15, fit16, fit17, fit18, fit19,
              fit20, fit21, fit22, fit23, fit24, fit25, fit26)

aicc_pdd = as.matrix(c(AICc(fit), AICc(fit2), AICc(fit3), AICc(fit4), AICc(fit5), AICc(fit6), AICc(fit7), AICc(fit8), AICc(fit9), AICc(fit10),
              AICc(fit11), AICc(fit12), AICc(fit13), AICc(fit14), AICc(fit15), AICc(fit16), AICc(fit17), AICc(fit18), AICc(fit19),
              AICc(fit20), AICc(fit21), AICc(fit22), AICc(fit23), AICc(fit24), AICc(fit25), AICc(fit26)),ncol = 1)

# Plot AICs and BICs
df_pdd = cbind(1:26, aic_pdd, bic_pdd)[,-c(2, 4)]
df_ugo = cbind(1:22, aic_ugo, bic_ugo)[,-c(2, 4)]
colnames(df_pdd) = colnames(df_ugo) = c("Order", "AIC", "BIC")
df_pdd = melt(df_pdd, id.vars = "Order")
df_ugo = melt(df_ugo, id.vars = "Order")
colnames(df_pdd) = colnames(df_ugo) = c("Order", "Criterion", "Value")

# PDD
ggplot(data = df_pdd, aes(x = Order, y = Value, colour = Criterion)) +
  geom_line(size = 3) +
  scale_color_manual(values = c('black', '#3D3C9F')) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=28))


# UGO
ggplot(data = df_ugo, aes(x = Order, y = Value, colour = Criterion)) +
  geom_line(size = 3) +
  scale_color_manual(values = c('black', '#3D3C9F')) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=28))



# 15 (plot), 26 (AIC) and 14

# Order 12
pred = predict(fit12)
autoplot(pdd, Peak.Daily.Demand) +
  labs(y = "Peak Daily Demand",
       title = "", x = "Day") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=28)) +
  geom_line(aes(Day, pred), colour = "#3D3C9F", linewidth = 2)

pdd2 = pdd %>%
  dplyr::mutate(detrended_PDD = Peak.Daily.Demand - pred)

Box.test(pdd2$detrended_PDD, type = "Ljung-Box", fitdf = 0)

autoplot(pdd2, detrended_PDD) +
  labs(y = "Detrended PDD",
       title = "", x = "Day") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=28))

# ACF
pdd2 |>
  ACF(detrended_PDD,lag_max = 500) |>
  autoplot() +
  labs(y = "ACF",
       title = "", x = "Lag") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=26))+
  scale_x_continuous(breaks = seq(0, 500, by = 100))

# PACF
pdd2 |>
  PACF(detrended_PDD,lag_max = 500) |>
  autoplot() +
  labs(y = "PACF",
       title = "", x = "Lag") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=26))+
  scale_x_continuous(breaks = seq(0, 500, by = 100))

# Order 26
pred = predict(fit26)
autoplot(pdd, Peak.Daily.Demand) +
  labs(y = "Peak Daily Demand",
       title = "", x = "Day") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=28)) +
  geom_line(aes(Day, pred), colour = "#3D3C9F", linewidth = 2)

pdd2 = pdd %>%
  dplyr::mutate(detrended_PDD = Peak.Daily.Demand - pred)

Box.test(pdd2$detrended_PDD, type = "Ljung-Box", fitdf = 0)

autoplot(pdd2, detrended_PDD) +
  labs(y = "Detrended PDD",
       title = "", x = "Day") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=28))

# ACF
pdd2 |>
  ACF(detrended_PDD,lag_max = 500) |>
  autoplot() +
  labs(y = "ACF",
       title = "", x = "Lag") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=26))+
  scale_x_continuous(breaks = seq(0, 500, by = 100))

# PACF
pdd2 |>
  PACF(detrended_PDD,lag_max = 500) |>
  autoplot() +
  labs(y = "PACF",
       title = "", x = "Lag") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=26))+
  scale_x_continuous(breaks = seq(0, 500, by = 100))

# Order 1
pred = predict(fit)
autoplot(pdd, Peak.Daily.Demand) +
  labs(y = "Peak Daily Demand",
       title = "", x = "Day") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=28)) +
  geom_line(aes(Day, pred), colour = "#3D3C9F", linewidth = 2)

pdd2 = pdd %>%
  dplyr::mutate(detrended_PDD = Peak.Daily.Demand - pred)

Box.test(pdd2$detrended_PDD, type = "Ljung-Box", fitdf = 0)

autoplot(pdd2, detrended_PDD) +
  labs(y = "Detrended PDD",
       title = "", x = "Day") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=28))

# ACF
pdd2 |>
  ACF(detrended_PDD,lag_max = 500) |>
  autoplot() +
  labs(y = "ACF",
       title = "", x = "Lag") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=26))+
  scale_x_continuous(breaks = seq(0, 500, by = 100))

# PACF
pdd2 |>
  PACF(detrended_PDD,lag_max = 500) |>
  autoplot() +
  labs(y = "PACF",
       title = "", x = "Lag") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=26))+
  scale_x_continuous(breaks = seq(0, 500, by = 100))

################################################################################
# Differencing
################################################################################
# first difference for PDD, second for UGO
pdd |>
  gg_tsdisplay(difference(Peak.Daily.Demand, 7) |> difference(),
               plot_type='partial', lag=36) +
  labs(title = "Double differenced", y="")

plot(pdd$Peak.Daily.Demand, type="l", main="No Differencing")
acf(pdd$Peak.Daily.Demand, lag.max = 100000)
pacf(pdd$Peak.Daily.Demand, lag.max = 100)

# 7 performs best. higher orders don't help much
pdd_box_df = as.data.frame(matrix(NA, nrow = 9, ncol = 4))
colnames(pdd_box_df) = c("Order", "Lag", "Statistic", "p-value")

lags = c(1, 7, 365)
for(i in 1:3){
  for(j in 1:3){
    pdd_lag = diff(pdd$Peak.Daily.Demand, differences = i, lag = lags[j])
    box = Box.test(pdd_lag, type = "Ljung-Box", fitdf = 0)
    pdd_box_df[j+3*(i-1),] = c(i, lags[j], box$statistic, box$p.value)
  }
}

print(xtable(pdd_box_df), type = "latex")

pdd_lag1 = diff(pdd$Peak.Daily.Demand, differences = 1, lag = 7)
# plot(pdd_lag1, type="l", main="First Difference")
acf(pdd_lag1, lag.max = 500)
pacf(pdd_lag1, lag.max = 500)

pdd |>
  PACF(pdd_lag_all,lag_max = 500) |>
  autoplot() +
  labs(y = "PACF",
       title = "", x = "Lag") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=26))+
  scale_x_continuous(breaks = seq(0, 500, by = 100))

pdd_lag2 = diff(pdd$Peak.Daily.Demand, differences = 2, lag = 7)
# plot(pdd_lag2, type="l", main="Second Difference")
acf(pdd_lag2, lag.max = 500)
pacf(pdd_lag2, lag.max = 500)

pdd_lag3 = diff(pdd$Peak.Daily.Demand, differences = 3, lag = 7)
# plot(pdd_lag3, type="l", main="Second Difference")
acf(pdd_lag3, lag.max = 500)
pacf(pdd_lag3, lag.max = 500)

pdd_lag_all = pdd$Peak.Daily.Demand |>
  diff(differences = 1, lag = 1) |>
  diff(differences = 1, lag = 7)|>
  diff(differences = 1, lag = 365)

Box.test(pdd_lag_all, type = "Ljung-Box", fitdf = 2)

acf(pdd_lag_all, lag.max = 15000)
pacf(pdd_lag_all, lag.max = 15000)


# UGO
# Ljung
ugo_box_df = as.data.frame(matrix(NA, nrow = 9, ncol = 4))
colnames(ugo_box_df) = c("Order", "Lag", "Statistic", "p-value")

lags = c(1, 24*365*0.75, 24*365)
for(i in 1:3){
  for(j in 1:3){
    ugo_lag = diff(ugo$Unplanned.Generation.Outages, differences = i, lag = lags[j])
    box = Box.test(ugo_lag, type = "Ljung-Box", fitdf = 0)
    ugo_box_df[j+3*(i-1),] = c(i, lags[j], box$statistic, box$p.value)
  }
}

print(xtable(ugo_box_df), type = "latex")

ugo_lag1 = diff(ugo$Unplanned.Generation.Outages, differences = 1, lag = 1)
# plot(ugo_lag1, type="l", main="First Difference")
acf(ugo_lag1, lag.max = 1000)
pacf(ugo_lag1, lag.max = 1000)

ugo |>
  ACF(ugo_lag3,lag_max = 500) |>
  autoplot() +
  labs(y = "ACF",
       title = "", x = "Lag") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=26))+
  scale_x_continuous(breaks = seq(0, 500, by = 100))

# Second is necessary
ugo_lag2 = diff(ugo$Unplanned.Generation.Outages, differences = 2, lag = 1)
# plot(ugo_lag2, type="l", main="Second Difference")
acf(ugo_lag2, lag.max = 1000)
pacf(ugo_lag2, lag.max = 1000)

# Third not needed
ugo_lag3 = diff(ugo$Unplanned.Generation.Outages, differences = 3, lag = 1)
# plot(ugo_lag3, type="l", main="Second Difference")
acf(ugo_lag3, lag.max = 1000)
pacf(ugo_lag3, lag.max = 1000)

ugo_lag_all = ugo$Unplanned.Generation.Outages |>
  diff(differences = 1, lag = 1) |>
  diff(differences = 1, lag = 24*365*0.75)|>
  diff(differences = 1, lag = 24*365)

Box.test(ugo_lag_all, type = "Ljung-Box", fitdf = 0)

acf(ugo_lag_all, lag.max = 15000)
pacf(ugo_lag_all, lag.max = 15000)

################################################################################
# Augmented Dickey Fuller and Phillips Peron Tests
################################################################################

tseries::adf.test(pdd$Peak.Daily.Demand, k = 0)
tseries::pp.test(pdd$Peak.Daily.Demand)

tseries::adf.test(diff(pdd$Peak.Daily.Demand, differences = 365), k = 0)
tseries::pp.test(diff(pdd$Peak.Daily.Demand, differences = 365))

tseries::adf.test(ugo$Unplanned.Generation.Outages, k = 0)
tseries::pp.test(ugo$Unplanned.Generation.Outages)

tseries::adf.test(diff(ugo$Unplanned.Generation.Outages, differences = 1), k = 0)
tseries::pp.test(diff(ugo$Unplanned.Generation.Outages, differences = 1))
?adf.test

################################################################################
# Scatterplots
################################################################################
lag1.plot(pdd$Peak.Daily.Demand, 10)
lag2.plot(pdd$Peak.Daily.Demand, ugo$Unplanned.Generation.Outages, 10)





