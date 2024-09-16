# Sian Wood
# 20/08/2024
# Time Series Assignment 1
# Exploratory Data Analysis

# List of necessary packages
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

# Read in Data
data = read.csv("ESK6816.csv")
class(data)
names(data)

# Testing as.POSIXct
# datetime = "2019/04/01 12:00"
# as.POSIXct(datetime,format="%Y/%m/%d %H:%M",tz=Sys.timezone())

# Convert dates from char to POSIXct
data = dplyr::mutate(data,
                     Date.Time.Hour.Beginning = as.POSIXct(Date.Time.Hour.Beginning,
                                                           format="%Y/%m/%d %H:%M",
                                                           tz=Sys.timezone()))

# Create tsibble (time series tibble)
tib = tibble(data)
tsib <- as_tsibble(tib, index = Date.Time.Hour.Beginning)
################################################################################
# Clean Data
################################################################################

# 4800 NA's in all but Date and RSA Contracted Forecast
which(is.na(tsib$RSA.Contracted.Demand))
tsib[which(is.na(tsib$RSA.Contracted.Demand)),]
# All the same rows are NA's
# NA rmved:
tsib_na_rmv = na.omit(tsib)

# any repeated dates?
nrow(tsib)
length(unique(tsib$Date.Time.Hour.Beginning))

################################################################################
# Create Variables of Interest
################################################################################

#' Mutate to add Peak.Daily.Demand
#' RSA Contracted Demand is the hourly average demand that needs to be supplied
#' by all resources that Eskom has contracts with.

# day not date
tsib_days = tsib_na_rmv
tsib_days$Date.Time.Hour.Beginning = as.Date(tsib_na_rmv$Date.Time.Hour.Beginning, format = "%Y-%m-%d",tz=Sys.timezone())

# sum across days
Daily.Demand = tsib_days %>%
  dplyr::group_by(Date.Time.Hour.Beginning) %>%
  dplyr::summarise(Total.Daily.Demand = sum(RSA.Contracted.Demand),
                   Peak.Daily.Demand = max(RSA.Contracted.Demand),
                   Mean.Daily.Demand = mean(RSA.Contracted.Demand),
                   Med.Daily.Demand = median(RSA.Contracted.Demand),
                   Min.Daily.Demand = min(RSA.Contracted.Demand))
colnames(Daily.Demand)[1] = "Day"

# Create tsibble (time series tibble)
Daily.Demand = tibble(Daily.Demand)
Daily.Demand <- as_tsibble(Daily.Demand, index = Day)

# add to tsib_na_rmv in case we ever need it there
tsib_na_rmv$Peak.Daily.Demand = rep(Daily.Demand$Peak.Daily.Demand, each = 24)

#' Mutate to add Unplanned.Generation.Outages
#' UCLF + OCLF
tsib_na_rmv$Unplanned.Generation.Outages = tsib_na_rmv$Total.UCLF + tsib_na_rmv$Total.OCLF

saveRDS(tsib_na_rmv, "tsib.RDS")


# Remove March to June 2020:
# March 2020
datetime = "2020/03/01 00:00"
march2020 = as.POSIXct(datetime,format="%Y/%m/%d %H:%M",tz=Sys.timezone())
# June 2020
datetime = "2020/06/01 00:00"
june2020 = as.POSIXct(datetime,format="%Y/%m/%d %H:%M",tz=Sys.timezone())

covid_rows1 = intersect(which(tsib_na_rmv$Date.Time.Hour.Beginning>march2020),
                   which(tsib_na_rmv$Date.Time.Hour.Beginning<june2020))


# Identify outliers:
autoplot(tsib_na_rmv[covid_rows1,], Unplanned.Generation.Outages) +
  geom_vline(xintercept = as.POSIXct("2020/04/30 00:00",format="%Y/%m/%d %H:%M",tz=Sys.timezone()), color = "#3D3C9F", lwd = 1.5)  +
  geom_vline(xintercept = as.POSIXct("2020/03/27 00:00",format="%Y/%m/%d %H:%M",tz=Sys.timezone()), color = "#3D3C9F", lwd = 1.5) +
  labs(y = "Unplanned Generation Outages",
       title = "", x = "Date") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=26))

covid_rows2 = intersect(which(Daily.Demand$Day>march2020),
                       which(Daily.Demand$Day<june2020))

autoplot(Daily.Demand[covid_rows2,], Peak.Daily.Demand)  +
  geom_vline(xintercept = Daily.Demand[covid_rows2[26],]$Day, color = "#3D3C9F", lwd = 1.5) +
  geom_vline(xintercept = Daily.Demand[covid_rows2[60],]$Day, color = "#3D3C9F", lwd = 1.5) +
  labs(y = "Peak Daily Demand",
       title = "", x = "Date") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=26))

# Truncate data
datetime = "2020/05/01 00:00"
may2020 = as.POSIXct(datetime,format="%Y/%m/%d %H:%M",tz=Sys.timezone())

datetime = "2023/08/13 23:00"
aug13.2023 = as.POSIXct(datetime,format="%Y/%m/%d %H:%M",tz=Sys.timezone())

#####

covid_rows3 = which(tsib_na_rmv$Date.Time.Hour.Beginning<may2020)

ugo       = tsib_na_rmv[-covid_rows3,]
test_rows = which(ugo$Date.Time.Hour.Beginning>aug13.2023)
ugo_test  = ugo[test_rows,]
ugo_train = ugo[-test_rows,]

autoplot(ugo_train, Unplanned.Generation.Outages)
saveRDS(ugo_test, "ugo_test.RDS")
saveRDS(ugo_train, "ugo_train.RDS")

#####

covid_rows_daily = which(Daily.Demand$Day<may2020)

pdd       = Daily.Demand[-covid_rows_daily,]
test_rows = which(pdd$Day>aug13.2023)
pdd_test  = pdd[test_rows,]
pdd_train = pdd[-test_rows,]

autoplot(pdd_train, Peak.Daily.Demand)
saveRDS(pdd_test, "pdd_test.RDS")
saveRDS(pdd_train, "pdd_train.RDS")
