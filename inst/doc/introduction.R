## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  cache = TRUE,
  fig.width = 7,
  fig.height = 5,
  comment = "#>"
)

opt <- options(max.print = 70)

## ----setup, message=FALSE-----------------------------------------------------
library(dfms)
library(xts)

## -----------------------------------------------------------------------------
# Using the monthly series from BM14
dim(BM14_M)
range(index(BM14_M))
head(colnames(BM14_M))
plot(scale(BM14_M), lwd = 1)

## -----------------------------------------------------------------------------
head(BM14_Models, 3)

# Using only monthly data
BM14_Models_M <- subset(BM14_Models, freq == "M")

## -----------------------------------------------------------------------------
library(magrittr)
# log-transforming and first-differencing the data
BM14_M[, BM14_Models_M$log_trans] %<>% log()
BM14_M_diff <- diff(BM14_M)
plot(scale(BM14_M_diff), lwd = 1)

## -----------------------------------------------------------------------------
ic <- ICr(BM14_M_diff)
print(ic)
plot(ic)

## -----------------------------------------------------------------------------
screeplot(ic)

## -----------------------------------------------------------------------------
# Using vars::VARselect() with 4 principal components to estimate the VAR lag order
vars::VARselect(ic$F_pca[, 1:4])

## -----------------------------------------------------------------------------
# Estimating the model with 4 factors and 3 lags using BM14's EM algorithm
model_m <- DFM(BM14_M_diff, r = 4, p = 3)
print(model_m)
plot(model_m)

## -----------------------------------------------------------------------------
dfm_summary <- summary(model_m)
print(dfm_summary) # Large model with > 40 series: defaults to compact = 2

# Can request more detailed printouts
# print(dfm_summary, compact = 1)
# print(dfm_summary, compact = 0) 

## -----------------------------------------------------------------------------
plot(resid(model_m, orig.format = TRUE), lwd = 1)
plot(fitted(model_m, orig.format = TRUE), lwd = 1)

## -----------------------------------------------------------------------------
plot(model_m, method = "all", type = "individual")

## -----------------------------------------------------------------------------
# Default: all estimates in long format
head(as.data.frame(model_m, time = index(BM14_M_diff)))

## -----------------------------------------------------------------------------
# 12-period ahead DFM forecast
fc <- predict(model_m, h = 12)
print(fc)

## -----------------------------------------------------------------------------
# Setting an appropriate plot range to see the forecast
plot(fc, xlim = c(320, 370))

## -----------------------------------------------------------------------------
# Factor forecasts in wide format
head(as.data.frame(fc, pivot = "wide"))

## -----------------------------------------------------------------------------
# Quarterly series from BM14
head(BM14_Q, 3)
# Pre-processing the data
BM14_Q[, BM14_Models$log_trans[BM14_Models$freq == "Q"]] %<>% log()
BM14_Q_diff <- diff(BM14_Q)
# Merging to monthly data
BM14_diff <- merge(BM14_M_diff, BM14_Q_diff)

# Estimating the model with 5 factors and 3 lags using BM14's EM algorithm
model_mq <- DFM(BM14_diff, r = 5, p = 3, quarterly.vars = colnames(BM14_Q))
print(model_mq)
plot(model_mq)

## ----include=FALSE------------------------------------------------------------
options(opt)

