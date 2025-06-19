# ==================================================================================================================================================================
# ==================================================================================================================================================================

# PROJECT TITLE:  PRAGMA
# CODE AUTHOR:    JM
# DATE STARTED:   2025/01/09

# ==================================================================================================================================================================
# ==================================================================================================================================================================

# clean workspace
rm(list=ls())

# date
DATE <- Sys.Date()

# load libraries
library( data.table )
library( ggplot2 )
library( ggthemes )
library( ggpubr ) ## for ggqqplot
library( mgcv ) ## for gam/gamm
library( lmtest ) ## for dwtest
library( forecast ) ## for auto.arima

# themes and options
theme_set( theme_gdocs() )
options(scipen = 999)


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 1) LOAD DATA
# ______________________________________________________________________________________________________________________

##  prepared data
filename <- paste0("data/output/", DATE, "_summary data.RDS")
sum.dat <- readRDS(filename)
filename <- paste0("data/output/", DATE, "_main data.RDS")
data <- readRDS(filename)
#filename <- paste0("data/output/", DATE, "_sensitivity data.RDS")
#data_sens <- readRDS(filename)
#filename <- paste0("data/output/", DATE, "_weekly count_intervention.RDS")
#data.int <- readRDS(filename)


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 2) CHECK DATA
# ______________________________________________________________________________________________________________________

##  1) Primary/secondary_outp outcome
# ..............

data[, summary(primary)]
data[, summary(secondary_outp)]
data[, summary(secondary_inp)]

##  2) Distributions
# ..............

##  hist
hist(data$primary) ## looks not great but ok

##  QQplots
ggqqplot(data$primary) ## looks not great but ok

##  scatterplot
ggplot(data, aes(x = time)) + 
  geom_point(aes(y = primary), color = "red") +
  geom_point(aes(y = secondary_outp), color = "blue") +
  geom_point(aes(y = secondary_inp), color = "green")

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 3) ITS - PRIMARY
# ______________________________________________________________________________________________________________________

##  1) GLM
# ..............

##  baseline model : simple GLM
pri.mod.0 <- glm(primary ~ time + lockdown1 + time_since_lockdown1 +
                    time_since_between_lockdowns + 
                    lockdown2 + time_since_lockdown2 +
                    time_since_after_lockdowns, data = data)
summary(pri.mod.0)
data$primary_pred.0 <- predict(pri.mod.0)

##  scatterplot with predictions
ggplot(data, aes(x = time, y = primary)) + 
  geom_point() + 
  geom_line(aes(y = primary_pred.0))

## Check for autocorrelation and stationarity
res <- residuals(pri.mod.0,type="deviance")
acf(res) # has autocorrelation at AR1 and 2 and also indications of seasonality
pacf(res) # looks ok
tseries::adf.test(res) # stationarity: -7; p<.01 = ok


##  2) GAM
# ..............

pri.mod.1 <- gamm(primary ~ time + lockdown1 + time_since_lockdown1 +
                    time_since_between_lockdowns + 
                    lockdown2 + time_since_lockdown2 +
                    time_since_after_lockdowns + 
                    s(week, bs = "cc", k=20), 
                  data = data, method = "REML")
summary(pri.mod.1$gam)
data$primary_pred.1 <- predict(pri.mod.1$gam)

##  scatterplot with predictions
ggplot(data, aes(x = time, y = primary)) + 
  geom_point() + 
  geom_line(aes(y = primary_pred.0), color = "blue") +
  geom_line(aes(y = primary_pred.1), color = "red")
  
## Check for autocorrelation and stationarity
res <- resid(pri.mod.1$lme, type = "normalized")
acf(res) # has autocorrelation at AR1 and 4-6
pacf(res) # looks mostly ok
tseries::adf.test(res) # stationarity: -5; p<.01 = ok

##  identity autocorrelation (AR/MA terms) using AUTO.ARIMA
auto.arima(resid(pri.mod.1$lme, type = "normalized"),
           stationary = TRUE, seasonal = FALSE) # MA1

##  3) GAMM
# ..............

## seasonality + MA1
pri.mod.2 <- gamm(primary ~ time + lockdown1 + time_since_lockdown1 +
                    time_since_between_lockdowns + 
                    lockdown2 + time_since_lockdown2 +
                    time_since_after_lockdowns + 
                    s(week, bs = "cc", k=15),
                  correlation = corARMA(form = ~ 1|year, q = 1),
                  data = data, method = "REML")
summary(pri.mod.2$gam)
data$primary_pred.2 <- predict(pri.mod.2$gam)

##  scatterplot with predictions
ggplot(data, aes(x = time, y = primary)) + 
  geom_point() + 
  geom_line(aes(y = primary_pred.0), color = "blue") +
  geom_line(aes(y = primary_pred.1), color = "red") +
  geom_line(aes(y = primary_pred.2), color = "green")

## Check for autocorrelation and stationarity
res <- resid(pri.mod.2$lme, type = "normalized")
acf(res) # looks ok
pacf(res) # looks mostly ok
tseries::adf.test(res) # stationarity: -5; p<.01 = ok

##  4) EFFECT SIZES
# ..............

dat.pri <- copy(data[,.(period,primary,time,lockdown1 = 0,time_since_lockdown1 = 0,
                        time_since_lockdown1,time_since_between_lockdowns,lockdown2,time_since_lockdown2,time_since_after_lockdowns,
                        week)])

dat.pri$pred <- predict(pri.mod.2$gam)
dat.pri$pred_counter <- predict(pri.mod.2$gam, newdata = dat.pri)
dat.pri[period == 2,.(time,primary,pred,pred_counter)]
dat.pri[period == 2,.(time,primary)][,summary(primary)]
dat.pri[period == 2,.(time,pred_counter)][,summary(pred_counter)]

obs <- dat.pri[period == 2]$primary
alt <- dat.pri[period == 2]$pred_counter

summary(-(1-obs/alt)) # median: -27.3%
rm(dat.pri, obs, alt)


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 4) ITS - SECONDARY OUTPATIENT
# ______________________________________________________________________________________________________________________

##  1) BASELINE MODEL
# ..............

##  baseline model : simple GLM
sec1.mod.0 <- glm(secondary_outp ~ time + lockdown1 + time_since_lockdown1 +
                   time_since_between_lockdowns + 
                   lockdown2 + time_since_lockdown2 +
                   time_since_after_lockdowns, data = data)
summary(sec1.mod.0)
data$secondary_outp_pred.0 <- predict(sec1.mod.0)

##  scatterplot with predictions
ggplot(data, aes(x = time, y = secondary_outp)) + 
  geom_point() + 
  geom_line(aes(y = secondary_outp_pred.0))

## Check for autocorrelation and stationarity
res <- residuals(sec1.mod.0,type="deviance")
acf(res) # has autocorrelation at AR1 and 2 and also indications of seasonality
pacf(res) # looks ok
tseries::adf.test(res) # stationarity: -8; p<.01 = ok


##  2) GAM MODEL
# ..............

sec1.mod.1 <- gamm(secondary_outp ~ time + lockdown1 + time_since_lockdown1 +
                    time_since_between_lockdowns + 
                    lockdown2 + time_since_lockdown2 +
                    time_since_after_lockdowns + 
                    s(week, bs = "cc", k=20), 
                   data = data, method = "REML")
summary(sec1.mod.1$gam)
data$secondary_outp_pred.1 <- predict(sec1.mod.1$gam)

##  scatterplot with predictions
ggplot(data, aes(x = time, y = secondary_outp)) + 
  geom_point() + 
  geom_line(aes(y = secondary_outp_pred.0), color = "blue") +
  geom_line(aes(y = secondary_outp_pred.1), color = "red")

## Check for autocorrelation and stationarity
res <- resid(sec1.mod.1$lme, type = "normalized")
acf(res) # looks ok
pacf(res) # looks ok
tseries::adf.test(res) # stationarity: -5; p<.01 = ok

##  identity autocorrelation (AR/MA terms) using AUTO.ARIMA
auto.arima(resid(sec1.mod.1$lme, type = "normalized"),
           stationary = TRUE, seasonal = FALSE) # none --> no GAMM

##  4) EFFECT SIZES
# ..............

dat.sec1 <- copy(data[,.(period,secondary_outp,time,lockdown1 = 0,time_since_lockdown1 = 0,
                        time_since_lockdown1,time_since_between_lockdowns,lockdown2,time_since_lockdown2,time_since_after_lockdowns,
                        week)])

dat.sec1$pred <- predict(sec1.mod.1$gam)
dat.sec1$pred_counter <- predict(sec1.mod.1$gam, newdata = dat.sec1)
dat.sec1[period == 2,.(time,secondary_outp,pred,pred_counter)]
dat.sec1[period == 2,.(time,secondary_outp)][,summary(secondary_outp)]
dat.sec1[period == 2,.(time,pred_counter)][,summary(pred_counter)]

obs <- dat.sec1[period == 2]$secondary_outp
alt <- dat.sec1[period == 2]$pred_counter

summary(-(1-obs/alt)) # median: -14.6%
rm(dat.sec1, obs, alt)

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 5) ITS - SECONDARY INPATIENT
# ______________________________________________________________________________________________________________________

##  1) BASELINE MODEL
# ..............

##  baseline model : simple GLM
sec2.mod.0 <- glm(secondary_inp ~ time + lockdown1 + time_since_lockdown1 +
                    time_since_between_lockdowns + 
                    lockdown2 + time_since_lockdown2 +
                    time_since_after_lockdowns, data = data)
summary(sec2.mod.0)
data$secondary_inp_pred.0 <- predict(sec2.mod.0)

##  scatterplot with predictions
ggplot(data, aes(x = time, y = secondary_inp)) + 
  geom_point() + 
  geom_line(aes(y = secondary_inp_pred.0))

## Check for autocorrelation and stationarity
res <- residuals(sec2.mod.0,type="deviance")
acf(res) # has strong autocorrelation and also indications of seasonality
pacf(res) # looks ok
tseries::adf.test(res) # stationarity: -5; p<.01 = ok


##  2) GAM MODEL
# ..............

sec2.mod.1 <- gamm(secondary_inp ~ time + lockdown1 + time_since_lockdown1 +
                     time_since_between_lockdowns + 
                     lockdown2 + time_since_lockdown2 +
                     time_since_after_lockdowns + 
                     s(week, bs = "cc", k=20), data = data, method = "REML")
summary(sec2.mod.1$gam)
data$secondary_inp_pred.1 <- predict(sec2.mod.1$gam)

##  scatterplot with predictions
ggplot(data, aes(x = time, y = secondary_inp)) + 
  geom_point() + 
  geom_line(aes(y = secondary_inp_pred.0), color = "blue") +
  geom_line(aes(y = secondary_inp_pred.1), color = "red")

## Check for autocorrelation and stationarity
res <- resid(sec2.mod.1$lme, type = "normalized")
acf(res) # strong autocorrelation
pacf(res) # looks ok
tseries::adf.test(res) # stationarity: -4; p<.01 = ok

##  identity autocorrelation (AR/MA terms) using AUTO.ARIMA
auto.arima(resid(sec2.mod.1$lme, type = "normalized"),
           stationary = TRUE, seasonal = FALSE) # AR1 and MA2

##  3) GAMM MODEL
# ..............

## GAMM: seasonality
sec2.mod.2 <- gamm(secondary_inp ~ time + lockdown1 + time_since_lockdown1 +
                     time_since_between_lockdowns + 
                     lockdown2 + time_since_lockdown2 +
                     time_since_after_lockdowns + 
                     s(week, bs = "cc", k=15),
                   correlation = corARMA(form = ~ 1|year, p = 1, q = 2),
                   data = data, method = "REML")
summary(sec2.mod.2$gam)
data$secondary_inp_pred.2 <- predict(sec2.mod.2$gam)

##  scatterplot with predictions
ggplot(data, aes(x = time, y = secondary_inp)) + 
  geom_point() + 
  geom_line(aes(y = secondary_inp_pred.0), color = "blue") +
  geom_line(aes(y = secondary_inp_pred.1), color = "red") +
  geom_line(aes(y = secondary_inp_pred.2), color = "green")

## Check for autocorrelation and stationarity
res <- resid(sec2.mod.2$lme, type = "normalized")
acf(res) # looks ok
pacf(res) # looks ok
tseries::adf.test(res) # stationarity: -8; p<.01 = ok

##  4) EFFECT SIZES
# ..............

dat.sec2 <- copy(data[,.(period,secondary_inp,time,lockdown1 = 0,time_since_lockdown1 = 0,
                         time_since_lockdown1,time_since_between_lockdowns,lockdown2,time_since_lockdown2,time_since_after_lockdowns,
                         week)])

dat.sec2$pred <- predict(sec2.mod.2$gam)
dat.sec2$pred_counter <- predict(sec2.mod.2$gam, newdata = dat.sec2)
dat.sec2[period == 2,.(time,secondary_inp,pred,pred_counter)]
dat.sec2[period == 2,.(time,secondary_inp)][,summary(secondary_inp)]
dat.sec2[period == 2,.(time,pred_counter)][,summary(pred_counter)]

obs <- dat.sec2[period == 2]$secondary_inp
alt <- dat.sec2[period == 2]$pred_counter

summary(-(1-obs/alt)) # median: -45.2%
rm(dat.sec2, obs, alt)


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 6) TABLES
# ______________________________________________________________________________________________________________________

##  1) TAB 2
# ..............

sjPlot::tab_model(pri.mod.2,
                  sec1.mod.1,
                  sec2.mod.2,
                  show.ci = 0.95,show.p = T,
                  dv.labels = c("primary","secondary: outpatient","secondary: inpatient"),
                  file = paste0("tables/Table2_model summary_",Sys.Date(),".html"))



# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 7) EXPORT
# ______________________________________________________________________________________________________________________

saveRDS(pri.mod.2, file = paste0("data/output/",DATE,"_primary outcome_model_main.rds"))
saveRDS(sec1.mod.1, file = paste0("data/output/",DATE,"_secondary outcome1_model_main.rds"))
saveRDS(sec2.mod.2, file = paste0("data/output/",DATE,"_secondary outcome2_model_main.rds"))

saveRDS(data, paste0("data/output/", DATE, "_main data with predictions.RDS"))

####################
#perplexity:

# Simulate sample data
set.seed(123)
n <- 90
lockdown_start <- 31
lockdown_end <- 60

time <- 1:n

# Define periods
period <- cut(time,
              breaks = c(0, lockdown_start-1, lockdown_end, n),
              labels = c("pre", "lockdown", "post"))

# Simulate outcome with different levels/slopes
y <- 10 + 0.2*time + 
  ifelse(time >= lockdown_start & time <= lockdown_end, 5, 0) +           # level jump in lockdown
  ifelse(time > lockdown_end, -3, 0) +                                   # level drop post-lockdown
  ifelse(time > lockdown_end, 0.5*(time - lockdown_end), 0) +            # slope change post-lockdown
  rnorm(n, 0, 1.5)

dat <- data.frame(time, period, y)

# --- Model 1: Bernal-style (dummy + interaction for each period) ---

dat$lockdown <- ifelse(dat$period == "lockdown", 1, 0)
dat$post <- ifelse(dat$period == "post", 1, 0)
dat$time_lockdown <- dat$time * dat$lockdown
dat$time_post <- dat$time * dat$post

fit0 <- lm(y ~ time + lockdown + time*lockdown + post + time*post, data = dat)
summary(fit0)

fit1 <- lm(y ~ time + lockdown + time_lockdown + post + time_post, data = dat)
summary(fit1)

# --- Model 2: "Time since intervention" approach ---

dat$time_since_lockdown <- ifelse(dat$time >= lockdown_start, dat$time - lockdown_start + 1, 0)
dat$time_since_post <- ifelse(dat$time > lockdown_end, dat$time - lockdown_end, 0)

fit2 <- lm(y ~ time + lockdown + time_since_lockdown + post + time_since_post, data = dat)
summary(fit2)

# --- Compare fitted values visually ---
plot(dat$time, dat$y, pch=16, col="grey", main="ITS: Model Comparison", xlab="Time", ylab="Outcome")
lines(dat$time, fitted(fit1), col="blue", lwd=2)
lines(dat$time, fitted(fit2), col="red", lwd=2, lty=2)
legend("topleft", legend=c("Bernal (blue)", "Time-since (red dashed)"), lty=1:2, col=c("blue", "red"))
