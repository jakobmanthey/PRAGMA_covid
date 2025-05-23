# ==================================================================================================================================================================
# ==================================================================================================================================================================

# PROJECT TITLE:  PRAGMA
# CODE AUTHOR:    JM
# DATE STARTED:   2025/01/09

# ==================================================================================================================================================================
# ==================================================================================================================================================================

# clean workspace
rm(list=ls())

# input path
inpath <- paste0("data/output/preprocessed/")

# output path
#outpath <- paste0("/Users/carolinkilian/Desktop/PRAGMA/Output/Administrative Prävalenz/")

# date
#DATE <- "2024-08-19"
DATE2 <- "2025-05-23"
#DATE2 <- Sys.Date()

# load libraries
library( data.table )
library( ggplot2 )
library( ggthemes )
library( ggpubr ) ## for ggqqplot
library( mgcv ) ## for gam/gamm
library( lmtest ) ## for dwtest
library( forecast ) ## for auto.arima
#library( tidyr )
#library( stringr )
#library( openxlsx )
#library( lme4 )
#library( gee )

# themes and options
theme_set( theme_gdocs() )
options(scipen = 999)
#blue_shades_5 <- colorRampPalette(c("lightblue", "darkblue"))(5)
#blue_shades_6 <- colorRampPalette(c("lightblue", "darkblue"))(6)
#green_shades_5 <- colorRampPalette(c("#DAF2D0", "#12501A"))(5)
#green_shades_6 <- colorRampPalette(c("#DAF2D0", "#12501A"))(6)
#three_colors <- c("#FFB3BA", "#B3FFB3", "#B3D9FF")
#three_colors <- c("#FF7F0E", "#2CA02C", "#1F77B4")
#five_colors <- c("#FF7F0E", "#2CA02C", "#1F77B4", "#9467BD", "#8C564B")

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 1) LOAD DATA AND ADD PANDEMIC PERIODS
# ______________________________________________________________________________________________________________________


##  prepared data
filename <- paste0("data/output/", DATE2, "_summary data.RDS")
sum.dat <- readRDS(filename)
filename <- paste0("data/output/", DATE2, "_main data.RDS")
data <- readRDS(filename)
filename <- paste0("data/output/", DATE2, "_sensitivity data.RDS")
data_sens <- readRDS(filename)
filename <- paste0("data/output/", DATE2, "_weekly count_intervention.RDS")
data.int <- readRDS(filename)
filename <- paste0("data/output/", DATE2, "_period and stringency data.RDS")
period.dat <- readRDS(filename)

##  dates:
date.period2 <- data[period == 2, min(date.start)]
date.period3 <- data[period == 3, min(date.start)]
date.period4 <- data[period == 4, min(date.start)]
date.period5 <- data[period == 5, min(date.start)]

##  ts data
ts_dat1 <- copy(data[date.start < date.period2])
ts_dat1$con_week_sq <- ts_dat1$con_week^2

ts_dat2 <- copy(data)
ts_dat2$con_week_sq <- ts_dat2$con_week^2

  
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 2) REPORT DATA
# ______________________________________________________________________________________________________________________

##  1) SAMPLE DESCRIPTION
# ..............

nrow(sum.dat)
sum.dat[, mean(sex == "female")]
sum.dat[, mean(sex == "male")]
sum.dat[, mean(2020-yob)]
sum.dat[, summary(2020-yob)]

# by intervention


##  2) Primary/Secondary outcome
# ..............

data[, summary(primary)]
data[, summary(secondary_outp)]
data[, summary(secondary_inp)]

##  3) Distributions
# ..............

##  QQplots
ggqqplot(data$primary) ## looks not great but ok

##  scatterplot
ggplot(data, aes(x = con_week, y = primary)) + 
  geom_point()

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 3) ITS - PRIMARY
# ______________________________________________________________________________________________________________________

##  1) BASELINE - PREPANDEMIC
# ..............

##  baseline model a) linear trend and 10 knots
pri.mod.0a <- gamm(primary ~ con_week + s(week, bs = "cc", k=10), data = ts_dat1, method = "REML")
ts_dat1$pred_0a <- predict(pri.mod.0a$gam)

##  baseline model b) linear trend and 20 knots
pri.mod.0b <- gamm(primary ~ con_week + s(week, bs = "cc", k=20), data = ts_dat1, method = "REML")
ts_dat1$pred_0b <- predict(pri.mod.0b$gam)

##  baseline model c) linear and quadratic trend and 10 knots
pri.mod.0c <- gamm(primary ~ con_week + con_week_sq + s(week, bs = "cc", k=10), data = ts_dat1, method = "REML")
ts_dat1$pred_0c <- predict(pri.mod.0c$gam)

##  baseline model d) linear and quadratic trend and 20 knots
pri.mod.0d <- gamm(primary ~ con_week + con_week_sq + s(week, bs = "cc", k=20), data = ts_dat1, method = "REML")
ts_dat1$pred_0d <- predict(pri.mod.0d$gam)

##  COMPARE
anova(pri.mod.0a$lme, pri.mod.0b$lme) ## no test but b better (more knots)
anova(pri.mod.0c$lme, pri.mod.0d$lme) ## no test but d better (more knots)
anova(pri.mod.0a$lme, pri.mod.0c$lme) ## Model c = superior (cubic)
anova(pri.mod.0b$lme, pri.mod.0d$lme) ## Model d = superior (cubic)


##  2) FULL - correcting for autocorrelation
# ..............

##  baseline model d) linear and quadratic trend and 20 knots
pri.mod.1 <- gamm(primary ~ con_week + con_week_sq + s(week, bs = "cc", k=20), data = ts_dat2, method = "REML")
summary(pri.mod.1$gam)
plot(pri.mod.1$gam) ## show seasonal effect
dwtest(pri.mod.1$gam) ## autocorrelation = 1.1

##  remove autocorrelation:
# let AUTO.ARIMA check for AR/MA terms:
auto.arima(resid(pri.mod.1$lme, type = "normalized"),
           stationary = TRUE, seasonal = FALSE) # AR1

## seasonality + AR1
pri.mod.2 <- gamm(primary ~ con_week + con_week_sq + s(week, bs = "cc", k=20),
              correlation = corARMA(form = ~ 1|year, p = 1),
              data = ts_dat2, method = "REML")
summary(pri.mod.2$gam)
plot(pri.mod.2$gam)
dwtest(pri.mod.2$gam) ## autocorrelation = 0.8

##  show ACF & PACF:
layout(matrix(1:2, ncol = 2))
acf(resid(pri.mod.2$lme, type="normalized"), lag.max = 24, main = "ACF")
pacf(resid(pri.mod.2$lme, type="normalized"), lag.max = 24, main = "pACF")
layout(1)

#   compare models:
anova(pri.mod.1$lme, pri.mod.2$lme) ## Model 2 = superior


##  3) FULL - WITH LEVELS AND SLOPES
# ..............

## seasonality + AR1 + level/slope effects
pri.mod.3 <- gamm(primary ~ con_week + con_week_sq + s(week, bs = "cc", k=20) +
                p2.level + p2.slope + p3.level + p3.slope + 
                p4.level + p4.slope + p5.level + p5.slope, 
              correlation = corARMA(form = ~ 1|year, p = 1),
              data = ts_dat2, method = "REML")
summary(pri.mod.3$gam)
plot(pri.mod.3$gam)
dwtest(pri.mod.3$gam) ## autocorrelation = 1.1

##  save ACF & PACF:
png(filename = paste0("figs/", Sys.Date(), "_plot_acf_pacf_model_primary.png"), width = 700, height = 600)
par(mfrow = c(2, 1))
acf(resid(pri.mod.3$lme, type="normalized"), lag.max = 24, main = "ACF")
pacf(resid(pri.mod.3$lme, type="normalized"), lag.max = 24, main = "pACF")
dev.off()
ggqqplot(resid(pri.mod.3$lme, type="normalized")) ## residuals look ok

##  Predict:
ts_dat2$primary_pred <- predict(pri.mod.3$gam)

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 4) ITS - SECONDARY - OUTPATIENT
# ______________________________________________________________________________________________________________________

##  1) BASELINE - PREPANDEMIC
# ..............

##  baseline model a) linear trend and 10 knots
sec1.mod.0a <- gamm(secondary_outp ~ con_week + s(week, bs = "cc", k=10), data = ts_dat1, method = "REML")
ts_dat1$pred_0a <- predict(sec1.mod.0a$gam)

##  baseline model b) linear trend and 20 knots
sec1.mod.0b <- gamm(secondary_outp ~ con_week + s(week, bs = "cc", k=20), data = ts_dat1, method = "REML")
ts_dat1$pred_0b <- predict(sec1.mod.0b$gam)

##  baseline model c) linear and quadratic trend and 10 knots
sec1.mod.0c <- gamm(secondary_outp ~ con_week + con_week_sq + s(week, bs = "cc", k=10), data = ts_dat1, method = "REML")
ts_dat1$pred_0c <- predict(sec1.mod.0c$gam)

##  baseline model d) linear and quadratic trend and 20 knots
sec1.mod.0d <- gamm(secondary_outp ~ con_week + con_week_sq + s(week, bs = "cc", k=20), data = ts_dat1, method = "REML")
ts_dat1$pred_0d <- predict(sec1.mod.0d$gam)

##  COMPARE
anova(sec1.mod.0a$lme, sec1.mod.0b$lme) ## no test but b better (more knots)
anova(sec1.mod.0c$lme, sec1.mod.0d$lme) ## no test but d better (more knots)
anova(sec1.mod.0a$lme, sec1.mod.0c$lme) ## Model a = superior (no cubic)
anova(sec1.mod.0b$lme, sec1.mod.0d$lme) ## Model b = superior (no cubic)


##  2) FULL - correcting for autocorrelation
# ..............

##  baseline model b) linear trend and 20 knots
sec1.mod.1 <- gamm(secondary_outp ~ con_week + s(week, bs = "cc", k=20), data = ts_dat2, method = "REML")
summary(sec1.mod.1$gam)
plot(sec1.mod.1$gam) ## show seasonal effect
dwtest(sec1.mod.1$gam) ## autocorrelation = 0.9

##  remove autocorrelation:
# let AUTO.ARIMA check for AR/MA terms:
auto.arima(resid(sec1.mod.1$lme, type = "normalized"),
           stationary = TRUE, seasonal = FALSE) # AR4, MA3 -> too much, start with AR1 & MA1

## seasonality + AR1 + MA4
sec1.mod.2 <- gamm(secondary_outp ~ con_week + s(week, bs = "cc", k=20),
              correlation = corARMA(form = ~ 1|year, p = 1, q = 1),
              data = ts_dat2, method = "REML")
summary(sec1.mod.2$gam)
plot(sec1.mod.2$gam)
dwtest(sec1.mod.2$gam) ## autocorrelation = 0.9

##  show ACF & PACF:
layout(matrix(1:2, ncol = 2))
acf(resid(sec1.mod.2$lme, type="normalized"), lag.max = 24, main = "ACF")
pacf(resid(sec1.mod.2$lme, type="normalized"), lag.max = 24, main = "pACF")
layout(1)

#   compare models:
anova(sec1.mod.1$lme, sec1.mod.2$lme) ## Model 2 = superior


##  3) FULL - WITH LEVELS AND SLOPES
# ..............

## seasonality + AR2 + MA1 + level/slope effects
sec1.mod.3 <- gamm(secondary_outp ~ con_week + s(week, bs = "cc", k=20) +
                p2.level + p2.slope + p3.level + p3.slope + 
                p4.level + p4.slope + p5.level + p5.slope, 
              correlation = corARMA(form = ~ 1|year, p = 2, q = 1),
              data = ts_dat2, method = "REML")
summary(sec1.mod.3$gam)
plot(sec1.mod.3$gam)
dwtest(sec1.mod.3$gam) ## autocorrelation = 1.3

##  save ACF & PACF:
png(filename = paste0("figs/", Sys.Date(), "_plot_acf_pacf_model_secondary_outp.png"), width = 700, height = 600)
par(mfrow = c(2, 1))
acf(resid(sec1.mod.3$lme, type="normalized"), lag.max = 24, main = "ACF")
pacf(resid(sec1.mod.3$lme, type="normalized"), lag.max = 24, main = "pACF")
dev.off()
ggqqplot(resid(sec1.mod.3$lme, type="normalized")) ## residuals look ok

##  Predict:
ts_dat2$secondary_outp_pred <- predict(sec1.mod.3$gam)


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 5) ITS - SECONDARY - INPATIENT
# ______________________________________________________________________________________________________________________

##  1) BASELINE - PREPANDEMIC
# ..............

##  baseline model a) linear trend and 10 knots
sec2.mod.0a <- gamm(secondary_inp ~ con_week + s(week, bs = "cc", k=10), data = ts_dat1, method = "REML")
ts_dat1$pred_0a <- predict(sec2.mod.0a$gam)

##  baseline model b) linear trend and 20 knots
sec2.mod.0b <- gamm(secondary_inp ~ con_week + s(week, bs = "cc", k=20), data = ts_dat1, method = "REML")
ts_dat1$pred_0b <- predict(sec2.mod.0b$gam)

##  baseline model c) linear and quadratic trend and 10 knots
sec2.mod.0c <- gamm(secondary_inp ~ con_week + con_week_sq + s(week, bs = "cc", k=10), data = ts_dat1, method = "REML")
ts_dat1$pred_0c <- predict(sec2.mod.0c$gam)

##  baseline model d) linear and quadratic trend and 20 knots
sec2.mod.0d <- gamm(secondary_inp ~ con_week + con_week_sq + s(week, bs = "cc", k=20), data = ts_dat1, method = "REML")
ts_dat1$pred_0d <- predict(sec2.mod.0d$gam)

##  COMPARE
anova(sec2.mod.0a$lme, sec2.mod.0b$lme) ## no test and no real diff
anova(sec2.mod.0c$lme, sec2.mod.0d$lme) ## no test and no real diff
anova(sec2.mod.0a$lme, sec2.mod.0c$lme) ## Model c = superior (with cubic)
anova(sec2.mod.0b$lme, sec2.mod.0d$lme) ## Model d = superior (with cubic)


##  2) FULL - correcting for autocorrelation
# ..............

##  baseline model d) linear trend and 20 knots
sec2.mod.1 <- gamm(secondary_inp ~ con_week + con_week_sq + s(week, bs = "cc", k=20), data = ts_dat2, method = "REML")
summary(sec2.mod.1$gam)
plot(sec2.mod.1$gam) ## show seasonal effect
dwtest(sec2.mod.1$gam) ## autocorrelation = 0.5

##  remove autocorrelation:
# let AUTO.ARIMA check for AR/MA terms:
auto.arima(resid(sec2.mod.1$lme, type = "normalized"),
           stationary = TRUE, seasonal = FALSE) # AR1

## seasonality + AR1
sec2.mod.2 <- gamm(secondary_inp ~ con_week + con_week_sq + s(week, bs = "cc", k=20),
                   correlation = corARMA(form = ~ 1|year, p = 1),
                   data = ts_dat2, method = "REML")
summary(sec2.mod.2$gam)
plot(sec2.mod.2$gam)
dwtest(sec2.mod.2$gam) ## autocorrelation = 0.9

##  show ACF & PACF:
layout(matrix(1:2, ncol = 2))
acf(resid(sec2.mod.2$lme, type="normalized"), lag.max = 24, main = "ACF")
pacf(resid(sec2.mod.2$lme, type="normalized"), lag.max = 24, main = "pACF")
layout(1)

#   compare models:
anova(sec2.mod.1$lme, sec2.mod.2$lme) ## Model 2 = superior


##  3) FULL - WITH LEVELS AND SLOPES
# ..............

## seasonality + AR4 + MA3 + level/slope effects
sec2.mod.3 <- gamm(secondary_inp ~ con_week + con_week_sq + s(week, bs = "cc", k=20) +
                     p2.level + p2.slope + p3.level + p3.slope + 
                     p4.level + p4.slope + p5.level + p5.slope, 
                   correlation = corARMA(form = ~ 1|year, p = 1),
                   data = ts_dat2, method = "REML")
summary(sec2.mod.3$gam)
plot(sec2.mod.3$gam)
dwtest(sec2.mod.3$gam) ## autocorrelation = 0.7

##  save ACF & PACF:
png(filename = paste0("figs/", Sys.Date(), "_plot_acf_pacf_model_secondary_inp.png"), width = 700, height = 600)
par(mfrow = c(2, 1))
acf(resid(sec2.mod.3$lme, type="normalized"), lag.max = 24, main = "ACF")
pacf(resid(sec2.mod.3$lme, type="normalized"), lag.max = 24, main = "pACF")
dev.off()
ggqqplot(resid(sec2.mod.3$lme, type="normalized")) ## residuals look ok

##  Predict:
ts_dat2$secondary_inp_pred <- predict(sec2.mod.3$gam)



# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 6) TABLES
# ______________________________________________________________________________________________________________________

##  1) TAB 2
# ..............

sjPlot::tab_model(pri.mod.3$gam,
                  sec1.mod.3$gam,
                  sec2.mod.3$gam,
                  show.ci = T,show.p = T,
                  dv.labels = c("primary","secondary: outpatient","secondary: inpatient"),
                  file = paste0("tables/Table2_model summary_",Sys.Date(),".html"))


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 4) FIGURES
# ______________________________________________________________________________________________________________________

##  1) FIG 1
# ..............

##  observed and predicted primary / secondary
pdat1 <- melt(ts_dat2[,.(date.start,
                        primary,
                        secondary_outp,
                        secondary_inp)],
             id.vars = c("date.start"), value.name = "observed")
pdat2 <- melt(ts_dat2[,.(date.start,
                         primary=as.integer(primary_pred),
                         secondary_outp=as.integer(secondary_outp_pred),
                         secondary_inp=as.integer(secondary_inp_pred))],
              id.vars = c("date.start"), value.name = "predicted")
pdat <- merge(pdat1,pdat2, by = c("date.start","variable"))

ggplot(pdat, aes(x = date.start, y = observed, color = variable)) + 
  ggtitle("Number of people per week in any AUD treatment (and by outpatient/inpatient)",
          paste0("Dashed vertical lines separating pandemic periods: ", date.period2," | ", date.period3," | ", date.period4," | ", date.period5)) +
  geom_vline(xintercept = c(as.Date(date.period2),as.Date(date.period3),as.Date(date.period4),as.Date(date.period5)), linetype = 2) +
  #geom_vline(xintercept = c(as.Date("2020-11-02"),as.Date("2021-08-17")), linetype = 2) +
  geom_point(shape = 22, size = 2) +  
  geom_line(aes(y = predicted), linewidth = 0.5) +
  scale_color_viridis_d("") +
  scale_x_date("", breaks = "6 months", date_labels = "%y-%b") +
  scale_y_continuous("N") +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))

ggsave(filename = paste0("figs/", Sys.Date(), "_FIG_1_time series primary-secondary outcomes.png"),
       width = 12, height = 6)

##  2) FIG 2
# ..............

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 5) SUPPL FIGURES
# ______________________________________________________________________________________________________________________

##  1) SUPP FIG 1
# ..............


