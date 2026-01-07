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

# date
DATE <- Sys.Date() #"2025-06-18"
DATE <- "2025-07-29"

# load libraries
library( data.table )
library( ggplot2 )
library( ggthemes )
library( ggpubr ) ## for ggqqplot
library( mgcv ) ## for gam/gamm

# themes and options and colors
theme_set( theme_gdocs() )
options(scipen = 999)
col3 <- c("#1F77B4","#2CA02C","#FF7F0E")
col.yel <- c("#FFD700") # c("#FFFFE0")
col.red <- c("#FF6F6F") # c("#FFEDED")


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 1) LOAD DATA AND ADD PANDEMIC PERIODS
# ______________________________________________________________________________________________________________________

##  from: 0_prepared data.R
filename <- paste0("data/output/", DATE, "_summary data.RDS")
sum.dat <- readRDS(filename)

filename <- paste0("data/output/", DATE, "_period and stringency data.RDS")
period.dat <- readRDS(filename)

filename <- paste0("data/output/", DATE, "_year count.RDS")
agg.y <- readRDS(filename)

filename <- paste0("data/output/", DATE, "_quarter count.RDS")
agg.q <- readRDS(filename)

filename <- paste0("data/output/", DATE, "_month count.RDS")
agg.m <- readRDS(filename)

##  from: 1_ITS main analyses.R
filename <- paste0("data/output/",DATE,"_primary outcome_model_main.rds")
pri_mod_main <- readRDS(filename)
filename <- paste0("data/output/",DATE,"_secondary outcome1_model_main.rds")
sec1_mod_main <- readRDS(filename)
filename <- paste0("data/output/",DATE,"_secondary outcome2_model_main.rds")
sec2_mod_main <- readRDS(filename)
filename <- paste0("data/output/", DATE, "_main data with predictions.RDS")
data <- readRDS(filename)

##  from: 2_ITS sensitivity analyses.R
filename <- paste0("data/output/",DATE,"_primary outcome_model_sensitivity.rds")
pri_mod_sens <- readRDS(filename)
filename <- paste0("data/output/",DATE,"_secondary outcome1_model_sensitivity.rds")
sec1_mod_sens <- readRDS(filename)
filename <- paste0("data/output/",DATE,"_secondary outcome2_model_sensitivity.rds")
sec2_mod_sens <- readRDS(filename)
filename <- paste0("data/output/", DATE, "_sensitivity data with predictions.RDS")
data_sens <- readRDS(filename)

##  dates:
data[, .(min = min(date.start), max = max(date.start)), by = period]
date.period2 <- data[period == 2, min(date.start)]
date.period3 <- data[period == 3, min(date.start)]
date.period4 <- data[period == 4, min(date.start)]
date.period5 <- data[period == 5, min(date.start)]

##  background shades
background_shades <- data.frame(
  period = factor(c(1,2,3,4,5)),
  xmin = c(as.Date(as.character(min(data$date)-100)),date.period2,date.period3,date.period4,date.period5),
  xmax = c(date.period2,date.period3,date.period4,date.period5,as.Date(as.character(max(data$date)+1)))
)

##  date breaks
date.breaks <- c(as.Date("2016-01-01"),
                 as.Date("2016-07-01"),
                 as.Date("2017-01-01"),
                 as.Date("2017-07-01"),
                 as.Date("2018-01-01"),
                 as.Date("2018-07-01"),
                 as.Date("2019-01-01"),
                 as.Date("2019-07-01"),
                 as.Date("2020-01-01"),
                 as.Date("2020-07-01"),
                 as.Date("2021-01-01"),
                 as.Date("2021-07-01"))


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 2) REPORT DATA
# ______________________________________________________________________________________________________________________

##  1) SAMPLE DESCRIPTION
# ..............

nrow(sum.dat)
sum.dat[, table(sex)]
sum.dat[, mean(sex == "female")]
sum.dat[, mean(sex == "male")]
sum.dat[, mean(2020-yob)]
sum.dat[, summary(2020-yob)]

# by intervention
sum.dat[, table(int.psych_short)]
sum.dat[, prop.table(table(int.psych_short))] # 54.6%
sum.dat[, table(int.psych_full)]
sum.dat[, prop.table(table(int.psych_full))] # 2.0%
sum.dat[, table(int.medi)]
sum.dat[, prop.table(table(int.medi))] # 4.8%
sum.dat[, table(int.inpat)]
sum.dat[, prop.table(table(int.inpat))] # 31.6
sum.dat[, table(int.qwt)]
sum.dat[, prop.table(table(int.qwt))] # 33.9%
sum.dat[, table(int.reha_outp)]
sum.dat[, prop.table(table(int.reha_outp))] # 9.3%
sum.dat[, table(int.reha_inp)]
sum.dat[, prop.table(table(int.reha_inp))] # 9.9%



# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 3) TABLES
# ______________________________________________________________________________________________________________________

##  1) SUPPLEMENTARY TABLE 1
# ..............

# period 1:
period.dat[date < as.Date("2020-03-22"),]
period.dat[date < as.Date("2020-03-22"), summary(stringency)]
# period 2:
period.dat[date %between% c(as.Date("2020-03-22"),as.Date("2020-05-05"))]
period.dat[date %between% c(as.Date("2020-03-22"),as.Date("2020-05-05")), summary(stringency)]
# period 3:
period.dat[date %between% c(as.Date("2020-05-06"),as.Date("2020-12-14")),]
period.dat[date %between% c(as.Date("2020-05-06"),as.Date("2020-12-14")), summary(stringency)]
# period 4:
period.dat[date %between% c(as.Date("2020-12-15"),as.Date("2021-05-30")),]
period.dat[date %between% c(as.Date("2020-12-15"),as.Date("2021-05-30")), summary(stringency)]
# period 5:
period.dat[date %between% c(as.Date("2021-05-31"),as.Date("2021-12-31")),]
period.dat[date %between% c(as.Date("2021-05-31"),as.Date("2021-12-31")), summary(stringency)]

##  2) SUPPLEMENTARY TABLE 2
# ..............

agg.y
sum.dat
1-agg.y[year == 2020]$primary / agg.y[year == 2019]$primary 
1-agg.y[year == 2021]$primary / agg.y[year == 2019]$primary 



# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 4) FIGURES
# ______________________________________________________________________________________________________________________

##  1) FIG 1
# ..............

##  observed and predicted primary / secondary
pdat1 <- melt(data[,.(date.start,
                      primary,
                      secondary_outp,
                      secondary_inp)],
              id.vars = c("date.start"), value.name = "observed")
pdat2 <- melt(data[,.(date.start,
                      primary=as.integer(primary_pred.2),
                      secondary_outp=as.integer(secondary_outp_pred.1),
                      secondary_inp=as.integer(secondary_inp_pred.2))],
              id.vars = c("date.start"), value.name = "predicted")
pdat <- merge(pdat1,pdat2, by = c("date.start","variable"))

pdat[, variable := dplyr::recode(variable,
                                 "primary" = "Any treatment (primary)",
                                 "secondary_outp" = "Outpatient treatment (secondary)",
                                 "secondary_inp" = "Inpatient treatment (secondary)")]


ggplot(pdat, aes(x = date.start, y = observed, color = variable)) + 
  geom_rect(data = background_shades, 
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = period), 
            inherit.aes = FALSE, show.legend = F, alpha = 0.3) +
  #ggtitle("Number of people per week in any AUD treatment (and by outpatient/inpatient)",
  #        paste0("Dashed vertical lines separating pandemic periods: ", date.period2," | ", date.period3," | ", date.period4," | ", date.period5)) +
  #geom_vline(xintercept = c(years), linetype = 3) +
  geom_vline(xintercept = c(as.Date(date.period2),as.Date(date.period3),as.Date(date.period4),as.Date(date.period5)), linetype = 2) +
  geom_point(shape = 22, size = 2) +  
  geom_line(aes(y = predicted), linewidth = 0.5) +
  scale_color_manual("", values = col3) +
  scale_fill_manual("periods", values = c("white",col.red,col.yel,col.red,col.yel)) +
  scale_x_date("", limits = c(min(pdat$date.start)-1,max(pdat$date.start)+1), 
               breaks = date.breaks, date_labels = "%d %b %y", expand = c(0.01, 0.01)) +
  scale_y_continuous("N", limits = c(0,max(pdat$observed)+10)) +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))

ggsave(filename = paste0("figs/", Sys.Date(), "_FIG_1_time series primary-secondary outcomes.png"),
       width = 12, height = 6)
ggsave(filename = paste0("figs/", Sys.Date(), "_FIG_1_time series primary-secondary outcomes.tif"),
       width = 12, height = 6)


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 4) SUPPLEMENTAL FIGURES
# ______________________________________________________________________________________________________________________

##  1) SUPP FIG 1
# ..............

##  quarterly aggregation
pdat <- copy(agg.q)

ggplot(pdat, aes(x = date.start, y = primary)) + 
  geom_rect(data = background_shades, 
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = period), 
            inherit.aes = FALSE, show.legend = F, alpha = 0.3) +
  ggtitle("Number of people per quarter in any AUD treatment",
          paste0("Dashed vertical lines separating pandemic periods: ", date.period2," | ", date.period3," | ", date.period4," | ", date.period5)) +
  geom_vline(xintercept = c(as.Date(date.period2),as.Date(date.period3),as.Date(date.period4),as.Date(date.period5)), linetype = 2) +
  geom_point(shape = 22, size = 2) +  
  geom_line(linewidth = 0.5) +
  scale_fill_manual("periods", values = c("white",col.red,col.yel,col.red,col.yel)) +
  scale_x_date("", breaks = date.breaks, date_labels = "%d %b %y", expand = c(0.01, 0.01)) +
  scale_y_continuous("N", limits = c(0,1300)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))

ggsave(filename = paste0("figs/", Sys.Date(), "_SUPP FIG_1_time series primary MAIN quarterly.png"),
       width = 12, height = 6)

rm(pdat)

##  2) SUPP FIG 2
# ..............

##  monthly aggregation
pdat <- copy(agg.m)

ggplot(pdat, aes(x = date.start, y = primary)) + 
  geom_rect(data = background_shades, 
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = period), 
            inherit.aes = FALSE, show.legend = F, alpha = 0.3) +
  ggtitle("Number of people per month in any AUD treatment",
          paste0("Dashed vertical lines separating pandemic periods: ", date.period2," | ", date.period3," | ", date.period4," | ", date.period5)) +
  geom_vline(xintercept = c(as.Date(date.period2),as.Date(date.period3),as.Date(date.period4),as.Date(date.period5)), linetype = 2) +
  geom_point(shape = 22, size = 2) +  
  geom_line(linewidth = 0.5) +
  scale_fill_manual("periods", values = c("white",col.red,col.yel,col.red,col.yel)) +
  scale_x_date("", breaks = date.breaks, date_labels = "%d %b %y", expand = c(0.01, 0.01)) +
  scale_y_continuous("N", limits = c(0,700)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))

ggsave(filename = paste0("figs/", Sys.Date(), "_SUPP FIG_2_time series primary MAIN monthly.png"),
       width = 12, height = 6)

rm(pdat)


##  3) SUPP FIG 3
# ..............

pdat <- copy(period.dat[year(date) < 2022])

ggplot(pdat, aes(x = date, y = stringency)) + 
  geom_rect(data = background_shades, 
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = period), 
            inherit.aes = FALSE, show.legend = F, alpha = 0.3) +
  ggtitle("Daily OxCGRT Stringency Index",
          paste0("Dashed vertical lines separating pandemic periods: ", date.period2," | ", date.period3," | ", date.period4," | ", date.period5)) +
  geom_vline(xintercept = c(as.Date(date.period2),as.Date(date.period3),as.Date(date.period4),as.Date(date.period5)), linetype = 2) +
  geom_line(linewidth = 0.5) +
  scale_fill_manual("periods", values = c("white",col.red,col.yel,col.red,col.yel)) +
  scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2022-01-01")),"", 
               breaks = "3 months", date_labels = "%d %b %y", expand = c(0.01, 0.01)) +
  scale_y_continuous("", limits = c(0,100)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))

ggsave(filename = paste0("figs/", Sys.Date(), "_SUPP FIG_3_time series OxCGRT.png"),
       width = 12, height = 6)

rm(pdat)


##  4) SUPP FIG 4
# ..............

pdat <- melt(data[year > 2019,.(date.start,cases,deaths)], id.vars = "date.start")
pdat[, value_diff := c(0,diff(value)), by = variable]

ggplot(pdat, aes(x = date.start, y = value_diff, linetype = variable)) + 
  ggtitle("Weekly COVID-19 cases and deaths",
          paste0("Dashed vertical lines separating pandemic periods: ", date.period2," | ", date.period3," | ", date.period4," | ", date.period5)) +
  geom_rect(data = background_shades, 
            aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 10000000, fill = period), 
            inherit.aes = FALSE, show.legend = F, alpha = 0.3) +
  scale_fill_manual("periods", values = c("white",col.red,col.yel,col.red,col.yel)) +
  scale_linetype_discrete("") +
  geom_vline(xintercept = c(as.Date(date.period2),as.Date(date.period3),as.Date(date.period4),as.Date(date.period5)), linetype = 2) +
  geom_line(linewidth = 0.5) +
  scale_x_date(limits = c(as.Date("2020-01-01"),as.Date("2022-01-01")),"", 
               breaks = "3 months", date_labels = "%d %b %y", expand = c(0.01, 0.01)) +
  scale_y_continuous("N", transform = "log10") + 
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))

ggsave(filename = paste0("figs/", Sys.Date(), "_SUPP FIG_4_time series covide cases and deaths.png"),
       width = 12, height = 6)


rm(pdat)


##  5) SUPP FIG 5
# ..............

# ACF and pACF plots of main models

res.pri <- resid(pri_mod_main$lme, type = "normalized")
res.sec1 <- resid(sec1_mod_main$lme, type = "normalized")
res.sec2 <- resid(sec2_mod_main$lme, type = "normalized")


png(paste0("figs/", Sys.Date(), "_SUPP FIG_5_residuals main models.png"),
    width = 1200, height = 900, res = 130)

# plots by models and ACF, PACF
par(mfrow = c(3, 2), mar = c(4, 4, 4, 1), oma = c(0, 0, 4, 0))

  acf(res.pri, main = "ACF: Primary outcome")
  pacf(res.pri, main = "PACF: Primary outcome")
  
  acf(res.sec1, main = "ACF: Secondary outcome 1")
  pacf(res.sec1, main = "PACF: Secondary outcome 1")
  
  acf(res.sec2, main = "ACF: Secondary outcome 2")
  pacf(res.sec2, main = "PACF: Secondary outcome 2")
  
  mtext("Main analyses: ACF (left) and pACF plots (right) of model residuals", 
        side = 3, line = 1, outer = TRUE, cex = 1.5, font = 2)
  
dev.off()

rm(res.pri, res.sec1, res.sec2)


##  6) SUPP FIG 6
# ..............

# ACF and pACF plots of sens models

res.pri <- resid(pri_mod_sens$lme, type = "normalized")
res.sec1 <- resid(sec1_mod_sens$lme, type = "normalized")
res.sec2 <- resid(sec2_mod_sens$lme, type = "normalized")


png(paste0("figs/", Sys.Date(), "_SUPP FIG_6_residuals sens models.png"),
    width = 1200, height = 900, res = 130)

# plots by models and ACF, PACF
par(mfrow = c(3, 2), mar = c(4, 4, 4, 1), oma = c(0, 0, 4, 0))

acf(res.pri, main = "ACF: Primary outcome")
pacf(res.pri, main = "PACF: Primary outcome")

acf(res.sec1, main = "ACF: Secondary outcome 1")
pacf(res.sec1, main = "PACF: Secondary outcome 1")

acf(res.sec2, main = "ACF: Secondary outcome 2")
pacf(res.sec2, main = "PACF: Secondary outcome 2")

mtext("Sensitivity analyses: ACF (left) and pACF plots (right) of model residuals", 
      side = 3, line = 1, outer = TRUE, cex = 1.5, font = 2)

dev.off()


##  7) SUPP FIG 7
# ..............

##  SENSITIVITY: observed and predicted primary / secondary
pdat1 <- melt(data_sens[,.(date.start,
                           primary,
                           secondary_outp,
                           secondary_inp)],
              id.vars = c("date.start"), value.name = "observed")
pdat2 <- melt(data_sens[,.(date.start,
                           primary=as.integer(primary_pred.2),
                           secondary_outp=as.integer(secondary_outp_pred.1),
                           secondary_inp=as.integer(secondary_inp_pred.2))],
              id.vars = c("date.start"), value.name = "predicted")
pdat <- merge(pdat1,pdat2, by = c("date.start","variable"))

pdat[, variable := dplyr::recode(variable,
                                 "primary" = "Any treatment (primary)",
                                 "secondary_outp" = "Outpatient treatment (secondary)",
                                 "secondary_inp" = "Inpatient treatment (secondary)")]

ggplot(pdat, aes(x = date.start, y = observed, color = variable)) + 
  geom_rect(data = background_shades, 
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = period), 
            inherit.aes = FALSE, show.legend = F, alpha = 0.3) +
  ggtitle("Sensitivity analyses: Number of people per week in any/outpatient/inpatient AUD treatment",
          paste0("Without rehabilitation treatment; dashed vertical lines separating pandemic periods: ", date.period2," | ", date.period3," | ", date.period4," | ", date.period5)) +
  geom_vline(xintercept = c(as.Date(date.period2),as.Date(date.period3),as.Date(date.period4),as.Date(date.period5)), linetype = 2) +
  geom_point(shape = 22, size = 2) +  
  geom_line(aes(y = predicted), linewidth = 0.5) +
  scale_color_manual("", values = col3) +
  scale_fill_manual("periods", values = c("white",col.red,col.yel,col.red,col.yel)) +
  scale_x_date("", limits = c(min(pdat$date.start)-1,max(pdat$date.start)+1), 
               breaks = date.breaks, date_labels = "%d %b %y", expand = c(0.01, 0.01)) +
  scale_y_continuous("N", limits = c(0,max(pdat$observed)+10)) +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))

ggsave(filename = paste0("figs/", Sys.Date(), "_SUPP FIG_7_time series primary-secondary outcomes_SENS.png"),
       width = 12, height = 6)


