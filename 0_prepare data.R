# ==================================================================================================================================================================
# ==================================================================================================================================================================

# PROJECT TITLE:  PRAGMA
# CODE AUTHORS:   JM
# DATE STARTED:   2024/12/04

# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 0) ESSENTIALS
# ______________________________________________________________________________________________________________________

# clean workspace
rm(list=ls())

# date
DATE <- "2024-08-19"

# load libraries
library( data.table )
library( ggplot2 )
library( ggthemes )
#library( tidyr )
#library( stringr )
#library( lubridate )

options(scipen = 999)




# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 1) LOAD DATA
# ______________________________________________________________________________________________________________________
# ______________________________________________________________________________________________________________________
# ______________________________________________________________________________________________________________________

# GKV Stammdaten
filename <- paste0("data/input/0_pragma_id_GKV with Stammdata_", DATE,".rds")
id.dat <- readRDS(filename)
id.dat <- id.dat[gkv != "drvonly"]
rm(filename)

# INTERVENTIONS
##  PSYCH-BRIEF
filename <- paste0("data/input/2_PSYCH_SHORT data_", DATE,".rds")
interv.dat.1 <- readRDS(filename)
rm(filename)

##  PSYCH-LONG
filename <- paste0("data/input/2_PSYCH_FULL data_", DATE,".rds")
interv.dat.2 <- readRDS(filename)
rm(filename)

##  PHARMA
filename <- paste0("data/input/2_MEDI data_", DATE,".rds")
interv.dat.3 <- readRDS(filename)
rm(filename)

##  INPAT-STANDARD
filename <- paste0("data/input/2_INPAT data_", DATE,".rds")
interv.dat.4 <- readRDS(filename)
rm(filename)

##  INPAT-INTENSE
filename <- paste0("data/input/2_QWT data_", DATE,".rds")
interv.dat.5 <- readRDS(filename)
rm(filename)

##  REHAB
filename <- paste0("data/input/2_REHA-DRV and GKV data_", DATE,".rds")
interv.dat.6 <- readRDS(filename)
rm(filename)

##  PANDEMIC PERIODS
# downloaded on 22 may 2025 here: https://github.com/OxCGRT/covid-policy-dataset/tree/main/data
# subnational
#temp <- data.table(read.csv("data/input/OxCGRT_compact_subnational_v1.csv"))
#temp[CountryCode == "DEU"] # no info
#rm(temp2)

# national
period.input1 <- data.table(read.csv("data/input/OxCGRT_compact_national_v1.csv"))
period.input1[CountryCode == "DEU", .(Date,StringencyIndex_Average,ContainmentHealthIndex_Average)]

period.dat <- period.input1[CountryCode == "DEU", .(date = Date,stringency = StringencyIndex_Average,cases = ConfirmedCases,deaths = ConfirmedDeaths)]
period.dat$date <- as.Date(as.character(period.dat$date), format = "%Y%m%d")

# periods added manually
period.input2 <- data.table(openxlsx::read.xlsx("data/input/covid-19-stringency-index.xlsx", detectDates = T))



# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# create empty data file
# ..............

empty <- data.table(date = seq(as.Date("2016-01-01"),as.Date("2021-12-31"), by = 1))
empty[, ':=' (year = year(date),
              quarter = quarter(date),
              month = month(date),
              week = isoweek(date),
              wday = wday(date),
              weekday = weekdays(date))]
empty[, ':=' (time = rleid(week))]
empty[1:50]

empty[year == 2020 & month == 3]
empty[(year == 2020 & month == 12)|(year == 2021 & month == 1)] # looks correct

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 2) PREPARE AGGREGATED DATA - YEARLY
# ______________________________________________________________________________________________________________________
# ______________________________________________________________________________________________________________________
# ______________________________________________________________________________________________________________________

# prepare yearly data for each ID
# ..............

dat.y <- unique(empty[,.(date.start = min(date),
                         date.end = max(date)), by = .(year)])

add <- rbind(id.dat[,.(pragmaid,source,gkv,year = 2016)],
             id.dat[,.(pragmaid,source,gkv,year = 2017)],
             id.dat[,.(pragmaid,source,gkv,year = 2018)],
             id.dat[,.(pragmaid,source,gkv,year = 2019)],
             id.dat[,.(pragmaid,source,gkv,year = 2020)],
             id.dat[,.(pragmaid,source,gkv,year = 2021)])

dat.y <- merge(add, dat.y, by = "year", allow.cartesian = T)
dat.y[, .N, by = .(pragmaid,source)] # 6
rm(add)

# add interventions
# ..............

dat.y <- merge(dat.y,
               interv.dat.1[,.(source,pragmaid,date.psych_short)], 
               by = c("source","pragmaid"), all.x = T, allow.cartesian = T)

dat.y <- merge(dat.y,
               interv.dat.2[,.(source,pragmaid,date.psych_full.start,date.psych_full.end)],
               by = c("source","pragmaid"), all.x = T, allow.cartesian = T)

dat.y <- merge(dat.y,
               interv.dat.3[,.(gkv,pragmaid,date.medi)],
               by = c("gkv","pragmaid"), all.x = T, allow.cartesian = T)

dat.y <- merge(dat.y,
               interv.dat.4[,.(gkv,pragmaid,date.inpat.start,date.inpat.end)],
               by = c("gkv","pragmaid"), all.x = T, allow.cartesian = T)

dat.y <- merge(dat.y,
               interv.dat.5[,.(gkv,pragmaid,date.qwt.start,date.qwt.end)],
               by = c("gkv","pragmaid"), all.x = T, allow.cartesian = T)

dat.y <- merge(dat.y,
               interv.dat.6[,.(pragmaid,date.reha.start,date.reha.end,reha.typ)],
               by = c("pragmaid"), all.x = T, allow.cartesian = T)


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 3) PREPARE AGGREGATED DATA - QUARTERLY
# ______________________________________________________________________________________________________________________
# ______________________________________________________________________________________________________________________
# ______________________________________________________________________________________________________________________


# prepare quarterly data for each ID
# ..............

dat.q <- unique(empty[,.(date.start = min(date),
                         date.end = max(date)), by = .(year,quarter)])

add <- rbind(id.dat[,.(pragmaid,source,gkv,year = 2016)],
             id.dat[,.(pragmaid,source,gkv,year = 2017)],
             id.dat[,.(pragmaid,source,gkv,year = 2018)],
             id.dat[,.(pragmaid,source,gkv,year = 2019)],
             id.dat[,.(pragmaid,source,gkv,year = 2020)],
             id.dat[,.(pragmaid,source,gkv,year = 2021)])

dat.q <- merge(add, dat.q, by = "year", allow.cartesian = T)
dat.q[, .N, by = .(pragmaid,source)] # 24 = 6*4
rm(add)

# add interventions
# ..............

dat.q <- merge(dat.q,
               interv.dat.1[,.(source,pragmaid,date.psych_short)], 
               by = c("source","pragmaid"), all.x = T, allow.cartesian = T)

dat.q <- merge(dat.q,
               interv.dat.2[,.(source,pragmaid,date.psych_full.start,date.psych_full.end)],
               by = c("source","pragmaid"), all.x = T, allow.cartesian = T)

dat.q <- merge(dat.q,
               interv.dat.3[,.(gkv,pragmaid,date.medi)],
               by = c("gkv","pragmaid"), all.x = T, allow.cartesian = T)

dat.q <- merge(dat.q,
               interv.dat.4[,.(gkv,pragmaid,date.inpat.start,date.inpat.end)],
               by = c("gkv","pragmaid"), all.x = T, allow.cartesian = T)

dat.q <- merge(dat.q,
               interv.dat.5[,.(gkv,pragmaid,date.qwt.start,date.qwt.end)],
               by = c("gkv","pragmaid"), all.x = T, allow.cartesian = T)

dat.q <- merge(dat.q,
               interv.dat.6[,.(pragmaid,date.reha.start,date.reha.end,reha.typ)],
               by = c("pragmaid"), all.x = T, allow.cartesian = T)

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 4) PREPARE AGGREGATED DATA - MONTHLY
# ______________________________________________________________________________________________________________________
# ______________________________________________________________________________________________________________________
# ______________________________________________________________________________________________________________________

# prepare monthly data for each ID
# ..............

dat.m <- unique(empty[,.(date.start = min(date),
                         date.end = max(date)), by = .(year,month)])

add <- rbind(id.dat[,.(pragmaid,source,gkv,year = 2016)],
             id.dat[,.(pragmaid,source,gkv,year = 2017)],
             id.dat[,.(pragmaid,source,gkv,year = 2018)],
             id.dat[,.(pragmaid,source,gkv,year = 2019)],
             id.dat[,.(pragmaid,source,gkv,year = 2020)],
             id.dat[,.(pragmaid,source,gkv,year = 2021)])

dat.m <- merge(add, dat.m, by = "year", allow.cartesian = T)
dat.m[, .N, by = .(pragmaid,source)] # 72 = 6*12
rm(add)

# add interventions
# ..............

dat.m <- merge(dat.m,
               interv.dat.1[,.(source,pragmaid,date.psych_short)], 
               by = c("source","pragmaid"), all.x = T, allow.cartesian = T)

dat.m <- merge(dat.m,
               interv.dat.2[,.(source,pragmaid,date.psych_full.start,date.psych_full.end)],
               by = c("source","pragmaid"), all.x = T, allow.cartesian = T)

dat.m <- merge(dat.m,
               interv.dat.3[,.(gkv,pragmaid,date.medi)],
               by = c("gkv","pragmaid"), all.x = T, allow.cartesian = T)

dat.m <- merge(dat.m,
               interv.dat.4[,.(gkv,pragmaid,date.inpat.start,date.inpat.end)],
               by = c("gkv","pragmaid"), all.x = T, allow.cartesian = T)

dat.m <- merge(dat.m,
               interv.dat.5[,.(gkv,pragmaid,date.qwt.start,date.qwt.end)],
               by = c("gkv","pragmaid"), all.x = T, allow.cartesian = T)

dat.m <- merge(dat.m,
               interv.dat.6[,.(pragmaid,date.reha.start,date.reha.end,reha.typ)],
               by = c("pragmaid"), all.x = T, allow.cartesian = T)


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 5) PREPARE AGGREGATED DATA - WEEKLY
# ______________________________________________________________________________________________________________________
# ______________________________________________________________________________________________________________________
# ______________________________________________________________________________________________________________________

# prepare weekly data for each ID
# ..............

dat.w <- unique(empty[,.(date.start = min(date),
                         date.end = max(date),
                         n_days = length(unique(date))), by = .(time)])
dat.w[, ':=' (year = year(date.start),
              week = isoweek(date.start))]
dat.w[(year == 2020 & week >= 50)|(year == 2021 & week <= 5)] # looks correct

add <- rbind(id.dat[,.(pragmaid,source,gkv,year = 2016)],
             id.dat[,.(pragmaid,source,gkv,year = 2017)],
             id.dat[,.(pragmaid,source,gkv,year = 2018)],
             id.dat[,.(pragmaid,source,gkv,year = 2019)],
             id.dat[,.(pragmaid,source,gkv,year = 2020)],
             id.dat[,.(pragmaid,source,gkv,year = 2021)])

dat.w <- merge(add, dat.w, by = "year", allow.cartesian = T)
dat.w[, .N, by = .(pragmaid,source)] # 314 = 6*53-4 (4 years with only 52 weeks)
rm(add)

# add interventions
# ..............

dat.w <- merge(dat.w,
               interv.dat.1[,.(source,pragmaid,date.psych_short)], 
               by = c("source","pragmaid"), all.x = T, allow.cartesian = T)

dat.w <- merge(dat.w,
               interv.dat.2[,.(source,pragmaid,date.psych_full.start,date.psych_full.end)],
               by = c("source","pragmaid"), all.x = T, allow.cartesian = T)

dat.w <- merge(dat.w,
               interv.dat.3[,.(gkv,pragmaid,date.medi)],
               by = c("gkv","pragmaid"), all.x = T, allow.cartesian = T)

dat.w <- merge(dat.w,
               interv.dat.4[,.(gkv,pragmaid,date.inpat.start,date.inpat.end)],
               by = c("gkv","pragmaid"), all.x = T, allow.cartesian = T)

dat.w <- merge(dat.w,
               interv.dat.5[,.(gkv,pragmaid,date.qwt.start,date.qwt.end)],
               by = c("gkv","pragmaid"), all.x = T, allow.cartesian = T)

dat.w <- merge(dat.w,
               interv.dat.6[,.(pragmaid,date.reha.start,date.reha.end,reha.typ)],
               by = c("pragmaid"), all.x = T, allow.cartesian = T)
gc()


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 6) DETERMINE OVERLAPS IN AGGREGATED DATA
# ______________________________________________________________________________________________________________________
# ______________________________________________________________________________________________________________________
# ______________________________________________________________________________________________________________________

dat.list <- list(dat.y,dat.q,dat.m,dat.w)
i <- 1
for(DAT in dat.list){
  
  # GET date.int# = person was in treatment in that period
  DAT$date.int1 <- F
  DAT$date.int2 <- F
  DAT$date.int3 <- F
  DAT$date.int4 <- F
  DAT$date.int5 <- F
  DAT$date.int6.outp <- F
  DAT$date.int6.inp <- F
  
  # PSYCH-BRIEF
  DAT[date.psych_short %between% list(date.start,date.end)]
  DAT[date.psych_short %between% list(date.start,date.end), date.int1 := T]
  
  # PSYCH-LONG
  #DAT[date.psych_full.start %between% list(date.start,date.end) |
  #       date.psych_full.end %between% list(date.start,date.end) |
  #       (date.psych_full.start <= date.start & date.psych_full.end >= date.end)][pragmaid == "39MZVTUfc5"]
  DAT[date.psych_full.start %between% list(date.start,date.end) |
         date.psych_full.end %between% list(date.start,date.end) |
         (date.psych_full.start <= date.start & date.psych_full.end >= date.end), date.int2 := T]
  
  # PHARMA
  DAT[date.medi %between% list(date.start,date.end)]
  DAT[date.medi %between% list(date.start,date.end), date.int3 := T]
  
  #  INPAT-STANDARD
  #DAT[date.inpat.start %between% list(date.start,date.end) |
  #       date.inpat.end %between% list(date.start,date.end) |
  #       (date.inpat.start <= date.start & date.inpat.end >= date.end)][pragmaid == "zuXfE4s1LB"]
  DAT[date.inpat.start %between% list(date.start,date.end) |
         date.inpat.end %between% list(date.start,date.end) |
         (date.inpat.start <= date.start & date.inpat.end >= date.end), date.int4 := T]
  
  #  INPAT-INTENSE
  #DAT[date.qwt.start %between% list(date.start,date.end) |
  #       date.qwt.end %between% list(date.start,date.end) |
  #       (date.qwt.start <= date.start & date.qwt.end >= date.end)][pragmaid == "zuXfE4s1LB"]
  DAT[date.qwt.start %between% list(date.start,date.end) |
         date.qwt.end %between% list(date.start,date.end) |
         (date.qwt.start <= date.start & date.qwt.end >= date.end), date.int5 := T]
  
  ##  REHAB
  #DAT[date.reha.start %between% list(date.start,date.end) |
  #       date.reha.end %between% list(date.start,date.end) |
  #       (date.reha.start <= date.start & date.reha.end >= date.end)][pragmaid == "zuXfE4s1LB"]
  DAT[reha.typ %like% "ambulant" & (date.reha.start %between% list(date.start,date.end) |
                                      date.reha.end %between% list(date.start,date.end) |
                                      (date.reha.start <= date.start & date.reha.end >= date.end)), date.int6.outp := T]
  DAT[reha.typ %like% "stationär" & (date.reha.start %between% list(date.start,date.end) |
                                      date.reha.end %between% list(date.start,date.end) |
                                      (date.reha.start <= date.start & date.reha.end >= date.end)), date.int6.inp := T]
  
  # GET date.in = person was in ANY treatment in that period
  DAT$date.in <- F
  DAT[, date.in := rowSums(.SD, na.rm = TRUE) > 0, 
      .SDcols = startsWith(names(DAT), "date.int")]
  
  # GET date.in_sens = person was in ANY treatment except REHA in that period
  DAT$date.in_sens <- F
  DAT[, date.in_sens := rowSums(.SD, na.rm = TRUE) > 0, 
      .SDcols = names(DAT) %like% "date.int[1-5]"]
  
  ##  replace original data file
  if(i == 1){
    dat.y <- DAT
    print("dat.y done")
  }
  if(i == 2){
    dat.q <- DAT
    print("dat.q done")
  }
  if(i == 3){
    dat.m <- DAT
    print("dat.m done")
  }
  if(i == 4){
    dat.w <- DAT
    print("dat.w done")
  }
  ##  add 1 to i and continue with loop
  i <- i+1
  
}
rm(dat.list)
gc()
unique(dat.y[,.(pragmaid,date.in)])[, table(date.in)] # 5671 with any intervention at any time
unique(dat.q[,.(pragmaid,date.in)])[, table(date.in)] # 5671 with any intervention at any time
unique(dat.m[,.(pragmaid,date.in)])[, table(date.in)] # 5671 with any intervention at any time
unique(dat.w[,.(pragmaid,date.in)])[, table(date.in)] # 5671 with any intervention at any time
unique(dat.w[,.(pragmaid,date.in_sens)])[, table(date.in_sens)] # 5421 with any intervention at any time
length(unique(dat.y$pragmaid)) # 25412
length(unique(dat.q$pragmaid)) # 25412
length(unique(dat.m$pragmaid)) # 25412
length(unique(dat.w$pragmaid)) # 25412

unique(dat.q[,.(pragmaid,source)])[, table(source)] # all PRAGMA people 
unique(dat.q[date.in == T,.(pragmaid,source)])[, table(source)] # all PRAGMA people with at least one intervention


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 6) PREPARE AGGREGATED DATA
# ______________________________________________________________________________________________________________________
# ______________________________________________________________________________________________________________________
# ______________________________________________________________________________________________________________________

# PRIMARY: aggregate across all interventions
# ..............

agg.y <- dat.y[date.in == T,.(date.start = unique(date.start),
                              primary = length(unique(pragmaid))), by = .(year)][order(date.start)]
agg.q <- dat.q[date.in == T,.(date.start = unique(date.start),
                              primary = length(unique(pragmaid))), by = .(year,quarter)][order(date.start)]
agg.m <- dat.m[date.in == T,.(date.start = unique(date.start),
                              primary = length(unique(pragmaid))), by = .(year,month)][order(date.start)]
agg.w <- dat.w[date.in == T,.(date.start = unique(date.start), n_days = unique(n_days),
                              primary = length(unique(pragmaid))), by = .(time,year,week)][order(date.start)]

# SECONDARY: aggregate across OUTPATIENT/INPATIENT
# ..............

agg.w_outp <- dat.w[date.int1 == T | date.int2 == T | date.int3 == T | date.int6.outp == T,
               .(date.start = unique(date.start), n_days = unique(n_days),
                 secondary_outp = length(unique(pragmaid))), by = .(time,year,week)][order(date.start)]
agg.w_inp  <- dat.w[date.int4 == T | date.int5 == T | date.int6.inp == T,
               .(date.start = unique(date.start), n_days = unique(n_days),
                 secondary_inp = length(unique(pragmaid))), by = .(time,year,week)][order(date.start)]

# test:
agg.w[time == 10]$primary # 226 in any treatment
agg.w_outp[time == 10]$secondary_outp # 136 in outpatient treatment
agg.w_inp[time == 10]$secondary_inp # 91 in outpatient treatment

dat.w[time == 10 & date.in == T, length(unique(pragmaid))] # 226
dat.w[time == 10 & (date.int1 == T | date.int2 == T | date.int3 == T | date.int6.outp == T), length(unique(pragmaid))] # 136
dat.w[time == 10 & (date.int4 == T | date.int5 == T | date.int6.inp == T), length(unique(pragmaid))] # 91

# combine
data <- merge(agg.w,
              agg.w_outp[,.(date.start,secondary_outp)],
              by = c("date.start"))
data <- merge(data,
              agg.w_inp[,.(date.start,secondary_inp)],
              by = c("date.start"))


# SENSITIVITY: aggregations without rehab
# ..............

sens_agg.w <- dat.w[date.in_sens == T,.(date.start = unique(date.start), n_days = unique(n_days),
                                        primary = length(unique(pragmaid))), by = .(time,year,week)][order(date.start)]
sens_agg.w_outp <- dat.w[date.int1 == T | date.int2 == T | date.int3 == T,
                         .(date.start = unique(date.start), n_days = unique(n_days),
                           secondary_outp = length(unique(pragmaid))), by = .(time,year,week)][order(date.start)]
sens_agg.w_inp  <- dat.w[date.int4 == T | date.int5 == T,
                         .(date.start = unique(date.start), n_days = unique(n_days),
                           secondary_inp = length(unique(pragmaid))), by = .(time,year,week)][order(date.start)]

# test:
sens_agg.w[time == 10]$primary # 160 in any treatment
sens_agg.w_outp[time == 10]$secondary_outp # 103 in outpatient treatment
sens_agg.w_inp[time == 10]$secondary_inp # 58 in inpatient treatment

dat.w[time == 10 & date.in_sens == T, length(unique(pragmaid))] # 160
dat.w[time == 10 & (date.int1 == T | date.int2 == T | date.int3 == T), length(unique(pragmaid))] # 103
dat.w[time == 10 & (date.int4 == T | date.int5 == T), length(unique(pragmaid))] # 58

# combine
data_sens <- merge(sens_agg.w,
              sens_agg.w_outp[,.(date.start,secondary_outp)],
              by = c("date.start"))
data_sens <- merge(data_sens,
              sens_agg.w_inp[,.(date.start,secondary_inp)],
              by = c("date.start"))



# aggregate weekly for each intervention (for supplement)
# ..............

# PSYCH-BRIEF
agg.w.i1 <- dat.w[date.int1 == T,.(int = "PSYCH-BRIEF",
                                   date.start = unique(date.start),
                                   n = length(unique(pragmaid))), by = .(year,week)][order(date.start)]

# PSYCH-LONG
agg.w.i2 <- dat.w[date.int2 == T,.(int = "PSYCH-LONG",
                                   date.start = unique(date.start),
                                   n = length(unique(pragmaid))), by = .(year,week)][order(date.start)]

# PHARMA
agg.w.i3 <- dat.w[date.int3 == T,.(int = "PHARMA",
                                   date.start = unique(date.start),
                                   n = length(unique(pragmaid))), by = .(year,week)][order(date.start)]

#  INPAT-STANDARD
agg.w.i4 <- dat.w[date.int4 == T,.(int = "INPAT-STANDARD",
                                   date.start = unique(date.start),
                                   n = length(unique(pragmaid))), by = .(year,week)][order(date.start)]

#  INPAT-INTENSE
agg.w.i5 <- dat.w[date.int5 == T,.(int = "INPAT-INTENSE",
                                   date.start = unique(date.start),
                                   n = length(unique(pragmaid))), by = .(year,week)][order(date.start)]

##  REHAB-OUTPATIENT
agg.w.i6.outp <- dat.w[date.int6.outp == T,.(int = "REHAB-OUTPATIENT",#
                                        date.start = unique(date.start),
                                        n = length(unique(pragmaid))), by = .(year,week)][order(date.start)]
agg.w.i6.inp <- dat.w[date.int6.inp == T,.(int = "REHAB-INPATIENT",#
                                            date.start = unique(date.start),
                                            n = length(unique(pragmaid))), by = .(year,week)][order(date.start)]

##  COMBINE
agg.w.int <- rbind(agg.w.i1,agg.w.i2,agg.w.i3,agg.w.i6.outp,agg.w.i6.inp,agg.w.i4,agg.w.i5)
agg.w.int$int <- factor(agg.w.int$int,
                        c("PSYCH-BRIEF","PSYCH-LONG","PHARMA","REHAB-OUTPATIENT",
                          "REHAB-INPATIENT","INPAT-STANDARD","INPAT-INTENSE"))
rm(agg.w.i1,agg.w.i2,agg.w.i3,agg.w.i4,agg.w.i5,agg.w.i6.outp,agg.w.i6.inp)




# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 7) ADD PANDEMIC PERIODS TO WEEKLY DATA AND DEFINE LEVEL/SLOPE PARAMETERS FOR ITS
# ______________________________________________________________________________________________________________________
# ______________________________________________________________________________________________________________________
# ______________________________________________________________________________________________________________________

##  periods = from oxford stringency data / manually defined
add.period <- period.dat[year(date) != 2022,.(
  date = date, week = isoweek(date), weekday = weekdays(date), con_day = seq(1:.N),
  stringency, cases, deaths)]

add.period <- merge(add.period, 
                    period.input2[Entity == "Germany",.(date = Day,period = period_week)], 
                    by = "date", all.x = T)

##  prepare format
add.period <- add.period[,.(date.start = min(date),year = year(min(date)),
                            string_avg = mean(stringency),string_min = min(stringency),string_max = max(stringency),
                            cases = sum(cases), deaths = sum(deaths)), by = .(week,period)]

add.period[(year == 2020 & month(date.start) ==12)|(year == 2021 & month(date.start) == 1)] # looks correct

### weeks with 2 periods - should be none:
add.period[,.(length(unique(date.start))), by = .(year,week)][V1 != 1]

##  combine
data <- merge(data, add.period, by = c("year","week","date.start"), all.x = T)
data[year < 2020, ':=' (period = 1,
                         string_avg = 0,
                         string_min = 0,
                         string_max = 0,
                         cases = 0,
                         deaths = 0)]
data[!complete.cases(data)] # none

data_sens <- merge(data_sens, add.period, by = c("year","week","date.start"), all.x = T)
data_sens[year < 2020, ':=' (period = 1,
                        string_avg = 0,
                        string_min = 0,
                        string_max = 0,
                        cases = 0,
                        deaths = 0)]
data_sens[!complete.cases(data_sens)] # none

##  check
data[(year == 2020 & week >= 50)|(year == 2021 & week <= 5)] # transition 20/21 looks correct
data_sens[(year == 2020 & week >= 50)|(year == 2021 & week <= 5)] # transition 20/21 looks correct


##  LEVEL/SLOPE
##  reparamtrization of segmented ITS to account for cumulative effects of periods beyond the concerning period; 
##  see https://bookdown.org/mike/data_analysis/sec-interrupted-time-series.html

##  period2 (lockdown1)
w1 <- data[period == 2, min(time)]
w2 <- data[period == 2, max(time)]
data[, lockdown1 := ifelse(time < w1 | time > w2, 0, 1)]
data[, time_since_lockdown1 := ifelse(time >= w1, time - w1 + 1, 0)]
data_sens[, lockdown1 := ifelse(time < w1 | time > w2, 0, 1)]
data_sens[, time_since_lockdown1 := ifelse(time >= w1, time - w1 + 1, 0)]

data[time %between% c(w1,w2)]
data_sens[time %between% c(w1,w2)]

## period3 (between lockdowns)
w3 <- data[period == 3, min(time)]
w4 <- data[period == 3, max(time)]
data[, between_lockdowns := ifelse(time < w3 | time > w4, 0, 1)]
data[, time_since_between_lockdowns := ifelse(time >= w3, time - w3 + 1, 0)]
data_sens[, between_lockdowns := ifelse(time < w3 | time > w4, 0, 1)]
data_sens[, time_since_between_lockdowns := ifelse(time >= w3, time - w3 + 1, 0)]

data[time %between% c(w3,w4)]
data_sens[time %between% c(w3,w4)]

## period4 (lockdown2)
w5 <- data[period == 4, min(time)]
w6 <- data[period == 4, max(time)]
data[, lockdown2 := ifelse(time < w5 | time > w6, 0, 1)]
data[, time_since_lockdown2 := ifelse(time >= w5, time - w5 + 1, 0)]
data_sens[, lockdown2 := ifelse(time < w5 | time > w6, 0, 1)]
data_sens[, time_since_lockdown2 := ifelse(time >= w5, time - w5 + 1, 0)]

data[time %between% c(w5,w6)]
data_sens[time %between% c(w5,w6)]

## period5 (after lockdowns)
w7 <- data[period == 5, min(time)]
w8 <- data[period == 5, max(time)]
data[, after_lockdowns := ifelse(time < w7 | time > w8, 0, 1)]
data[, time_since_after_lockdowns := ifelse(time >= w7, time - w7 + 1, 0)]
data_sens[, after_lockdowns := ifelse(time < w7 | time > w8, 0, 1)]
data_sens[, time_since_after_lockdowns := ifelse(time >= w7, time - w7 + 1, 0)]

data[time %between% c(w7,w8)]
data_sens[time %between% c(w7,w8)]

##  LEVEL/SLOPE
##  slope definition according to https://academic.oup.com/ije/article/50/3/1011/5937253

run <- F
if(run == T){
  
  ##  period2
  w1 <- data[period == 2, min(time)]
  w2 <- data[period == 2, max(time)]
  data[, p2.level := ifelse(time < w1 | time > w2, F, T)]
  data[, p2.slope := ifelse(time < w1+1 | time > w2, 0, time-w1)]
  data_sens[, p2.level := ifelse(time < w1 | time > w2, F, T)]
  data_sens[, p2.slope := ifelse(time < w1+1 | time > w2, 0, time-w1)]
  data[time %between% c(w1,w2)]
  data_sens[time %between% c(w1,w2)]
  
  ##  period3
  w1 <- data[period == 3, min(time)]
  w2 <- data[period == 3, max(time)]
  data[, p3.level := ifelse(time < w1 | time > w2, F, T)]
  data[, p3.slope := ifelse(time < w1+1 | time > w2, 0, time-w1)]
  data_sens[, p3.level := ifelse(time < w1 | time > w2, F, T)]
  data_sens[, p3.slope := ifelse(time < w1+1 | time > w2, 0, time-w1)]
  data[time %between% c(w1,w2)]
  data_sens[time %between% c(w1,w2)]
  
  ##  period4
  w1 <- data[period == 4, min(time)]
  w2 <- data_sens[period == 4, max(time)]
  data[, p4.level := ifelse(time < w1 | time > w2, F, T)]
  data[, p4.slope := ifelse(time < w1+1 | time > w2, 0, time-w1)]
  data_sens[, p4.level := ifelse(time < w1 | time > w2, F, T)]
  data_sens[, p4.slope := ifelse(time < w1+1 | time > w2, 0, time-w1)]
  data[time %between% c(w1,w2)]
  data_sens[time %between% c(w1,w2)]
  
  ##  period5
  w1 <- data[period == 5, min(time)]
  w2 <- data[period == 5, max(time)]
  data[, p5.level := ifelse(time < w1 | time > w2, F, T)]
  data[, p5.slope := ifelse(time < w1+1 | time > w2, 0, time-w1)]
  data_sens[, p5.level := ifelse(time < w1 | time > w2, F, T)]
  data_sens[, p5.slope := ifelse(time < w1+1 | time > w2, 0, time-w1)]
  data[time %between% c(w1,w2)]
  data_sens[time %between% c(w1,w2)]
  
  
}



# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 8) PREPARE SUMMARY DATA
# ______________________________________________________________________________________________________________________
# ______________________________________________________________________________________________________________________
# ______________________________________________________________________________________________________________________

sum.dat <- copy(id.dat[,.(pragmaid,source,gkv,sex,yob)])
add <- unique(dat.w[date.in == T,.(year,min = min(year),
                                   int.psych_short = any(date.int1 == T),
                                   int.psych_full = any(date.int2 == T),
                                   int.medi = any(date.int3 == T),
                                   int.inpat = any(date.int4 == T),
                                   int.qwt = any(date.int5 == T),
                                   int.reha_outp = any(date.int6.outp == T),
                                   int.reha_inp = any(date.int6.inp == T)),
                    by = .(pragmaid,source,gkv)])
add <- unique(add[year == min,])
sum.dat <- merge(add, sum.dat, by = c("pragmaid","source","gkv"), all.x = T)



# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 9) SAFE OUTPUT FILES
# ______________________________________________________________________________________________________________________

saveRDS(sum.dat, paste0("data/output/", Sys.Date(), "_summary data.RDS"))
saveRDS(data, paste0("data/output/", Sys.Date(), "_main data.RDS"))
saveRDS(data_sens, paste0("data/output/", Sys.Date(), "_sensitivity data.RDS"))
#saveRDS(agg.w.int, paste0("data/output/", Sys.Date(), "_weekly count_intervention.RDS"))
saveRDS(period.dat, paste0("data/output/", Sys.Date(), "_period and stringency data.RDS"))
saveRDS(agg.y, paste0("data/output/", Sys.Date(), "_year count.RDS"))
saveRDS(agg.q, paste0("data/output/", Sys.Date(), "_quarter count.RDS"))
saveRDS(agg.m, paste0("data/output/", Sys.Date(), "_month count.RDS"))



