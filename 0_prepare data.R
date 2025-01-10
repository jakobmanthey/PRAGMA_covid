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
period.input <- data.table(openxlsx::read.xlsx("data/input/covid-19-stringency-index.xlsx", detectDates = T))




# Insurance period
#filename <- paste0("data/input/1_data_insurance periods_", DATE,".rds")
#ins.dat <- readRDS(filename)

# Employment period
#filename <- paste0("data/input/1_data_employment periods_", DATE,".rds")
#emp.dat <- readRDS(filename)

# Alcohol diagnoses
#filename <- paste0("data/input/1_data_alcohol diagnoses_", DATE,".rds")
#alc.diag.dat <- readRDS(filename)
#rm(filename)

# All diagnoses
#filename <- paste0("data/input/1_data_all diagnoses_",DATE,".rds")
#diag.dat <- readRDS(filename)
#rm(filename)


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
empty[, ':=' (con_week = rleid(week))]
empty[1:50]

empty[year == 2020 & month == 3]
empty[(year == 2020 & month == 12)|(year == 2021 & month == 1)] # looks correct

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 2) PREPARE AGGREGATED DATA - QUARTERLY
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
               interv.dat.6[,.(pragmaid,date.reha.start,date.reha.end)],
               by = c("pragmaid"), all.x = T, allow.cartesian = T)

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 3) PREPARE AGGREGATED DATA - MONTHLY
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
               interv.dat.6[,.(pragmaid,date.reha.start,date.reha.end)],
               by = c("pragmaid"), all.x = T, allow.cartesian = T)


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 4) PREPARE AGGREGATED DATA - WEEKLY
# ______________________________________________________________________________________________________________________
# ______________________________________________________________________________________________________________________
# ______________________________________________________________________________________________________________________

# prepare weekly data for each ID
# ..............

dat.w <- unique(empty[,.(date.start = min(date),
                         date.end = max(date),
                         n_days = length(unique(date))), by = .(con_week)])
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
               interv.dat.6[,.(pragmaid,date.reha.start,date.reha.end)],
               by = c("pragmaid"), all.x = T, allow.cartesian = T)
gc()


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 5) DETERMINE OVERLAPS IN AGGREGATED DATA
# ______________________________________________________________________________________________________________________
# ______________________________________________________________________________________________________________________
# ______________________________________________________________________________________________________________________

dat.list <- list(dat.q,dat.m,dat.w)
i <- 1
for(DAT in dat.list){
  
  # GET date.int# = person was in treatment in that period
  DAT$date.int1 <- F
  DAT$date.int2 <- F
  DAT$date.int3 <- F
  DAT$date.int4 <- F
  DAT$date.int5 <- F
  DAT$date.int6 <- F
  
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
  DAT[date.reha.start %between% list(date.start,date.end) |
         date.reha.end %between% list(date.start,date.end) |
         (date.reha.start <= date.start & date.reha.end >= date.end), date.int6 := T]
  
  # GET date.in = person was in ANY treatment in that period
  DAT$date.in <- F
  DAT[, date.in := rowSums(.SD, na.rm = TRUE) > 0, 
      .SDcols = startsWith(names(DAT), "date.int")]
  
  ##  replace original data file
  if(i == 1){
    dat.q <- DAT
    print("dat.q done")
  }
  if(i == 2){
    dat.m <- DAT
    print("dat.m done")
  }
  if(i == 3){
    dat.w <- DAT
    print("dat.w done")
  }
  ##  add 1 to i and continue with loop
  i <- i+1
  
}
rm(dat.list)
gc()
unique(dat.q[,.(pragmaid,date.in)])[, table(date.in)] # 5671 with any intervention at any time
unique(dat.m[,.(pragmaid,date.in)])[, table(date.in)] # 5671 with any intervention at any time
unique(dat.w[,.(pragmaid,date.in)])[, table(date.in)] # 5671 with any intervention at any time
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

# aggregate across all interventions
# ..............

agg.q <- dat.q[date.in == T,.(date.start = unique(date.start),
                              n = length(unique(pragmaid))), by = .(year,quarter)][order(date.start)]
agg.m <- dat.m[date.in == T,.(date.start = unique(date.start),
                              n = length(unique(pragmaid))), by = .(year,month)][order(date.start)]
agg.w <- dat.w[date.in == T,.(date.start = unique(date.start), n_days = unique(n_days),
                              n = length(unique(pragmaid))), by = .(con_week,year,week)][order(date.start)]
agg.w[n==0]


# aggregate for each intervention
# ..............

# PSYCH-BRIEF
agg.q.i1 <- dat.q[date.int1 == T,.(int = "PSYCH-BRIEF",
                                   date.start = unique(date.start),
                                   n = length(unique(pragmaid))), by = .(year,quarter)][order(date.start)]
agg.m.i1 <- dat.m[date.int1 == T,.(int = "PSYCH-BRIEF",
                                   date.start = unique(date.start),
                                   n = length(unique(pragmaid))), by = .(year,month)][order(date.start)]
agg.w.i1 <- dat.w[date.int1 == T,.(int = "PSYCH-BRIEF",
                                   date.start = unique(date.start),
                                   n = length(unique(pragmaid))), by = .(year,week)][order(date.start)]

# PSYCH-LONG
agg.q.i2 <- dat.q[date.int2 == T,.(int = "PSYCH-LONG",
                                   date.start = unique(date.start),
                                   n = length(unique(pragmaid))), by = .(year,quarter)][order(date.start)]
agg.m.i2 <- dat.m[date.int2 == T,.(int = "PSYCH-LONG",
                                   date.start = unique(date.start),
                                   n = length(unique(pragmaid))), by = .(year,month)][order(date.start)]
agg.w.i2 <- dat.w[date.int2 == T,.(int = "PSYCH-LONG",
                                   date.start = unique(date.start),
                                   n = length(unique(pragmaid))), by = .(year,week)][order(date.start)]

# PHARMA
agg.q.i3 <- dat.q[date.int3 == T,.(int = "PHARMA",
                                   date.start = unique(date.start),
                                   n = length(unique(pragmaid))), by = .(year,quarter)][order(date.start)]
agg.m.i3 <- dat.m[date.int3 == T,.(int = "PHARMA",
                                   date.start = unique(date.start),
                                   n = length(unique(pragmaid))), by = .(year,month)][order(date.start)]
agg.w.i3 <- dat.w[date.int3 == T,.(int = "PHARMA",
                                   date.start = unique(date.start),
                                   n = length(unique(pragmaid))), by = .(year,week)][order(date.start)]

#  INPAT-STANDARD
agg.q.i4 <- dat.q[date.int4 == T,.(int = "INPAT-STANDARD",
                                   date.start = unique(date.start),
                                   n = length(unique(pragmaid))), by = .(year,quarter)][order(date.start)]
agg.m.i4 <- dat.m[date.int4 == T,.(int = "INPAT-STANDARD",
                                   date.start = unique(date.start),
                                   n = length(unique(pragmaid))), by = .(year,month)][order(date.start)]
agg.w.i4 <- dat.w[date.int4 == T,.(int = "INPAT-STANDARD",
                                   date.start = unique(date.start),
                                   n = length(unique(pragmaid))), by = .(year,week)][order(date.start)]

#  INPAT-INTENSE
agg.q.i5 <- dat.q[date.int5 == T,.(int = "INPAT-INTENSE",
                                   date.start = unique(date.start),
                                   n = length(unique(pragmaid))), by = .(year,quarter)][order(date.start)]
agg.m.i5 <- dat.m[date.int5 == T,.(int = "INPAT-INTENSE",
                                   date.start = unique(date.start),
                                   n = length(unique(pragmaid))), by = .(year,month)][order(date.start)]
agg.w.i5 <- dat.w[date.int5 == T,.(int = "INPAT-INTENSE",
                                   date.start = unique(date.start),
                                   n = length(unique(pragmaid))), by = .(year,week)][order(date.start)]

##  REHAB
agg.q.i6 <- dat.q[date.int6 == T,.(int = "REHAB",
                                   date.start = unique(date.start),
                                   n = length(unique(pragmaid))), by = .(year,quarter)][order(date.start)]
agg.m.i6 <- dat.m[date.int6 == T,.(int = "REHAB",
                                   date.start = unique(date.start),
                                   n = length(unique(pragmaid))), by = .(year,month)][order(date.start)]
agg.w.i6 <- dat.w[date.int6 == T,.(int = "REHAB",
                                   date.start = unique(date.start),
                                   n = length(unique(pragmaid))), by = .(year,week)][order(date.start)]

##  COMBINE
agg.q.int <- rbind(agg.q.i1,agg.q.i2,agg.q.i3,agg.q.i4,agg.q.i5,agg.q.i6)
agg.q.int$int <- factor(agg.q.int$int,
                        c("PSYCH-BRIEF","PSYCH-LONG",
                          "PHARMA","INPAT-STANDARD",
                          "INPAT-INTENSE","REHAB"))
rm(agg.q.i1,agg.q.i2,agg.q.i3,agg.q.i4,agg.q.i5,agg.q.i6)

agg.m.int <- rbind(agg.m.i1,agg.m.i2,agg.m.i3,agg.m.i4,agg.m.i5,agg.m.i6)
agg.m.int$int <- factor(agg.m.int$int,
                        c("PSYCH-BRIEF","PSYCH-LONG",
                          "PHARMA","INPAT-STANDARD",
                          "INPAT-INTENSE","REHAB"))
rm(agg.m.i1,agg.m.i2,agg.m.i3,agg.m.i4,agg.m.i5,agg.m.i6)

agg.w.int <- rbind(agg.w.i1,agg.w.i2,agg.w.i3,agg.w.i4,agg.w.i5,agg.w.i6)
agg.w.int$int <- factor(agg.w.int$int,
                        c("PSYCH-BRIEF","PSYCH-LONG",
                          "PHARMA","INPAT-STANDARD",
                          "INPAT-INTENSE","REHAB"))
rm(agg.w.i1,agg.w.i2,agg.w.i3,agg.w.i4,agg.w.i5,agg.w.i6)




# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 7) ADD PANDEMIC PERIODS TO WEEKLY DATA AND DEFINE LEVEL/SLOPE PARAMETERS FOR ITS
# ______________________________________________________________________________________________________________________
# ______________________________________________________________________________________________________________________
# ______________________________________________________________________________________________________________________

##  periods = from oxford stringency data
period.dat <- copy(period.input)
period.dat <- period.dat[Entity == "Germany" & year(Day) != 2022,.(
  date = Day, week = isoweek(Day), weekday = weekdays(Day), period = period_week, con_day = seq(1:.N))]
period.dat <- period.dat[,.(date.start = min(date)), by = .(week,period)]
period.dat[, year := year(date.start)]
period.dat[(year == 2020 & month(date.start) ==12)|(year == 2021 & month(date.start) == 1)] # looks correct

### weeks with 2 periods - should be none:
period.dat[,.(length(unique(date.start))), by = .(year,week)][V1 != 1]

##  combine
agg.w <- merge(agg.w, period.dat, by = c("year","week","date.start"), all.x = T)
agg.w[year < 2020, period := 1]

##  check
agg.w[(year == 2020 & week >= 50)|(year == 2021 & week <= 5)] # transition 20/21 looks correct


##  LEVEL/SLOPE
##  slope definition according to https://academic.oup.com/ije/article/50/3/1011/5937253

##  period2
w1 <- agg.w[period == 2, min(con_week)]
w2 <- agg.w[period == 2, max(con_week)]
agg.w[, p2.level := ifelse(con_week < w1 | con_week > w2, F, T)]
agg.w[, p2.slope := ifelse(con_week < w1+1 | con_week > w2, 0, con_week-w1)]
agg.w[con_week %between% c(w1,w2)]

##  period3
w1 <- agg.w[period == 3, min(con_week)]
w2 <- agg.w[period == 3, max(con_week)]
agg.w[, p3.level := ifelse(con_week < w1 | con_week > w2, F, T)]
agg.w[, p3.slope := ifelse(con_week < w1+1 | con_week > w2, 0, con_week-w1)]
agg.w[con_week %between% c(w1,w2)]

##  period4
w1 <- agg.w[period == 4, min(con_week)]
w2 <- agg.w[period == 4, max(con_week)]
agg.w[, p4.level := ifelse(con_week < w1 | con_week > w2, F, T)]
agg.w[, p4.slope := ifelse(con_week < w1+1 | con_week > w2, 0, con_week-w1)]
agg.w[con_week %between% c(w1,w2)]

##  period5
w1 <- agg.w[period == 5, min(con_week)]
w2 <- agg.w[period == 5, max(con_week)]
agg.w[, p5.level := ifelse(con_week < w1 | con_week > w2, F, T)]
agg.w[, p5.slope := ifelse(con_week < w1+1 | con_week > w2, 0, con_week-w1)]
agg.w[con_week %between% c(w1,w2)]



# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 8) PREPARE SUMMARY DATA
# ______________________________________________________________________________________________________________________
# ______________________________________________________________________________________________________________________
# ______________________________________________________________________________________________________________________

sum.dat <- copy(id.dat[,.(pragmaid,source,gkv,sex,yob)])
add <- unique(dat.w[date.in == T,.(pragmaid,source,gkv,year)])
add[, min := min(year), by = .(pragmaid,source,gkv)]
add <- unique(add[year == min,.(pragmaid,source,gkv)])
sum.dat <- merge(add, sum.dat, by = c("pragmaid","source","gkv"), all.x = T)


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 4) PRELIMINARY PLOTS
# ______________________________________________________________________________________________________________________

theme_set( theme_gdocs() )

# X) any intervention
# ..............

##  quarterly
pdat <- copy(agg.q)
pdat[, christmas := month(date.start) == 10]

ggplot(pdat, aes(x = date.start, y = n)) + 
  ggtitle("Number of people in any alcohol-specific treatment - by quarter",
          "Dashed vertical lines (lockdowns): 22 March 2020-4 May 2020 | 2 Nov 2020-17 Aug 2021") +
  geom_vline(xintercept = c(as.Date("2020-03-20"),as.Date("2020-05-04")), linetype = 2) +
  geom_vline(xintercept = c(as.Date("2020-11-02"),as.Date("2021-08-17")), linetype = 2) +
  geom_line(linewidth = 0.8) +
  geom_point(aes(fill = christmas), shape = 22, size = 2) + 
  scale_fill_manual(values = c("black","red")) +
  scale_x_date("", breaks = "6 months", date_labels = "%y-%B") +
  scale_y_continuous("N")

ggsave(filename = paste0("figs/", Sys.Date(), "_fig_x_time series quarterly.png"),
       width = 14, height = 8)

##  monthly
pdat <- copy(agg.m)
pdat[, christmas := month(date.start) == 12]

ggplot(pdat, aes(x = date.start, y = n)) + 
  ggtitle("Number of people in any alcohol-specific treatment - by month",
          "Dashed vertical lines (lockdowns): 22 March 2020-4 May 2020 | 2 Nov 2020-17 Aug 2021") +
  geom_vline(xintercept = c(as.Date("2020-03-20"),as.Date("2020-05-04")), linetype = 2) +
  geom_vline(xintercept = c(as.Date("2020-11-02"),as.Date("2021-08-17")), linetype = 2) +
  geom_line(linewidth = 0.8) +
  geom_point(aes(fill = christmas), shape = 22, size = 2) + 
  scale_fill_manual(values = c("black","red")) +
  scale_x_date("", breaks = "6 months", date_labels = "%y-%B") +
  scale_y_continuous("N")

ggsave(filename = paste0("figs/", Sys.Date(), "_fig_x_time series monthly.png"),
       width = 14, height = 8)

##  weekly
pdat <- copy(agg.w)
pdat[, christmas := month(date.start) == 12]

ggplot(pdat, aes(x = date.start, y = n)) + 
  ggtitle("Number of people in any alcohol-specific treatment - by week",
          "Dashed vertical lines (lockdowns): 22 March 2020-4 May 2020 | 2 Nov 2020-17 Aug 2021") +
  geom_vline(xintercept = c(as.Date("2020-03-20"),as.Date("2020-05-04")), linetype = 2) +
  geom_vline(xintercept = c(as.Date("2020-11-02"),as.Date("2021-08-17")), linetype = 2) +
  geom_line(linewidth = 0.8) +
  geom_point(aes(fill = christmas), shape = 22, size = 2) + 
  scale_fill_manual(values = c("black","red")) +
  scale_x_date("", breaks = "6 months", date_labels = "%y-%B") +
  scale_y_continuous("N")

ggsave(filename = paste0("figs/", Sys.Date(), "_fig_x_time series weekly.png"),
       width = 14, height = 8)


# Y) by intervention
# ..............

six_colors <- c("#FF7F0E", "#2CA02C", "#1F77B4", "#9467BD", "#8C564B", "#E377C2")

##  quarterly
pdat <- copy(agg.q.int)
pdat[, christmas := month(date.start) == 10]

ggplot(pdat, aes(x = date.start, y = n, color = int)) + 
  ggtitle("Number of people in different alcohol-specific treatments - by quarter",
          "Dashed vertical lines (lockdowns): 22 March 2020-4 May 2020 | 2 Nov 2020-17 Aug 2021") +
  geom_vline(xintercept = c(as.Date("2020-03-20"),as.Date("2020-05-04")), linetype = 2) +
  geom_vline(xintercept = c(as.Date("2020-11-02"),as.Date("2021-08-17")), linetype = 2) +
  geom_line(linewidth = 1.0) +
  geom_point(aes(fill = christmas), shape = 22, size = 2) + 
  scale_fill_manual(values = c("black","red")) +
  scale_color_viridis_d("interventions") +
  scale_x_date("", breaks = "6 months", date_labels = "%y-%B") +
  scale_y_continuous("N") + 
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))

ggsave(filename = paste0("figs/", Sys.Date(), "_fig_y_time series quarterly by intervention.png"),
       width = 12, height = 6)

##  monthly
pdat <- copy(agg.m.int)
pdat[, christmas := month(date.start) == 12]

ggplot(pdat, aes(x = date.start, y = n, color = int)) + 
  ggtitle("Number of people in different alcohol-specific treatments - by month",
          "Dashed vertical lines (lockdowns): 22 March 2020-4 May 2020 | 2 Nov 2020-17 Aug 2021") +
  geom_vline(xintercept = c(as.Date("2020-03-20"),as.Date("2020-05-04")), linetype = 2) +
  geom_vline(xintercept = c(as.Date("2020-11-02"),as.Date("2021-08-17")), linetype = 2) +
  geom_line(linewidth = 1.0) +
  geom_point(aes(fill = christmas), shape = 22, size = 2) + 
  scale_fill_manual(values = c("black","red")) +
  scale_color_viridis_d("interventions") +
  scale_x_date("", breaks = "6 months", date_labels = "%y-%B") +
  scale_y_continuous("N") + 
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))

ggsave(filename = paste0("figs/", Sys.Date(), "_fig_y_time series monthly by intervention.png"),
       width = 12, height = 6)

##  weekly
pdat <- copy(agg.w.int)
pdat[, christmas := month(date.start) == 12]

ggplot(pdat, aes(x = date.start, y = n, color = int)) + 
  ggtitle("Number of people in different alcohol-specific treatments - by week",
          "Dashed vertical lines (lockdowns): 22 March 2020-4 May 2020 | 2 Nov 2020-17 Aug 2021") +
  geom_vline(xintercept = c(as.Date("2020-03-20"),as.Date("2020-05-04")), linetype = 2) +
  geom_vline(xintercept = c(as.Date("2020-11-02"),as.Date("2021-08-17")), linetype = 2) +
  geom_line(linewidth = 1.0) +
  geom_point(aes(fill = christmas), shape = 22, size = 2) + 
  scale_fill_manual(values = c("black","red")) +
  scale_color_viridis_d("interventions") +
  scale_x_date("", breaks = "6 months", date_labels = "%y-%B") +
  scale_y_continuous("N") + 
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))

ggsave(filename = paste0("figs/", Sys.Date(), "_fig_y_time series weekly by intervention.png"),
       width = 12, height = 6)


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 9) SAFE OUTPUT FILES
# ______________________________________________________________________________________________________________________

saveRDS(sum.dat, paste0("data/output/", Sys.Date(), "summary data.RDS"))
saveRDS(agg.w, paste0("data/output/", Sys.Date(), "_weekly count.RDS"))
saveRDS(agg.w.int, paste0("data/output/", Sys.Date(), "_weekly count_intervention.RDS"))


