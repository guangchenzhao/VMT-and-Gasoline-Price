library(data.table)
library(bit64)
library(tidyverse)
library(lme4)
library(mgcv)
library(AICcmodavg)
library(car)
library(gplots)
library(plm)

#### individual level
device_acs_trip_gas_total = data.table()
for(month_i in 5:12){
  device_acs_trip_gas = fread(paste0(
    "/device_trip_acs_gas_2021",
                                     formatC(month_i, width = 2, format = "d", flag = "0"),"weekdays.csv"))
  device_acs_trip_gas$month = formatC(month_i, width = 2, format = "d", flag = "0")
  device_acs_trip_gas$year = '2021'
  device_acs_trip_gas_total=rbind(device_acs_trip_gas_total, device_acs_trip_gas)
}
 
################################## processing for regression
device_acs_trip_gas_total = unique(device_acs_trip_gas_total)
#weekorder
week_order_device = as.data.table(unique(device_acs_trip_gas_total$weekofyear)) 
week_order_device = week_order_device[order(week_order_device$V1),]
week_order_device$week_order = seq.int(nrow(week_order_device))
colnames(week_order_device) = c('weekofyear', 'week_order')
device_acs_trip_gas_total = merge(device_acs_trip_gas_total, week_order_device, by=c("weekofyear"))

#drivemileage_prev
device_acs_trip_gas_total$week_next = device_acs_trip_gas_total$week_order +1
device_acs_trip_gas_total_2 = device_acs_trip_gas_total[,c("week_next","device_id", "drivemileage", "h_gs_avg_price_5mi")]
colnames(device_acs_trip_gas_total_2) = c("week_order","device_id", "drivemileage_prev", "h_gs_avg_price_5mi_prev")
device_acs_trip_gas_total = merge(device_acs_trip_gas_total, device_acs_trip_gas_total_2, 
                     by=c("week_order","device_id"), all.x=TRUE)
         
#numofweeks
device_numofweeks = device_acs_trip_gas_total[ , .(numofweeks = .N), by = .(device_id)] 
numdevice_numofweeks=as.data.table(c(nrow(device_numofweeks[numofweeks>=1]),
     nrow(device_numofweeks[numofweeks>=2]),
     nrow(device_numofweeks[numofweeks>=3]),
     nrow(device_numofweeks[numofweeks>=4]),
     nrow(device_numofweeks[numofweeks>=6]),
     nrow(device_numofweeks[numofweeks>=8]),
     nrow(device_numofweeks[numofweeks>=12]),
     nrow(device_numofweeks[numofweeks>=16])))
# select the devices without relocation
length(unique(device_acs_trip_gas_total$device_id))
device_numofhome = unique(device_acs_trip_gas_total[,c('device_id', 'median_HHincome', 'cbg_inc_pct_25k_50k')])[
  , .(numofhome = .N), by = .(device_id)] 
numdevice_numofhome = as.data.table(c(nrow(device_numofhome[numofhome>=1]),
  nrow(device_numofhome[numofhome>=2]),
  nrow(device_numofhome[numofhome>=3]),
  nrow(device_numofhome[numofhome>=4]),
  nrow(device_numofhome[numofhome>=5])))
device_id_list_1home = as.data.table(unique(device_numofhome[numofhome==1]$device_id))
device_id_list_4weeks = as.data.table(unique(device_numofweeks[numofweeks>=4]$device_id))
device_id_list_4weeks_1home = merge(device_id_list_4weeks, device_id_list_1home)
device_id_sample_4weeks_1home = device_id_list_4weeks_1home[
  sample(nrow(device_id_list_4weeks_1home), size=round(nrow(device_id_list_4weeks_1home)/10)), ] #10% sample
colnames(device_id_sample_4weeks_1home) = "device_id"
device_acs_trip_gas_sample_4weeks_1home = merge(device_acs_trip_gas_total, device_id_sample_4weeks_1home, by = 'device_id')
device_acs_trip_gas_sample_4weeks_1home = device_acs_trip_gas_sample_4weeks_1home[!is.na(drivemileage_prev)]
#numofgasstation avg_gasprice popdens roadlenth_dens area of land
numofgasstation = avg_gas_numoftrips_mileage_byTT[,c('TTFIPS', 'week_order', 'numofgasstation', 'RoadLength_dens', 'pop_dens', 'ALAND')]
colnames(numofgasstation) = c('h_TTFIPS', 'week_order', 'numofgasstation', 'RoadLength_dens', 'pop_dens', 'ALAND')
device_acs_trip_gas_sample_4weeks_1home$h_BGFIPS = as.character(device_acs_trip_gas_sample_4weeks_1home$h_BGFIPS)
device_acs_trip_gas_sample_4weeks_1home$h_TTFIPS = as.character(device_acs_trip_gas_sample_4weeks_1home$h_TTFIPS)
device_acs_trip_gas_sample_4weeks_1home = merge(device_acs_trip_gas_sample_4weeks_1home,
                                                numofgasstation,
                                                by = c('h_TTFIPS', 'week_order'))
device_acs_trip_gas_sample_4weeks_1home$week_order_2 = device_acs_trip_gas_sample_4weeks_1home$week_order*device_acs_trip_gas_sample_4weeks_1home$week_order

#####

gasloglm_sample_4weeks_1home = lm(
  drivemileage ~ 
    pop_dens

  + cbg_inc_pct_less_25k+cbg_inc_pct_25k_50k+cbg_inc_pct_50k_75k+cbg_inc_pct_75k_125k
  + cbg_age_pct_under_17+cbg_age_pct_18_24+cbg_age_pct_25_34+cbg_age_pct_35_54+cbg_age_pct_55_64
  + ct_pov_rate
  + ct_edu_pct_less_HS +ct_edu_pct_SCND_AD
  + ALAND 
  + RoadLength_dens 
  + h_gs_avg_price_5mi 
  +  drivemileage_prev
  +factor(month)
  +factor(year)
  ,
  data = device_acs_trip_gas_sample_4weeks_1home[(drivemileage<=200)&(drivemileage>0)])
vif(gasloglm_sample_4weeks_1home)
summary(gasloglm_sample_4weeks_1home)
gasloglm_sample_4weeks_1home$coefficients
view(t(summary(gasloglm_sample_4weeks_1home)$coefficients[,2]))

gasloglm_sample_4weeks_1home_2 = lm(
  drivemileage ~ 
    pop_dens
  + cbg_inc_pct_less_25k+cbg_inc_pct_25k_50k+cbg_inc_pct_50k_75k+cbg_inc_pct_75k_125k
  + cbg_age_pct_under_17+cbg_age_pct_18_24+cbg_age_pct_25_34+cbg_age_pct_35_54+cbg_age_pct_55_64
  + ct_pov_rate
  + ct_edu_pct_less_HS +ct_edu_pct_SCND_AD
  + ALAND 
  + RoadLength_dens 
  + h_gs_avg_price_5mi 
  +  drivemileage_prev 
  +factor(month)
  +factor(year)
  ,
  data = device_acs_trip_gas_sample_4weeks_1home[drivemileage<=200])
summary(gasloglm_sample_4weeks_1home_2)
view(t(summary(gasloglm_sample_4weeks_1home_2)$coefficients[,1]))
view(t(summary(gasloglm_sample_4weeks_1home_2)$coefficients[,2]))

################### regression with fixed effect
gasfelm_sample_4weeks_1home = plm(
  drivemileage ~ 
    pop_dens
  + cbg_inc_pct_less_25k+cbg_inc_pct_25k_50k+cbg_inc_pct_50k_75k+cbg_inc_pct_75k_125k
  + cbg_age_pct_under_17+cbg_age_pct_18_24+cbg_age_pct_25_34+cbg_age_pct_35_54+cbg_age_pct_55_64
  + ct_pov_rate
  + ct_edu_pct_less_HS +ct_edu_pct_SCND_AD
  + ALAND 
  + RoadLength_dens 
  + h_gs_avg_price_5mi 
  +  drivemileage_prev 
  +factor(month)
  +factor(year)
  ,
  index = "device_id", 
  model = "within",
  data = device_acs_trip_gas_sample_4weeks_1home[(drivemileage<=200)&(drivemileage>0)])
summary(gasfelm_sample_4weeks_1home)
gasfelm_sample_4weeks_1home$coefficients


gasfelm_sample_4weeks_1home_2 = plm(
  drivemileage ~ 
    pop_dens
  + cbg_inc_pct_less_25k+cbg_inc_pct_25k_50k+cbg_inc_pct_50k_75k+cbg_inc_pct_75k_125k
  + cbg_age_pct_under_17+cbg_age_pct_18_24+cbg_age_pct_25_34+cbg_age_pct_35_54+cbg_age_pct_55_64
  + ct_pov_rate
  + ct_edu_pct_less_HS +ct_edu_pct_SCND_AD
  + ALAND 
  + RoadLength_dens 
  + h_gs_avg_price_5mi 
  +  drivemileage_prev 
  +factor(month)
  +factor(year)
  ,
  index = "device_id", 
  model = "within",
  data = device_acs_trip_gas_sample_4weeks_1home[(drivemileage<=200)])
summary(gasfelm_sample_4weeks_1home_2)


#### aggregated level (e.g. tract level)

gas_station_price_reg_202101_202302 = fread(
  "/gas_station_price_reg_202101_202302.csv")
gas_station_price_reg_202101_202302$TTFIPS = as.character(gas_station_price_reg_202101_202302$TTFIPS)

avg_gas_byTT = gas_station_price_reg_202101_202302[ , .(numofgasstation = .N, 
                                                        avg_gasprice = mean(Price),
                                                        std_gasprice = sd(Price)),
                                                    by = .(TTFIPS, Date)]
avg_gas_byTT$Date_next = avg_gas_byTT$Date +7
avg_gas_byTT_2 = avg_gas_byTT[,c("Date_next","TTFIPS", "numofgasstation", "avg_gasprice", "std_gasprice")]
colnames(avg_gas_byTT_2) = c("Date","TTFIPS", "numofgasstation_prev", "avg_gasprice_prev", "std_gasprice_prev")
avg_gas_byTT = merge(avg_gas_byTT, avg_gas_byTT_2, 
                     by=c("Date","TTFIPS"), all.x=TRUE)

avg_numoftrips_mileage_byTT = gas_station_price_reg_202101_202302[
  ,c("Date", "TTFIPS","ALAND","CTFIPS","STFIPS","num_residents", "num_worker",
     "median_HHincome","cbg_inc_pct_less_25k","cbg_inc_pct_25k_50k","cbg_inc_pct_50k_75k",
     "cbg_inc_pct_75k_125k","cbg_inc_pct_125k_more","cbg_age_pct_under_17",
     "cbg_age_pct_18_24","cbg_age_pct_25_34", "cbg_age_pct_35_54","cbg_age_pct_55_64",
     "cbg_age_pct_65_over","ct_pov_rate","ct_edu_pct_less_HS","ct_edu_pct_SCND_AD",
     "ct_edu_pct_BD_more","pop_dens" ,"RoadLength","RoadCount","RoadLength_dens",
     "numofdevice","avg_numoftrips","avg_numofdrivetrips","avg_mileage","avg_drivemileage","avg_drivemileage_2nd",
     "total_mileage","total_mileage_2","numofdevice_prev","avg_numoftrips_prev",
     "avg_numofdrivetrips_prev","avg_mileage_prev","avg_drivemileage_prev","avg_drivemileage_2nd_prev",
     "total_mileage_prev" )]
avg_numoftrips_mileage_byTT = unique(avg_numoftrips_mileage_byTT)

avg_gas_numoftrips_mileage_byTT = merge(avg_gas_byTT, avg_numoftrips_mileage_byTT, by=c("Date","TTFIPS"))
week_order = as.data.table(unique(avg_gas_numoftrips_mileage_byTT$Date)) 
week_order = week_order[order(week_order$V1),]
week_order$week_order = seq.int(nrow(week_order))
colnames(week_order) = c('Date', 'week_order')

avg_gas_numoftrips_mileage_byTT = merge(avg_gas_numoftrips_mileage_byTT, week_order, by=c("Date"))
avg_gas_numoftrips_mileage_byTT$week_order_2 = avg_gas_numoftrips_mileage_byTT$week_order * avg_gas_numoftrips_mileage_byTT$week_order

avg_gas_numoftrips_mileage_byTT$month <- format(avg_gas_numoftrips_mileage_byTT$Date,"%m")
avg_gas_numoftrips_mileage_byTT$year <- format(avg_gas_numoftrips_mileage_byTT$Date,"%y")
avg_gas_numoftrips_mileage_byTT$year_month = format(avg_gas_numoftrips_mileage_byTT$Date,"%y%m")


TTgasdrivelm = lm(avg_drivemileage_2nd ~ 
                    pop_dens
                  + cbg_inc_pct_less_25k+cbg_inc_pct_25k_50k+cbg_inc_pct_50k_75k+cbg_inc_pct_75k_125k
                  + cbg_age_pct_under_17+cbg_age_pct_18_24+cbg_age_pct_25_34+cbg_age_pct_35_54+cbg_age_pct_55_64
                  + ct_pov_rate
                  + ct_edu_pct_less_HS +ct_edu_pct_SCND_AD
                  + avg_gasprice 
                  + RoadLength_dens  
                  +  avg_drivemileage_2nd_prev 
                  + ALAND
                  + factor(month)
                  +factor(year)
                  ,
                  weights = numofdevice,
                  data = avg_gas_numoftrips_mileage_byTT[Date<"2022-10-01"])
summary(TTgasdrivelm)
TTgasdrivelm$coefficients
vif(TTgasdrivelm)
view(t(summary(TTgasdrivelm)$coefficients[,1]))
view(t(summary(TTgasdrivelm)$coefficients[,2]))


TTgasdrivelm_TTfe = plm(avg_drivemileage_2nd ~ 
                          pop_dens
                        + cbg_inc_pct_less_25k+cbg_inc_pct_25k_50k+cbg_inc_pct_50k_75k+cbg_inc_pct_75k_125k
                        + cbg_age_pct_under_17+cbg_age_pct_18_24+cbg_age_pct_25_34+cbg_age_pct_35_54+cbg_age_pct_55_64
                        + ct_pov_rate
                        + ct_edu_pct_less_HS +ct_edu_pct_SCND_AD
                        + RoadLength + RoadLength_dens
                        +ALAND
                        + avg_gasprice
                        + avg_drivemileage_2nd_prev
                        + factor(month)
                        +factor(year)
                        ,
                        weights = numofdevice,
                        index = c("TTFIPS"), 
                        model = "within",
                        data = avg_gas_numoftrips_mileage_byTT[(Date < "2022-10-01") ])
summary(TTgasdrivelm_TTfe)

TTgasdrivelm_weekfe = plm(avg_drivemileage ~ 
                            pop_dens
                          + cbg_inc_pct_less_25k+cbg_inc_pct_25k_50k+cbg_inc_pct_50k_75k+cbg_inc_pct_75k_125k
                          + cbg_age_pct_under_17+cbg_age_pct_18_24+cbg_age_pct_25_34+cbg_age_pct_35_54+cbg_age_pct_55_64
                          + ct_pov_rate
                          + ct_edu_pct_less_HS +ct_edu_pct_SCND_AD
                          + avg_numofdrivetrips   
                          + numofgasstation + avg_gasprice 
                          + RoadLength + RoadLength_dens 
                          +  avg_drivemileage_prev + total_mileage_prev
                          +week_order
                          ,
                          weights = numofdevice,
                          index = "Date", 
                          model = "within",
                          data = avg_gas_numoftrips_mileage_byTT[Date<"2022-10-01"])
summary(TTgasdrivelm_weekfe)


########### modeling weekly gasoline price with regional VMT in the previous week

gas_station_price_reg_202101_202302 = fread(
  "/gas_station_price_reg_202101_202302.csv")
gas_station_price_reg_202101_202302$TTFIPS = as.character(gas_station_price_reg_202101_202302$TTFIPS)
week_order = as.data.table(unique(gas_station_price_reg_202101_202302$Date)) 
week_order = week_order[order(week_order$V1),]
week_order$week_order = seq.int(nrow(week_order))
colnames(week_order) = c('Date', 'week_order')

gas_station_price_reg_202101_202302 = merge(gas_station_price_reg_202101_202302, week_order, by=c("Date"))
gas_station_price_reg_202101_202302$week_order_2 = gas_station_price_reg_202101_202302$week_order * gas_station_price_reg_202101_202302$week_order

gas_station_price_reg_202101_202302$month <- format(gas_station_price_reg_202101_202302$Date,"%m")
gas_station_price_reg_202101_202302$year <- format(gas_station_price_reg_202101_202302$Date,"%y")
gas_station_price_reg_202101_202302$year_month = format(gas_station_price_reg_202101_202302$Date,"%y%m")

vif(lm(Price ~ 
         pop_dens
       + cbg_inc_pct_less_25k+cbg_inc_pct_25k_50k+cbg_inc_pct_50k_75k+cbg_inc_pct_75k_125k
       + cbg_age_pct_under_17+cbg_age_pct_18_24+cbg_age_pct_25_34+cbg_age_pct_35_54+cbg_age_pct_55_64
       + ct_pov_rate
       + ct_edu_pct_less_HS +ct_edu_pct_SCND_AD
       + num_gasstation_025mile 
       + RoadLength_dens  
       +  avg_drivemileage_prev 
       + total_mileage_prev
       +ALAND
       ,
       data = gas_station_price_reg_202101_202302))


gaspricelm_202101_202302 = lm(
  Price ~ 
    pop_dens
  + cbg_inc_pct_less_25k+cbg_inc_pct_25k_50k+cbg_inc_pct_50k_75k+cbg_inc_pct_75k_125k
  + cbg_age_pct_under_17+cbg_age_pct_18_24+cbg_age_pct_25_34+cbg_age_pct_35_54+cbg_age_pct_55_64
  + ct_pov_rate
  + ct_edu_pct_less_HS +ct_edu_pct_SCND_AD
  + num_gasstation_025mile 
  + RoadLength_dens 
  + total_1000mileage_prev 
  + ALAND
  + factor(`GasBrand`)
  + factor(month)
  +factor(year)
  +factor(STFIPS)
  ,
  data = gas_station_price_reg_202101_202302)
summary(gaspricelm_202101_202302)
gaspricelm_202101_202302$coefficients
view(t(summary(gaspricelm_202101_202302)$coefficients[,2]))

gaspricestationfelm_202101_202302 = plm(
  Price ~ 
    pop_dens
  + cbg_inc_pct_less_25k+cbg_inc_pct_25k_50k+cbg_inc_pct_50k_75k+cbg_inc_pct_75k_125k
  + cbg_age_pct_under_17+cbg_age_pct_18_24+cbg_age_pct_25_34+cbg_age_pct_35_54+cbg_age_pct_55_64
  + ct_pov_rate
  + ct_edu_pct_less_HS +ct_edu_pct_SCND_AD
  + num_gasstation_025mile 
  + RoadLength_dens 
  + total_mileage_prev 
  + ALAND
  + factor(month)
  +factor(year)
  +factor(STFIPS)
  ,
  index = "StationID", 
  model = "within",
  data = gas_station_price_reg_202101_202302[!is.na(total_mileage_prev)])
summary(gaspricestationfelm_202101_202302)
gaspricestationfelm_202101_202302$coefficients