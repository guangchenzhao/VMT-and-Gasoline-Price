library(data.table)
library(rgdal)
library(R.utils)
library(bit64)
library(plyr)
library(dplyr)
library(lutz)
library(lubridate)
library(ggplot2)
library(haven)

#### merge gas price & other ACS attributes for regression modeling

#ACS attributes at census tract level
acs_TT_aggregate = fread("/acs_TT_aggregate_2021.csv")
acs_TT_aggregate$median_HHincome = as.integer(acs_TT_aggregate$median_HHincome)
acs_TT_aggregate$TTFIPS = as.character(acs_TT_aggregate$TTFIPS)
acs_TT_aggregate$TTFIPS = formatC(acs_TT_aggregate$TTFIPS, width = 11, format = "d", flag = "0")

for(month_i in 1:12){
device_acs_trip_gas_total = data.table()

#device list with Geo FIPS of home locations
device_list = fread(paste0("/WMA_device_list_2022",formatC(month_i, width = 2, format = "d", flag = "0"),".csv"))
device_list$h_BGFIPS = as.character(device_list$h_BGFIPS)
device_list$h_TTFIPS = substr(device_list$h_BGFIPS,1,11)
print(nrow(device_list))

#merge acs by tract
device_list_acs = merge(device_list, acs_TT_aggregate, by.x = 'h_TTFIPS', by.y = 'TTFIPS', all.x = TRUE)
device_list_acs = device_list_acs[!is.na(median_HHincome)]
print(nrow(device_list_acs))

# device list is in monthly basis and vmt is weekly, so determine the range when merging them
startdate = as.Date(paste0("2022-",formatC(month_i, width = 2, format = "d", flag = "0"),"-01"))
weekofyear_1 = as.integer(strftime(startdate, format = "%U"))+1
weekofyear_2 = as.integer(strftime(ceiling_date(startdate, "month") - days(1), format = "%U"))+1

#attach gasoline price near home locations
for(week_i in max(weekofyear_1, 2):weekofyear_2){ # 2022 weekdays
  print(week_i)
  weekly_avgdaily_numoftrips_mileage = fread(
    paste0("/WMA_device_avgdaily_numoftrips_mileage_week",
           formatC(week_i, width = 2, format = "d", flag = "0"),".csv"))

  weekly_device_gas = fread(
    paste0("/WMA_device_gas_week",
           formatC(week_i, width = 2, format = "d", flag = "0"),"_2022",
           formatC(month_i, width = 2, format = "d", flag = "0"),".csv"))
  device_acs_trip_gas = merge(device_list_acs, weekly_avgdaily_numoftrips_mileage, by = 'device_id')
  device_acs_trip_gas = merge(device_acs_trip_gas, weekly_device_gas, by = 'device_id')
  device_acs_trip_gas = device_acs_trip_gas[,c("device_id",
                                               #"device_weight", 
                                               "numoftrips","numofdrivetrips","mileage","drivemileage",
                                               "h_BGFIPS","h_TTFIPS","h_CTFIPS","w_CTFIPS",
                                               "hw_centroid_dist","STFIPS","num_residents","num_worker","median_HHincome",
                                               "cbg_inc_pct_less_25k",  "cbg_inc_pct_25k_50k", "cbg_inc_pct_50k_75k","cbg_inc_pct_75k_125k", "cbg_inc_pct_125k_more",
                                               "cbg_age_pct_under_17", "cbg_age_pct_18_24", "cbg_age_pct_25_34", "cbg_age_pct_35_54","cbg_age_pct_55_64","cbg_age_pct_65_over",
                                               "ct_pov_rate","ct_edu_pct_less_HS","ct_edu_pct_SCND_AD", "ct_edu_pct_BD_more","h_gs_count_2mi","h_gs_avg_price_2mi",
                                               "w_gs_count_2mi","w_gs_avg_price_2mi","h_gs_count_3mi","h_gs_avg_price_3mi", "w_gs_count_3mi" ,"w_gs_avg_price_3mi",
                                               "h_gs_count_4mi" , "h_gs_avg_price_4mi" ,"w_gs_count_4mi", "w_gs_avg_price_4mi","h_gs_count_5mi" , "h_gs_avg_price_5mi",
                                               "w_gs_count_5mi" ,"w_gs_avg_price_5mi")]
  device_acs_trip_gas$weekofyear = paste0('2022week',formatC(week_i, width = 2, format = "d", flag = "0"))
  device_acs_trip_gas_total=rbind(device_acs_trip_gas_total, device_acs_trip_gas)
}

fwrite(device_acs_trip_gas_total, paste0(
  "/device_trip_acs_gas_2022",
  formatC(month_i, width = 2, format = "d", flag = "0"),".csv"))
}

## weekly gasoline price in each zone
######### plot gas price data
gas_price_202101_202302 = fread("D:/Guangchen/Gas/weekly_gas_price/gas_price_202101_202302.csv")
gas_station_TT = fread("D:/Guangchen/Gas/gas_station_TT.csv")

gas_price_202101_202302 = merge(gas_price_202101_202302, 
                                gas_station_TT[, c("Station ID","GEOID", "ALAND")],
                                #gas_station_BG[, c("Station ID","GEOID", "ALAND")],
                                by = 'Station ID')
gas_price_202101_202302$BGFIPS = as.character(gas_price_202101_202302$GEOID)
gas_price_202101_202302$TTFIPS = substr(gas_price_202101_202302$BGFIPS, 1, 11)
gas_price_202101_202302$CTFIPS = substr(gas_price_202101_202302$BGFIPS, 1, 5)
gas_price_202101_202302$STFIPS = substr(gas_price_202101_202302$BGFIPS, 1, 2)
gas_price_202101_202302 = gas_price_202101_202302[,-c('GEOID')]
gas_price_byCT = gas_price_202101_202302[ , .(numofstations = .N, avg_price = mean(Price)),
                              by = .(CTFIPS, Date)]
gas_price_byST = gas_price_202101_202302[ , .(numofstations = .N, avg_price = mean(Price)),
                                          by = .(STFIPS, Date)]

gas_price_byCT %>%
  ggplot( aes(x=Date, y=avg_price, group=CTFIPS, color=CTFIPS)) +
  geom_line(size = 1)

gas_price_byST %>%
  ggplot( aes(x=Date, y=avg_price, group=STFIPS, color=STFIPS)) +
  geom_line(size = 2)+
  #scale_color_viridis(discrete = TRUE) +
  ggtitle("Gas price variation in different States") +
  #theme_ipsum() +
  ylab("Average Gas Price ($)")

#Cross-sectional variation
summary(gas_price_202101_202302[Date==unique(gas_price_202101_202302$Date)[1]]$Price)
var(gas_price_202101_202302[Date==unique(gas_price_202101_202302$Date)[1]]$Price)
gas_price_sd_bySTweek = gas_price_202101_202302[ , .(price_sd = sd(Price)),
                                          by = .(STFIPS, Date)]
gas_price_sd_bySTweek %>%
  ggplot(aes(x = Date, y = price_sd, group=STFIPS, color=STFIPS)) +
  geom_line()+
  ggtitle("Standard deviation of gas price by state") +
  ylab("Standard deviation ($)")

##############regression of price and zone level variables

######################################
# other attibutes
numstation_0.25mile = fread("/numstation_0.25mile.csv")
colnames(numstation_0.25mile) = c('Station ID', 'num_gasstation_0.25mile')
road_length_TT = fread("/road_length_TT.csv")
road_length_TT$TTFIPS = as.character(road_length_TT$GEOID)

gas_station_price_reg_202101_202302 =  merge(gas_price_202101_202302, 
                                             acs_TT_aggregate[,-c('Geography ID')], by = 'TTFIPS')
gas_station_price_reg_202101_202302$pop_dens = gas_station_price_reg_202101_202302$num_residents*10^6 / gas_station_price_reg_202101_202302$ALAND
gas_station_price_reg_202101_202302 = merge(gas_station_price_reg_202101_202302, 
                                            numstation_0.25mile, by = 'Station ID')

gas_station_price_reg_202101_202302 = merge(gas_station_price_reg_202101_202302, 
                                            road_length_TT[,-c('GEOID')], by = 'TTFIPS')
gas_station_price_reg_202101_202302$RoadLength_dens = gas_station_price_reg_202101_202302$RoadLength * 10^3 / 
  gas_station_price_reg_202101_202302$ALAND

gas_station_price_reg_202101_202302$StationID = gas_station_price_reg_202101_202302$`Station ID`
gas_station_price_reg_202101_202302$GasBrand = gas_station_price_reg_202101_202302$`Gas Brand`
gas_station_price_reg_202101_202302$num_gasstation_025mile = gas_station_price_reg_202101_202302$num_gasstation_0.25mile

gas_station_price_reg_202101_202302 = gas_station_price_reg_202101_202302[, -c('num_gasstation_0.25mile','Gas Brand',
                                                                               'Station ID')]
mypath_6 = "/weekyly_gas_station_price_reg_weekdays"
zipF_6 <- list.files(path = mypath_6, pattern = ".csv", full.names = TRUE)

gas_station_price_reg = data.table()
for (i in 1:length(zipF_6)) {
  gas_station_price_reg_i = fread(zipF_6[i])
  gas_station_price_reg = rbind(gas_station_price_reg, gas_station_price_reg_i)
}
gas_station_price_reg$TTFIPS = as.character(gas_station_price_reg$TTFIPS)
gas_station_price_reg$StationID = gas_station_price_reg$`Station ID`

gas_station_price_reg_202101_202302 = merge(
  gas_station_price_reg_202101_202302, 
  gas_station_price_reg[,c('Date', 'StationID',"numofdevice","avg_numoftrips", 
                           "avg_numofdrivetrips", "avg_mileage", "avg_drivemileage", 
                           "avg_drivemileage_2nd", "numofdevice_prev",
                           "avg_numoftrips_prev", "avg_numofdrivetrips_prev", "avg_mileage_prev",
                           "avg_drivemileage_prev","avg_drivemileage_2nd_prev", "weekofyear")], 
  by = c('Date', 'StationID'), all.x=TRUE)
gas_station_price_reg_202101_202302$total_mileage = gas_station_price_reg_202101_202302$num_residents*
  gas_station_price_reg_202101_202302$avg_drivemileage
gas_station_price_reg_202101_202302$total_mileage_2 = gas_station_price_reg_202101_202302$total_mileage*
  gas_station_price_reg_202101_202302$total_mileage
gas_station_price_reg_202101_202302$total_mileage_prev = gas_station_price_reg_202101_202302$avg_drivemileage_prev * 
  gas_station_price_reg_202101_202302$num_residents

fwrite(gas_station_price_reg_202101_202302, 
       "/gas_station_price_reg_202101_202302.csv")