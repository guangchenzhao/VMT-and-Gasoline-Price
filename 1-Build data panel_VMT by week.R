library(data.table)
library(rgdal)
library(bit64)
library(plyr)
library(dplyr)
library(lutz)
library(lubridate)
library(tidyverse)

setwd()

#daily_numoftrips_VMT for each device
mypath = "/Triprosters_daily"

for (i in 1:length(zipF)){
  
  trip_roster = fread(zipF[i],fill = TRUE)
  trip_roster$Trip_Distance = trip_roster$cum_trip_distance_mile
  trips_daily = list(
    trip_roster %>% count(device_id), 
    # vehicle trips only & reasonable trip duration and average speed
    trip_roster[(linked_trip_mode==0) & (travel_time_minute<=720) &
                  (Trip_Distance*60/travel_time_minute<=100)] %>% count(device_id),
    data.table(aggregate(Trip_Distance ~ device_id, data = trip_roster, sum)), 
    data.table(aggregate(Trip_Distance ~ device_id, data = trip_roster[(linked_trip_mode==0) & (travel_time_minute<=720) & 
                                                                         (Trip_Distance*60/travel_time_minute<=100)], sum))
  ) %>% 
    reduce(left_join, by='device_id')
  
  trips_daily[is.na(trips_daily)] = 0
  
  colnames(trips_daily) = c('device_id', 'numoftrips', 'numofdrivetrips', 'mileage', 'drivemileage')
  
  fwrite(trips_daily, paste0("/daily_numoftrips_mileage/WMA_device_daily_numoftrips_mileage_",
                             substr(zipF[i], 59,66),".csv"))
  
}

## average daily_numoftrips_VMT for each device in each week
mypath_1 = "/daily_numoftrips_mileage"
zipF_1 <- list.files(path = mypath_5,pattern = "WMA_device_daily_numoftrips_mileage_2021", 
                     full.names = TRUE)

for(week_i in 2:53){
  print(week_i)
  weeklist = zipF_1[max(1,(7*week_i-10)):min((7*week_i-6),length(zipF_1))]#2021 only weekdays
  #weeklist = zipF_1[max(1,(7*week_i-11)):min((7*week_i-7),length(zipF_1))]#2022# only weekdays
  
  weekly_numoftrips_mileage = data.table()
  
  for (i in 1:length(weeklist)){
    daily_numoftrips_mileage = fread(weeklist[i],fill = TRUE)
    weekly_numoftrips_mileage= rbind(weekly_numoftrips_mileage, daily_numoftrips_mileage)
  }
  weekly_avgdaily_numoftrips_mileage = 
    weekly_numoftrips_mileage[ , .(numofdays = .N, 
                                   numoftrips = mean(numoftrips), 
                                   numofdrivetrips = mean(numofdrivetrips), 
                                   mileage = mean(mileage), 
                                   drivemileage = mean(drivemileage)),
                               by = .(device_id)]
  fwrite(weekly_avgdaily_numoftrips_mileage, paste0(
    "/weekly_avgdaily_numoftrips_mileage_2021weekdays/WMA_device_avgdaily_numoftrips_mileage_week",
    formatC(week_i, width = 2, format = "d", flag = "0"),".csv"))
}

#some stats for evaluation - daily

mypath_5 = "/daily_numoftrips_mileage"
zipF_5 <- list.files(path = mypath_5,pattern = "WMA_device_daily_numoftrips_mileage_2022", full.names = TRUE)

stats_drivetrips = data.table()

for (i in 1:12){
  
  print(i)
  monthlylist = list.files(path = mypath_5,pattern = paste0("mileage_2022",formatC(i, width = 2, format = "d", flag = "0")), full.names = TRUE)
  device_list = fread(paste0("/WMA_device_list_2022",formatC(i, width = 2, format = "d", flag = "0"),".csv"))[,c('device_id', 'provider')]
  
  for (j in 1:length(monthlylist)){
    
    daily_numoftrips_mileage = fread(monthlylist[j],fill = TRUE)
    daily_numoftrips_mileage = merge(daily_numoftrips_mileage, device_list, by = 'device_id', all.x = TRUE)
    daily_stats_drivetrips = as.data.table(t(c(
      paste0(substr(monthlylist[j], 79,82),"-",substr(monthlylist[j], 83,84),"-",substr(monthlylist[j], 85,86)),
      length(unique(daily_numoftrips_mileage$device_id)), 
      mean(daily_numoftrips_mileage$numofdrivetrips), 
      mean(daily_numoftrips_mileage$drivemileage), 
      median(daily_numoftrips_mileage$numofdrivetrips), 
      median(daily_numoftrips_mileage$drivemileage)))) 
    
    stats_drivetrips = rbind(stats_drivetrips, daily_stats_drivetrips)
  }
}

colnames(stats_drivetrips) = c('Date','num_of_device', 'avg#_driving_trips', 'avg_mileage_driven', 'median#_driving_trips', 'median_mileage_driven')

fwrite(stats_drivetrips, "/stats/stats_drivetrips_daily_2022.csv")

#########################################weekly

weekly_avgdaily_numoftrips_mileage_stat = data.table()

for(week_i in 2:53){
  weekly_avgdaily_numoftrips_mileage = fread(paste0(
    "/WMA_device_avgdaily_numoftrips_mileage_week",
    formatC(week_i, width = 2, format = "d", flag = "0"),".csv"))
  
  
  weekly_stats = as.data.table(t(c(
    paste0("2021week",formatC(week_i, width = 2, format = "d", flag = "0")),
    length(unique(weekly_avgdaily_numoftrips_mileage$device_id)), 
    mean(weekly_avgdaily_numoftrips_mileage$numoftrips),
    mean(weekly_avgdaily_numoftrips_mileage$numofdrivetrips), 
    mean(weekly_avgdaily_numoftrips_mileage$mileage),
    mean(weekly_avgdaily_numoftrips_mileage$drivemileage)
  ))) 
  
  weekly_avgdaily_numoftrips_mileage_stat = rbind(weekly_avgdaily_numoftrips_mileage_stat, weekly_stats)
  
}
colnames(weekly_avgdaily_numoftrips_mileage_stat) = c('Date','num_of_device', 'avg#_trips', 'avg#_driving_trips', 'avg_mileage', 'avg_mileage_driven')
fwrite(weekly_avgdaily_numoftrips_mileage_stat, "/stats/stats_worktrips_weekly_weekdays.csv")


######################################### time span of each device

total_weekly_avgdaily_numoftrips_mileage = data.table()

for(week_i in 19:53){
  weekly_avgdaily_numoftrips_mileage = fread(paste0(
    "/WMA_device_avgdaily_numoftrips_mileage_week",
    formatC(week_i, width = 2, format = "d", flag = "0"),".csv"))
  
  total_weekly_avgdaily_numoftrips_mileage = rbind(total_weekly_avgdaily_numoftrips_mileage, weekly_avgdaily_numoftrips_mileage)
  
}

day_week_stats = 
  total_weekly_avgdaily_numoftrips_mileage[ , .( 
    numofdays = sum(numofdays), 
    numofweeks = .N),
    by = .(device_id)]