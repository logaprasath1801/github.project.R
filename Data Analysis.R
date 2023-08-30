#load libraries 
library(tidyverse) #calculations
library(lubridate) #dates 
library(hms) #time
library(data.table) #exporting data frame

#load original .csv files, a years worth of data from January 2022 to December 2022
jan01_df <- read_csv("202201-divvy-tripdata.csv") 
feb02_df <- read_csv("202202-divvy-tripdata.csv") 
mar03_df <- read_csv("202203-divvy-tripdata.csv")
apr04_df <- read_csv("202204-divvy-tripdata.csv") 
may05_df <- read_csv("202205-divvy-tripdata.csv")
jun06_df <- read_csv("202206-divvy-tripdata.csv") 
jul07_df <- read_csv("202207-divvy-tripdata.csv") 
aug08_df <- read_csv("202208-divvy-tripdata.csv")
sep09_df <- read_csv("202209-divvy-publictripdata.csv")
oct10_df <- read_csv("202210-divvy-tripdata.csv") 
nov11_df <- read_csv("202211-divvy-tripdata.csv") 
dec12_df <- read_csv("202212-divvy-tripdata.csv") 

#merge all of the data frames into one year view
cyclistic_df <- rbind (jan01_df, feb02_df, mar03_df, apr04_df, may05_df, jun06_df, jul07_df,aug08_df, sep09_df, oct10_df, nov11_df, dec12_df)

#remove individual month data frames to clear up space in the environment 
remove(jan01_df, feb02_df, mar03_df, apr04_df, may05_df, jun06_df, jul07_df,aug08_df, sep09_df, oct10_df, nov11_df, dec12_df)

#create new data frame to contain new columns
cyclistic_date <- cyclistic_df

#calculate ride length by subtracting ended_at time from started_at time and converted it to minutes
cyclistic_date$ride_length <- difftime(cyclistic_df$ended_at, cyclistic_df$started_at, units = "mins")

#create columnds for: day of week, month, day, year, time, hour
cyclistic_date$date <- as.Date(cyclistic_date$started_at) #default format is yyyy-mm-dd, use start date
cyclistic_date$day_of_week <- wday(cyclistic_df$started_at) #calculate the day of the week 
cyclistic_date$day_of_week <- format(as.Date(cyclistic_date$date), "%A") #create column for day of week
cyclistic_date$month <- format(as.Date(cyclistic_date$date), "%m")#create column for month
cyclistic_date$day <- format(as.Date(cyclistic_date$date), "%d") #create column for day
cyclistic_date$year <- format(as.Date(cyclistic_date$date), "%Y") #create column for year
cyclistic_date$time <- format(as.Date(cyclistic_date$date), "%H:%M:%S") #format time as HH:MM:SS
cyclistic_date$time <- as_hms((cyclistic_df$started_at)) #create new column for time
cyclistic_date$hour <- hour(cyclistic_date$time) #create new column for hour



#clean the data
cyclistic_date <- na.omit(cyclistic_date) #remove rows with NA values
cyclistic_date <- distinct(cyclistic_date) #remove duplicate rows 
cyclistic_date <- cyclistic_date[!(cyclistic_date$ride_length <=0),] #remove where ride_length is 0 or negative
cyclistic_date <- cyclistic_date %>%  #remove columns not needed: ride_id, start_station_id, end_station_id, start_lat, start_long, end_lat, end_lng
  select(-c(ride_id, start_station_id, end_station_id,start_lat,start_lng,end_lat,end_lng)) 

#view the final data
View(cyclistic_date)

#-----------------------------------------TOTAL RIDES--------------------------------------

#total number of rides
nrow(cyclistic_date)

#-----------------MEMBER TYPE---------------------
cyclistic_date %>%
  group_by(member_casual) %>% 
  count(member_casual)

#----------------TYPE OF BIKE---------------------

#total rides by member type 
cyclistic_date %>%
  group_by(member_casual, rideable_type) %>% 
  count(rideable_type)

#total rides 
cyclistic_date %>%
  group_by(rideable_type) %>% 
  count(rideable_type)

#-------------------HOUR--------------------------

#total rides by member type 
cyclistic_date %>%
  group_by(member_casual) %>% 
  count(hour) %>% 
  print(n = 48) #lets you view the entire tibble

#total rides
cyclistic_date %>%
  count(hour) %>% 
  print(n = 24) #lets you view the entire tibble


#----------------DAY OF THE WEEK------------------

#total rides by member type
cyclistic_date %>%
  group_by(member_casual) %>% 
  count(day_of_week)

#total rides 
cyclistic_date %>%
  count(day_of_week)

#----------------DAY OF THE MONTH-----------------

#total rides by member type
cyclistic_date %>%
  group_by(member_casual) %>% 
  count(day) %>% 
  print(n = 62)
 #lets you view the entire tibble


#total rides
cyclistic_date %>%
  count(day) %>% 
  print(n=31)


#---------------------MONTH-----------------------

#total rides by member type 
cyclistic_date %>%
  group_by(member_casual) %>% 
  count(month) %>% 
  print(n = 24) #lets you view the entire tibble

#total rides
cyclistic_date %>%
  count(month) 



#------------------------------------AVERAGE RIDE LENGTH-----------------------------------

#average of ride_length
cyclistic_avgRide <- mean(cyclistic_date$ride_length)
print(cyclistic_avgRide)

#------------------MEMBER TYPE--------------------

#average ride_length
cyclistic_date %>% group_by( member_casual) %>% 
  summarise_at (vars(ride_length),
                list(time = mean))
              

#----------------TYPE OF BIKE---------------------

#total rides by member type 
cyclistic_date %>% group_by(member_casual, rideable_type) %>% 
  summarise_at(vars(ride_length), 
               list(time = mean))

  #average ride_length
  cyclistic_date %>% group_by(rideable_type) %>% 
    summarise_at(vars(ride_length), 
                 list(time = mean))
  
   
#-----------------------HOUR-------------------------

#average ride_length by member type
cyclistic_date %>% group_by(hour, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=48) #lets you view entire tibble

#average ride_length
cyclistic_date %>% group_by(hour) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=24) #lets you view entire tibble



#-------------------DAY OF THE WEEK-----------------

#average ride_length by member type
cyclistic_date %>% group_by(member_casual, day_of_week) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride_length 
cyclistic_date %>% group_by(day_of_week) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----------------DAY OF THE MONTH------------------

#average ride_length by member type
cyclistic_date %>% group_by(day, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=62)  #lets you view entire tibble

#average ride_length
cyclistic_date %>% group_by(day) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=31)  #lets you view entire tibble

#---------------------MONTH--------------------------

#average ride_length by member type
cyclistic_date %>% group_by(month, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=24)  #lets you view entire tibble

#average ride_length
cyclistic_date %>% group_by(month) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))
