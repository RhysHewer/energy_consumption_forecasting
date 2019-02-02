#------------------------------------------------------------------------------#
# Developer: Rhys Hewer
# Project: Ubiqum - Energy Consumption Forecasting
# Version: 1
# Purpose: Preprocessing data for energy consumption forecasting
#------------------------------------------------------------------------------#

#Load libraries
source("scripts/libraries.R")

#load data (separated by ;)
data <- read_csv2("data/household_power_consumption.txt")


##### DATA CLASS CONVERSION ###################################################

#Create date-time feature as dmy lubridate object
dData <- data %>% select(Date, Time)
dData$datetime <- paste(dData$Date, dData$Time, sep = " ")
dData$datetime <- dData$datetime %>% dmy_hms()
dData$Date <- dData$Date %>% dmy()

#Convert non-time features to numerics
numData <- data %>% select(-Date, -Time)
numData <- numData %>% sapply(as.numeric) %>% as.data.frame()

#Recombine date & numeric datasets
comData <- bind_cols(dData, numData)
rm(data, dData, numData)


##### NA IMPUTATION ###########################################################

#Subset features with NAs
naSet <- c( "Global_active_power",
            "Global_reactive_power",
            "Voltage",
            "Global_intensity",
            "Sub_metering_1",
            "Sub_metering_2",
            "Sub_metering_3")

#Convert to time-series objects and impute missing values
comData[,naSet] <- comData[,naSet] %>% sapply(ts)

comData[,naSet] <- comData[,naSet] %>% 
        sapply(function(x) na.interpolation(x, option = "linear"))


##### FEATURE ENGINEERING - DATE/TIME BREAKDOWN ###############################

#Breakdown date into constituent parts for subsetting
comData <- comData %>% 
        mutate(year = year(datetime), 
               month = month(datetime), 
               day = day(datetime),
               week = week(datetime),
               quarter = quarter(datetime, with_year = TRUE),
               weekday = wday(datetime, label = T, abbr = F),
               hour = hour(datetime))

#Arrange features in date order
comData <- comData %>% select(Date, 
                              Time, 
                              datetime, 
                              year, 
                              quarter, 
                              month,
                              week,
                              weekday,
                              day,
                              hour,
                              everything())


##### FEATURE ENGINEERING - MISSING SUBMETER ENERGY ###########################

#submeter 4 calculated as remaining energy when submeters removed from GAP
comData$sm4 <- (comData$Global_active_power*1000/60) - 
                (comData$Sub_metering_1 + 
                 comData$Sub_metering_2 + 
                 comData$Sub_metering_3)

#Total energy use in kwh per minute
comData$totkwh = comData$Sub_metering_1 + 
                comData$Sub_metering_2 + 
                comData$Sub_metering_3 + 
                comData$sm4

#rename submeter features
comData <- comData %>% rename(sm1 = Sub_metering_1, 
                              sm2 = Sub_metering_2, 
                              sm3 = Sub_metering_3, 
                              kwhpm = totkwh)

#convert watt-hour values to kilowatt-hour values (prices usually given in kwh)
kwhSet <- c("sm1", "sm2", "sm3", "sm4", "kwhpm")

comData[,kwhSet] <- comData[,kwhSet]/1000


##### ADDING TIME-OF-DAY COSTS ################################################

timeData <- comData
rm(comData)

timeData$timecost <- NA

##Weekend Low - 7.91
timeData[(timeData$weekday == "Saturday" | 
                  timeData$weekday == "Sunday") & 
                 (timeData$hour >= 0 & 
                          timeData$hour < 7),]$timecost <- 7.91

##Weekend Standard - 16.27
timeData[(timeData$weekday == "Saturday" | 
                  timeData$weekday == "Sunday") & 
                 (timeData$hour <= 23 & 
                          timeData$hour >= 7),]$timecost <- 16.27


#weekday low - 7.91
timeData[(timeData$weekday != "Saturday" & 
                  timeData$weekday != "Sunday") & 
                 (timeData$hour >= 0 & 
                          timeData$hour < 7),]$timecost <- 7.91

#weekday standard - 16.27
timeData[(timeData$weekday != "Saturday" & 
                  timeData$weekday != "Sunday") & 
                 (timeData$hour >= 7 & 
                          timeData$hour < 16),]$timecost <- 16.27

#weekday high - 32.55
timeData[(timeData$weekday != "Saturday" & 
                  timeData$weekday != "Sunday") & 
                 (timeData$hour >= 16 & 
                          timeData$hour < 20),]$timecost <- 32.55

#weeknight - 16.27
timeData[(timeData$weekday != "Saturday" & 
                  timeData$weekday != "Sunday") & 
                 (timeData$hour >= 20 & 
                          timeData$hour <= 23),]$timecost <- 7.91

#Cost feature
timeData$cost <- (timeData$kwhpm*timeData$timecost)/100


##### INCONSISTENT DATA #######################################################

#sm4 negative values - to zero (no change to mean/median/max)
timeData[(timeData$sm4 < 0),]$sm4 <- 0


##### OUTPUT FILE #############################################################

save(timeData, file = "output/timeData.RDS")

