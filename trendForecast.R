#------------------------------------------------------------------------------#
# Developer: Rhys Hewer
# Project: Ubiqum - Energy Consumption Forecasting
# Version: 1
# Purpose: Forecasting energy consumption trend for following 6 months 
#------------------------------------------------------------------------------#

#load libraries
source("scripts/libraries.R")

#load data
load("output/timeData.RDS")


##### MONTHLY FIGURES AND TREND ###############################################

#total period grouped by months
conData <- timeData %>% group_by(year, month) %>% 
        summarise(monthAvg = mean(kwhpm)*60*24) # per month avg kwh per day

TSconData <- conData$monthAvg %>% ts(frequency = 12, start = c(2006,12))

#decompose to find trend
conDec <- stl(TSconData, "periodic")
autoplot(conDec)

conDec_trend <- conDec$time.series[,2]


##### 6 MONTH FORECAST MODELLING ##############################################

#Training set
conDecTrain <- conDec_trend[1:42] %>% 
        ts(frequency = 12, start = c(2006,12))

#ARIMA Model
conModFin.h <- auto.arima(conDec_trend)
conFitFin.h <- forecast(conModFin.h, h = 6)
autoplot(conFitFin.h)


##### VISUALISING FORECAST ####################################################

###Plotting presentation graph
conAct <- conFitFin.h %>% fortify()
conAct <- conAct %>% rename(preds = 'Point Forecast', 
                            lo80 = 'Lo 80', hi80 = 'Hi 80',
                            lo95 = 'Lo 95', hi95 = 'Hi 95')


g.conAct <- ggplot(conAct) +
        
        theme_bw(base_size = 20) +
        ylab("Average kWh Use per Day") + 
        xlab("Year") + 
        ggtitle("KwH Use per Day: Trend & Forecast") +
        
        geom_ribbon(aes(x = Index, ymin=conAct$lo80, ymax=conAct$hi80), 
                    linetype=2, alpha=0.3, fill = "#FF9F1C")+
        geom_ribbon(aes(x = Index, ymin=conAct$lo95, ymax=conAct$hi95), 
                    linetype=2, alpha=0.3, fill = "#FF9F1C") +
        
        geom_line(aes(Index, Data), size = 1.5, colour = "#011627") +
        geom_line(aes(Index, preds), size = 2, colour = "#E71D36")

g.conAct
