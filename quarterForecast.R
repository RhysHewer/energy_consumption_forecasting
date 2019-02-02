#------------------------------------------------------------------------------#
# Developer: Rhys Hewer
# Project: Ubiqum - Energy Consumption Forecasting
# Version: 1
# Purpose: Forecasting energy consumption to end of current quarter/
#          next quarter
#------------------------------------------------------------------------------#

#load libraries
source("scripts/libraries.R")

#load data
load("output/timeData.RDS")


##### DATA PREPARATION ########################################################

#Averaged daily data for prediction
qTime <- timeData %>% filter(quarter >= "2008.2" & quarter < "2010.4")

qTimeDay <- qTime %>% 
        group_by(Date) %>% 
        summarise(avgKwhpD = mean(kwhpm)*60*24, timecost = mean(timecost))

qTimeDay$poundCost <- (qTimeDay$avgKwhpD * qTimeDay$timecost)/100


##### MODELLING FORECAST ######################################################

#work out horizon to end of next quarter (q1, 2011)
finalHor1 <- "2011-03-31" %>% ymd() #end of next quarter
finalHor2 <- "2010-10-20" %>% ymd() # fake "today"
finalHor <- interval(finalHor2,finalHor1) %>% as.duration()/(60*60*24) 

finalHor <- finalHor %>% 
        as.numeric() %>% +1 #+1 to include today's date in prediction   


#adjust time series to run until 'today' (20 Oct 2010)
qTimeFinal <- timeData %>% filter(quarter >= "2008.2" & quarter <= "2010.4")

qTimeDayFinal <- qTimeFinal %>% 
        group_by(Date) %>% 
        summarise(avgKwhpD = mean(kwhpm)*60*24, timecost = mean(timecost))

qTimeDayFinal$poundCost <- (qTimeDayFinal$avgKwhpD * 
                                    qTimeDayFinal$timecost)/100

TStime <- qTimeDayFinal$avgKwhpD
TSfinal <- TStime[1:932] %>% ts(start = c(2008, 92), frequency = 365.25)


#Using STL/Arima model and full dataset (q2,2008- q4, 2010)       
qMod.final <- stlm(TSfinal, method = "arima") 
qFit.final <- forecast(qMod.final, h = finalHor)
autoplot(qFit.final)

#fortify to DF + add date column
qFit <- qFit.final %>% fortify()
qFit <- qFit %>% rename(avgKwhpD = Data, preds = 'Point Forecast')
qFit$date <- seq(from = as.Date("2008-04-01"), 
                 to = as.Date(finalHor1), by = 'day')

#monetise predictions 
#(combine actual and predictions into one column and * by average kWh cost)
avgKwh <- mean(qTimeDay$timecost)
qFit$cost <- rowSums(qFit[,c("avgKwhpD", "preds")]*avgKwh/100, na.rm = TRUE)

qCurr <- qFit %>% filter(date >= "2010-10-01" & date <= "2010-12-31")
qNxt <- qFit %>% filter(date >= "2011-01-01" & date <= "2011-03-31")

#Current quarter cost (£)
qCurrCost <- sum(qCurr$cost)

#Next quarter costs (£)
qNxtCost <- sum(qNxt$cost)


##### VISUALISING FORECAST ####################################################


qFit <- qFit %>% rename(lo80 = 'Lo 80', hi80 = 'Hi 80',
                        lo95 = 'Lo 95', hi95 = 'Hi 95')

g.qFit <- ggplot(qFit) +
        
        theme_bw(base_size = 20) +
        ylab("Average kWh Use per Day") + 
        xlab("Year") + 
        ggtitle("KwH Use per Day: Trend & Forecast") +
        
        geom_rect(xmin = 2010.749, xmax = 2010.998, ymin = 0, ymax = 60, 
                  fill = "#A8ADAC", alpha = 0.3)+
        geom_rect(xmin = 2011.001, xmax = 2011.244, ymin = 0, ymax = 60, 
                  fill = "#BEC4C3", alpha = 0.1)+
        
        geom_line(aes(Index, avgKwhpD), 
                  size = 1, 
                  colour = "#011627", 
                  alpha = 0.9) +
        
        geom_line(aes(Index, preds), 
                  size = 1, 
                  colour = "#FF9F1C", 
                  alpha = 0.9) +
        
        
        geom_smooth(aes(Index, preds), 
                    se = FALSE, 
                    colour = "#E71D36", 
                    size = 2) +
        
        geom_vline(xintercept = 2010.801, 
                   linetype="dotted", 
                   color = "#26532B", 
                   size=1.5) +
        
        geom_text(aes(x=2010.850, label="20 Oct 2010", y=5), 
                  colour="#26532B", angle=90)

g.qFit
