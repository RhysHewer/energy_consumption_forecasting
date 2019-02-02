#------------------------------------------------------------------------------#
# Developer: Rhys Hewer
# Project: Ubiqum - Energy Consumption Forecasting
# Version: 1
# Purpose: 14 day energy costs & visualisation
#------------------------------------------------------------------------------#

#load libraries
source("scripts/libraries.R")

#load data
load("output/timeData.RDS")

##### SUBSET DATA & ESTIMATED COST ############################################

#subset data for given time-range and calculate mean cost of energy use
last14 <- timeData %>% 
        filter(Date >= "2010-03-11" & Date < "2010-03-24")
rm(timeData)

#energy cost (£) last 14 days (est.)
useCost <- sum(last14$cost)


##### VISUALISING ENERGY USE ##################################################

#mean hourly use dataframe for last 14 days
useDemo <- last14 %>% 
        group_by(hour) %>% 
        summarise(use = mean(kwhpm)*60, 
                  cost = mean(cost), 
                  timecost = mean(timecost))

#daily use graph
g.dayUse <- ggplot(useDemo, aes(hour, use)) +
        geom_line(colour = "#011627", size = 2) +
        theme_bw(base_size = 20) +
        
        ylab("Kilowatt Hours") + 
        ggtitle("Hourly Energy Use (14 day avg) & 
                Energy Cost per Hour (pence)") +
        
        theme(axis.title.x = element_blank(), 
              axis.text.x = element_blank()) +
        
        coord_cartesian(xlim = c(0,23))
g.dayUse


#Hourly energy cost
g.timecost <- ggplot(useDemo, aes(hour, timecost, fill = timecost)) +
        geom_col() +
        
        scale_fill_gradient(low = "#FFE1B7", 
                            high = "#FF9F1C", 
                            guide=FALSE) +
        
        theme_bw(base_size = 20) +
        
        ylab("Cost per Kilowatt Hour") + 
        xlab("Time (Hour of Day)") +
        
        coord_cartesian(xlim = c(0,23))

g.timecost

#Combined plot
grid.newpage()
grid.draw(rbind(ggplotGrob(g.dayUse), ggplotGrob(g.timecost), size = "first"))
