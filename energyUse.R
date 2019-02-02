#------------------------------------------------------------------------------#
# Developer: Rhys Hewer
# Project: Ubiqum - Energy Consumption Forecasting
# Version: 1
# Purpose: visualise differences in energy consumption summer v winter
#------------------------------------------------------------------------------#

#load libraries
source("scripts/libraries.R")

#load data
load("output/timeData.RDS")


##### DATA PREPARATION ########################################################

#extract summer 2009 - summer2010
dayZone<- timeData %>% filter(Date >= "2009-07-01" & Date <= "2010-06-30")

#create minute feature and collect into 15 min chunks
dayZone$quarter <- NA
dayZone <- dayZone %>% mutate(minute = minute(datetime)) %>% 
        select(weekday, minute, hour, quarter, everything())


dayZone[(dayZone$minute >= 0 & dayZone$minute < 15),]$quarter <- 1
dayZone[(dayZone$minute >= 15 & dayZone$minute < 30),]$quarter <- 2
dayZone[(dayZone$minute >= 30 & dayZone$minute < 45),]$quarter <- 3
dayZone[(dayZone$minute >= 45 & dayZone$minute <= 59),]$quarter <- 4


##### DATA VISUALISATION ######################################################

#plot day average pattern for 2009 - Jan, July
#Jan 2010
dayZoneJan <- dayZone %>% filter(month == 1) %>%
        group_by(hour, quarter) %>% 
        summarise(sm1 = mean(sm1), 
                  sm2 = mean(sm2), 
                  sm3 = mean(sm3), 
                  sm4 = mean(sm4))

dayZoneJan.long <- dayZoneJan %>% 
        gather("sm1", "sm2", "sm3", "sm4", 
               key = "sm", 
               value = "avgKwhpm")

dayZoneJan.long$time <- paste(dayZoneJan.long$hour, 
                              dayZoneJan.long$quarter, 
                              sep = ".") %>% as.numeric()



g.jan10 <- ggplot(dayZoneJan.long, aes(time, avgKwhpm, 
                                       group = sm, 
                                       colour = sm, 
                                       size = sm)) +
        
        geom_line()+ 
        theme_bw(base_size = 20) +
        theme(axis.text.x=element_text(angle=90, hjust=1)) +
        ylab("Average kWh per minute") + 
        xlab("Time of Day") + 
        ggtitle("Average kWh by time of day Jan 2010") +
        coord_cartesian(ylim = c(0,0.03)) +
        
        scale_size_manual(values=c(1, 1, 3, 1), guide = FALSE)+
        scale_color_manual(values=c("#FF9F1C", 
                                    "#E71D36", 
                                    "#011627", 
                                    "#2EC4B6"), 
                           labels = c("Kitchen", 
                                      "Laundry", 
                                      "Water/Air", 
                                      "Rest"))

g.jan10



#July 2009
dayZoneJul <- dayZone %>% filter(month == 7) %>%
        group_by(hour, quarter) %>% 
        summarise(sm1 = mean(sm1), 
                  sm2 = mean(sm2), 
                  sm3 = mean(sm3), 
                  sm4 = mean(sm4))

dayZoneJul.long <- dayZoneJul %>% gather("sm1", "sm2", "sm3", "sm4", 
                                         key = "sm", 
                                         value = "avgKwhpm")

dayZoneJul.long$time <- paste(dayZoneJul.long$hour, 
                              dayZoneJul.long$quarter, 
                              sep = ".") %>% as.numeric()



g.jul09 <- ggplot(dayZoneJul.long, aes(time, avgKwhpm, 
                                       group = sm, 
                                       colour = sm, 
                                       size = sm)) +
        
        geom_line()+ 
        theme_bw(base_size = 20) +
        theme(axis.text.x=element_text(angle=90, hjust=1)) +
        ylab("Average kWh per minute") + 
        xlab("Time of Day") + 
        ggtitle("Average kWh by time of day Jul 2009") +
        coord_cartesian(ylim = c(0,0.03)) +
        
        scale_size_manual(values=c(1, 1, 3, 1), guide = FALSE)+
        scale_color_manual(values=c("#FF9F1C", 
                                    "#E71D36", 
                                    "#011627", 
                                    "#2EC4B6"), 
                           labels = c("Kitchen", 
                                      "Laundry", 
                                      "Water/Air", 
                                      "Rest"))
g.jul09

#compare
grid.arrange(g.jan10, g.jul09)