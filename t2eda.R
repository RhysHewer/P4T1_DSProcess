source("scripts/libraries.R")
load("output/timeData.RDS")


########ENERGY BY ZONE OVER DAY################
#extract 2009
dayZone<- timeData %>% filter(Date >= "2009-01-01" & Date <= "2009-12-31")

#create minute feature and collect into 15 min chunks
dayZone$quarter <- NA
dayZone <- dayZone %>% mutate(minute = minute(datetime)) %>% 
        select(weekday, minute, hour, quarter, everything())


dayZone[(dayZone$minute >= 0 & dayZone$minute < 15),]$quarter <- 1
dayZone[(dayZone$minute >= 15 & dayZone$minute < 30),]$quarter <- 2
dayZone[(dayZone$minute >= 30 & dayZone$minute < 45),]$quarter <- 3
dayZone[(dayZone$minute >= 45 & dayZone$minute <= 59),]$quarter <- 4

#plot day average pattern for 2009 - Jan, July
#Jan 2009
dayZoneJan <- dayZone %>% filter(month == 1) %>%
        group_by(hour, quarter) %>% 
        summarise(sm1 = mean(sm1), sm2 = mean(sm2), sm3 = mean(sm3), sm4 = mean(sm4))

dayZoneJan.long <- dayZoneJan %>% gather("sm1", "sm2", "sm3", "sm4", key = "sm", value = "avgKwhpm")

dayZoneJan.long$time <- paste(dayZoneJan.long$hour, dayZoneJan.long$quarter, sep = ".") %>% as.numeric()



g.jan09 <- ggplot(dayZoneJan.long, aes(time, avgKwhpm, group = sm, colour = sm)) +
        geom_line(size = 2)+ 
        theme_bw(base_size = 20) +
        theme(axis.text.x=element_text(angle=90, hjust=1)) +
        ylab("Average Kilowatt Hours per minute") + 
        xlab("Time of Day") + 
        ggtitle("Average kWh by time of day Jan 2009") +
        scale_color_discrete(labels = c("Kitchen", "Laundry", "Water/Air", "Rest"))
g.jan09

#July 2009
dayZoneJul <- dayZone %>% filter(month == 7) %>%
        group_by(hour, quarter) %>% 
        summarise(sm1 = mean(sm1), sm2 = mean(sm2), sm3 = mean(sm3), sm4 = mean(sm4))

dayZoneJul.long <- dayZoneJul %>% gather("sm1", "sm2", "sm3", "sm4", key = "sm", value = "avgKwhpm")

dayZoneJul.long$time <- paste(dayZoneJul.long$hour, dayZoneJul.long$quarter, sep = ".") %>% as.numeric()



g.jul09 <- ggplot(dayZoneJul.long, aes(time, avgKwhpm, group = sm, colour = sm)) +
        geom_line(size = 2)+ 
        theme_bw(base_size = 20) +
        theme(axis.text.x=element_text(angle=90, hjust=1)) +
        ylab("Average Kilowatt Hours per minute") + 
        xlab("Time of Day") + 
        ggtitle("Average kWh by time of day Jul 2009") +
        scale_color_discrete(labels = c("Kitchen", "Laundry", "Water/Air", "Rest"))
g.jul09

#compare
grid.arrange(g.jan09, g.jul09)


########ZONE USE BY DAY###########################

#general use per day
subZone <- dayZone %>% group_by(weekday) %>% 
        summarise(sm1 = mean(sm1), sm2 = mean(sm2), sm3 = mean(sm3), sm4 = mean(sm4))

g.gen <- ggplot(subZone, aes(weekday, sm1)) +
        geom_col() +
        theme_bw(base_size = 20) +
        ylab("Average Kilowatt Hours per minute") + 
        xlab("Day of Week") + 
        ggtitle("Average Power use in Kitchen per day")
g.gen

#avg use over day split by weekday (per sub-meter)
wkdayUse <- dayZone %>% group_by(weekday, hour, quarter) %>% 
        summarise(sm1 = mean(sm1), sm2 = mean(sm2), sm3 = mean(sm3), sm4 = mean(sm4))

wkdayUse$time <- paste(wkdayUse$hour, wkdayUse$quarter, sep = ".") %>% as.numeric()

#sm1 - kitchen
g.sm1 <- ggplot(wkdayUse, aes(time, sm1, colour = weekday)) +
        geom_line(size = 2) +
        facet_grid(weekday ~ .) +
        theme_bw(base_size = 20) +
        ylab("Average Kilowatt Hours per minute") + 
        xlab("Time of Day (hrs)") + 
        ggtitle("Power use in Kitchen by day")
g.sm1

#sm2 - Laundry
g.sm2 <- ggplot(wkdayUse, aes(time, sm2, colour = weekday)) +
        geom_line(size = 2) +
        facet_grid(weekday ~ .) +
        theme_bw(base_size = 20) +
        ylab("Average Kilowatt Hours per minute") + 
        xlab("Time of Day (hrs)") + 
        ggtitle("Power use in Laundry Room by day")
g.sm2

#sm3 - water heater/air conditioner
g.sm3 <- ggplot(wkdayUse, aes(time, sm3, colour = weekday)) +
        geom_line(size = 2) +
        facet_grid(weekday ~ .) +
        theme_bw(base_size = 20) +
        ylab("Average Kilowatt Hours per minute") + 
        xlab("Time of Day (hrs)") + 
        ggtitle("Power use of Water Heater/Air Conditioner by day")
g.sm3

#sm4 - rest of house
g.sm4 <- ggplot(wkdayUse, aes(time, sm4, colour = weekday)) +
        geom_line(size = 2) +
        facet_grid(weekday ~ .) +
        theme_bw(base_size = 20) +
        ylab("Average Kilowatt Hours per minute") + 
        xlab("Time of Day (hrs)") + 
        ggtitle("Power use of Rest of House by day")
g.sm4


##########