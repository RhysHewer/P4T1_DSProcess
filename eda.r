source("scripts/libraries.R")
load("output/comData.RDS")
load("output/impData.RDS")
load("output/timeData.RDS")

##########EDA - DESCRIPTIVE STATISTICS############

#daily avg electricity use 
dayUse <- impData %>% group_by(Date) %>% summarise(dayKWH = sum(kwhpm))
dayAvg <- mean(dayUse$dayKWH)

wkdayUse <- timeData %>% group_by(weekday) %>% summarise(mean = mean(kwhpm))
wkdayUse$mean <- wkdayUse$mean *60*24

#daily avg for each sub-meter
dayUseS1 <- impData %>% group_by(Date) %>% summarise(dayKWH = sum(sm1))
dayAvgS1 <- mean(dayUseS1$dayKWH)

dayUseS2 <- impData %>% group_by(Date) %>% summarise(dayKWH = sum(sm2))
dayAvgS2 <- mean(dayUseS2$dayKWH)

dayUseS3 <- impData %>% group_by(Date) %>% summarise(dayKWH = sum(sm3))
dayAvgS3 <- mean(dayUseS3$dayKWH)

dayUseS4 <- impData %>% group_by(Date) %>% summarise(dayKWH = sum(sm4))
dayAvgS4 <- mean(dayUseS4$dayKWH)

SMdayAvgs <- data.frame(c("subMeter1", "subMeter2", "subMeter3", "subMeter4"), 
                        c(dayAvgS1, dayAvgS2, dayAvgS3, dayAvgS4))
colnames(SMdayAvgs) <- c("SubMeter", "dayAvg")

g.dayAvg <- ggplot(SMdayAvgs, aes(SubMeter, dayAvg)) +
        geom_col() +
g.dayAvg


#monthly total avg use (full months only)
monthUseFilt <- impData %>% filter(datetime > "2007-01-01" & datetime < "2010-10-31")
monthUse <- monthUseFilt %>% group_by(month) %>% summarise(meanPerMin = mean(kwhpm))
monthUse$month <- monthUse$month %>% month()
monthUse <- monthUse %>% mutate(days = days_in_month(month))
monthUse <- monthUse %>% mutate(monthMean = days*meanPerMin*60*24)
monthUse <- monthUse %>% mutate(dayMeanperMonth = meanPerMin*60*24)
monthUse$month <- monthUse$month %>% as.factor()

g.monthAvg <- ggplot(monthUse, aes(month, monthMean)) +
        geom_col()
g.monthAvg

#yearly avg electricity use 2009 (final full year)
yearUse <- impData %>% filter(year == 2009)
yearTot <- sum(yearUse$kwhpm)

#min/max/ranges
summary(impData)

#Submeter 4 graph showing missing data

missPow <- impData %>% filter(datetime > "2010-09-01" & datetime < "2010-09-30")
missPow <- missPow %>% mutate(subTot = sm1 + sm2 + sm3)
missPow <- missPow %>% group_by(Date) %>% summarise(subTot = sum(subTot), tot = sum(kwhpm))
missPow$Date <- missPow$Date %>% ymd()
str(missPow)

g.miss <- ggplot() +
        geom_area(data = missPow, aes(Date, tot), fill = "#FF9F1C") +
        geom_area(data = missPow, aes(Date, subTot), fill = "#e0e1e2") +
        theme_bw() +
        ylab("Kilowatt Hours") + 
        xlab("Date") + 
        ggtitle("Missing Kilowatt Hours")
g.miss

##########VISUALISING + CHECKING COSTS################

#check day and year averages - seem reasonable
dayCost <- mean(timeData$cost) *24*60
yearCost <- dayCost*365

#proof of concept - avg day use, 14 days
proofTest <- timeData %>% filter(Date >= "2010-03-11" & Date < "2010-03-24")
proofTest <- proofTest %>% group_by(hour) %>% summarise(use = mean(kwhpm), cost = mean(cost), timecost = mean(timecost))

#daily use graph
g.dayUse <- ggplot(proofTest, aes(hour, use)) +
        geom_line(colour = "#011627", size = 2) +
        theme_bw() +
        ylab("Kilowatt Hours") + 
        ggtitle("Avg. Daily Energy Use & Energy Cost per Hour (pence)") +
        theme(axis.title.x = element_blank(), axis.text.x = element_blank())
g.dayUse

g.timecost <- ggplot(proofTest, aes(hour, timecost, fill = timecost)) +
        geom_col() +
        scale_fill_gradient(low = "#E8BBC1", high = "#E71D36", guide=FALSE) +
        theme_bw() +
        ylab("Cost per Kilowatt Hour") + 
        xlab("Time (Hour of Day)")
g.timecost


grid.newpage()
grid.draw(rbind(ggplotGrob(g.dayUse), ggplotGrob(g.timecost), size = "last"))

#####PLOTTING ROOM USE#####

roomUse <- timeData %>% filter(Date >= "2009-03-23" & Date < "2010-03-24")
roomUse <- roomUse %>% group_by(hour) %>% 
        summarise(use = mean(kwhpm), cost = mean(cost),
                  sm1 = mean(sm1), sm2 = mean(sm2), sm3 = mean(sm3), sm4 = mean(sm4))

g.rooms <- ggplot() +
        geom_line(data = roomUse, aes(hour, sm1), colour = "red") +
        geom_line(data = roomUse, aes(hour, sm2), colour = "blue") +
        geom_line(data = roomUse, aes(hour, sm3), colour = "green") +
        geom_line(data = roomUse, aes(hour, sm4), colour = "black") +
        theme_bw()
g.rooms

