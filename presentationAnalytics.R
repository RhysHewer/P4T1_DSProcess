source("scripts/libraries.R")
load("output/timeData.RDS")

##########WHAT IS MY CONSUMPTION TREND?############
#total period by months
conData <- timeData %>% group_by(year, month) %>% 
        summarise(monthAvg = mean(kwhpm)*60*24) # per month average kwh per day

TSconData <- conData$monthAvg %>% ts(frequency = 12, start = c(2006,12))

#decompose to find trend
conDec <- stl(TSconData, "periodic")
autoplot(conDec)

conDec_trend <- conDec$time.series[,2]
autoplot(conDec_trend)

####forecast next 6 months

#Training set
conDecTrain <- conDec_trend[1:42] %>% ts(frequency = 12, start = c(2006,12))


#Linear regression - quadratic
conMod.q <- tslm(conDecTrain ~ trend + I(trend^2))
conFit.q <- forecast(conMod.q, h = 6)
autoplot(conFit.q) + autolayer(fitted(conFit.q)) + autolayer(conDec_trend) 
checkresiduals(conFit.q)
accuracy(conFit.q, conDec_trend)

#Holt exponential smoothing
conMod.h <- ets(conDecTrain)
conFit.h <- forecast(conMod.h, h = 6)
autoplot(conFit.h) + autolayer(fitted(conFit.h)) + autolayer(conDec_trend)
checkresiduals(conFit.h)
accuracy(conFit.h, conDec_trend)

#best model - Holt exponential smoothing: apply to complete data
conModFin.h <- ets(conDec_trend)
conFitFin.h <- forecast(conModFin.h, h = 6)
autoplot(conFitFin.h)


###Plotting presentation graph



############HOW MUCH AM I SPENDING?###############

#date split/cost of use
last14 <- timeData %>% filter(Date >= "2010-03-11" & Date < "2010-03-24")
useCost <- sum(last14$cost)

useDemo <- last14 %>% group_by(hour) %>% summarise(use = mean(kwhpm)*60, cost = mean(cost), timecost = mean(timecost))

#daily use graph
g.dayUse <- ggplot(useDemo, aes(hour, use)) +
        geom_line(colour = "#011627", size = 2) +
        theme_bw(base_size = 20) +
        ylab("Kilowatt Hours") + 
        ggtitle("Hourly Energy Use (14 day avg) & Energy Cost per Hour (pence)") +
        theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
        coord_cartesian(xlim = c(0,23))
g.dayUse

g.timecost <- ggplot(useDemo, aes(hour, timecost, fill = timecost)) +
        geom_col() +
        scale_fill_gradient(low = "#E8BBC1", high = "#E71D36", guide=FALSE) +
        theme_bw(base_size = 20) +
        ylab("Cost per Kilowatt Hour") + 
        xlab("Time (Hour of Day)") +
        coord_cartesian(xlim = c(0,23))
g.timecost


grid.newpage()
grid.draw(rbind(ggplotGrob(g.dayUse), ggplotGrob(g.timecost), size = "first"))


############HOW MUCH WILL MY BILL BE?##################

####Forecast to end of current quarter/next quarter.
##Take 6 quarters sample. Need daily useage.

###To create models
#from q2 2008 - q3 2010 (need 2 years to allow trend modelling)
#Today's "Date" = 12 MAY 2010 to create models

#Actual Costs - current quarter
qCost <- timeData %>% filter(quarter == "2010.2" & Date < "2010-05-12")
qUseCost <- sum(qCost$cost) #in £


#Averaged daily data for prediction
qTime <- timeData %>% filter(quarter >= "2008.2" & quarter < "2010.4")
qTimeDay <- qTime %>% group_by(Date) %>% summarise(avgKwhpD = mean(kwhpm)*60*24, timecost = mean(timecost))
qTimeDay$poundCost <- (qTimeDay$avgKwhpD * qTimeDay$timecost)/100
TSqTimeDay <- qTimeDay$avgKwhpD %>% ts(start = c(2008, 92), frequency = 365.25)

#work out horizon to end of next quarter
fcHor1 <- qTime$Date %>% tail(1)
fcHor2 <- "2010-05-12" %>% ymd() # fake "today"
fcHor <- interval(fcHor2,fcHor1) %>% as.duration()/(60*60*24) 
fcHor <- fcHor %>% as.numeric() %>% +1 #+1 to include today's date in prediction

#####start modelling exploration
autoplot(TSqTimeDay) # general plot of time series

#split to test/train
testPeriodEnd <- length(TSqTimeDay)
testPeriodStart <- length(TSqTimeDay) - (fcHor-1)
qTest <- TSqTimeDay[testPeriodStart:testPeriodEnd]
qTrain <- TSqTimeDay[1:(testPeriodStart-1)] %>% ts(start = c(2008, 92), frequency = 365.25)

autoplot(qTrain)

##decompose to look for trend/seasonality - allows better model choice
qDec <- stl(qTrain, "periodic")
autoplot(qDec) # shows both trend and seasonality
qDecM <- mstl(qTrain)
autoplot(qDecM) # no seasonality beyond annual

###Model choices (trend+seasonality)

#seasonal naive
qFit.sn <- snaive(qTrain, h = fcHor)
autoplot(qFit.sn) + autolayer(fitted(qFit.sn)) + autolayer(TSqTimeDay)
checkresiduals(qFit.sn)
acc.sn <- accuracy(qFit.sn, qTest)
save(qFit.sn, file = "output/qFitsn.RDS")

#linear regression
qMod.reg <- tslm(qTrain ~ trend)
qFit.reg <- forecast(qMod.reg, h = fcHor)
autoplot(qFit.reg) + autolayer(fitted(qFit.reg)) + autolayer(TSqTimeDay)
checkresiduals(qFit.reg)
acc.reg <- accuracy(qFit.reg, qTest)

#STL/Arima
qMod.st <- stlm(qTrain, method = "arima") 
qFit.st <- forecast(qMod.st, h = fcHor)
autoplot(qFit.st) + autolayer(fitted(qFit.st)) + autolayer(TSqTimeDay)
checkresiduals(qFit.st)
acc.st <- accuracy(qFit.st, qTest)

#compare accuracy:
accComp <- acc.sn %>% as.data.frame()
accComp <- accComp %>% rbind(acc.reg, acc.st)
accComp <- accComp %>% rownames_to_column()
accComp$Model <- c("snTrain", "snTest", "regTrain", "regTest", "arTrain", "arTest")
accComp$rowname <- NULL
accComp <- accComp %>% select(Model, everything())
accComp

####Forecasting future
#Today's date = 1 Oct 2010 for unknown future forecasting.

#work out horizon to end of next quarter (q1, 2011)
finalHor1 <- "2011-03-31" %>% ymd() #end of next quarter
finalHor2 <- "2010-10-01" %>% ymd() # fake "today"
finalHor <- interval(finalHor2,finalHor1) %>% as.duration()/(60*60*24) 
finalHor <- finalHor %>% as.numeric() %>% +1 #+1 to include today's date in prediction        

#Using STL/Arima model and full dataset (q2,2008- q4, 2010)       
qMod.final <- stlm(TSqTimeDay, method = "arima") 
qFit.final <- forecast(qMod.final, h = finalHor)
autoplot(qFit.final)

#fortify to DF + add date column
qFit <- qFit.final %>% fortify()
qFit <- qFit %>% rename(avgKwhpD = Data, preds = 'Point Forecast')
qFit$date <- seq(from = as.Date("2008-04-01"), to = as.Date(finalHor1), by = 'day')

#monetise predictions (combine actual and predictions into one column and multiply by average kWh cost)
avgKwh <- mean(qTimeDay$timecost)
qFit$cost <- rowSums(qFit[,c("avgKwhpD", "preds")]*avgKwh/100, na.rm = TRUE)
qCurr <- qFit %>% filter(date >= "2010-10-01" & date <= "2010-12-31")
qCurrCost <- sum(qCurr$cost)
qNxt <- qFit %>% filter(date >= "2011-01-01" & date <= "2011-03-31")
qNxtCost <- sum(qNxt$cost)


##################WHAT IS USING MOST ELECTRICITY AND WHEN?#####################

#extract sum2009-sum2010
dayZone<- timeData %>% filter(Date >= "2009-07-01" & Date <= "2010-06-30")

#create minute feature and collect into 15 min chunks
dayZone$quarter <- NA
dayZone <- dayZone %>% mutate(minute = minute(datetime)) %>% 
        select(weekday, minute, hour, quarter, everything())


dayZone[(dayZone$minute >= 0 & dayZone$minute < 15),]$quarter <- 1
dayZone[(dayZone$minute >= 15 & dayZone$minute < 30),]$quarter <- 2
dayZone[(dayZone$minute >= 30 & dayZone$minute < 45),]$quarter <- 3
dayZone[(dayZone$minute >= 45 & dayZone$minute <= 59),]$quarter <- 4

#plot day average pattern for 2009 - Jan, July
#Jan 2010
dayZoneJan <- dayZone %>% filter(month == 1) %>%
        group_by(hour, quarter) %>% 
        summarise(sm1 = mean(sm1), sm2 = mean(sm2), sm3 = mean(sm3), sm4 = mean(sm4))

dayZoneJan.long <- dayZoneJan %>% gather("sm1", "sm2", "sm3", "sm4", key = "sm", value = "avgKwhpm")

dayZoneJan.long$time <- paste(dayZoneJan.long$hour, dayZoneJan.long$quarter, sep = ".") %>% as.numeric()



g.jan10 <- ggplot(dayZoneJan.long, aes(time, avgKwhpm, group = sm, colour = sm)) +
        geom_line(size = 2)+ 
        theme_bw(base_size = 20) +
        theme(axis.text.x=element_text(angle=90, hjust=1)) +
        ylab("Average Kilowatt Hours per minute") + 
        xlab("Time of Day") + 
        ggtitle("Average kWh by time of day Jan 2010") +
        scale_color_discrete(labels = c("Kitchen", "Laundry", "Water/Air", "Rest")) +
        coord_cartesian(ylim = c(0,0.03))
g.jan10

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
        scale_color_discrete(labels = c("Kitchen", "Laundry", "Water/Air", "Rest")) +
        coord_cartesian(ylim = c(0,0.03))
g.jul09

#compare
grid.arrange(g.jan10, g.jul09)

##Focus on what we can analyse - sm3

#overall trend
TSsm3 <- timeData %>% group_by(Date) %>% summarise(avgKwhPD = mean(sm3)*60*24)
TSsm3 <- TSsm3$avgKwhPD %>% ts(frequency = 365.25, start = c(2006, 350))

sm3Dec <- stl(TSsm3, "periodic")
sm3Dec <- sm3Dec$time.series[,2]
autoplot(sm3Dec)


#summer 2009 - summer 10 usage
sm3Year <- dayZone %>% group_by(Date) %>% summarise(avgKwhPD = mean(sm3)*60*24)

g.sm3Year <- ggplot(sm3Year, aes(Date, avgKwhPD)) +
        geom_line() +
        geom_smooth()
g.sm3Year


