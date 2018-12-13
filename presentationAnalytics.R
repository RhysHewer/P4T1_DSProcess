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

#Exponential smoothing
conMod.h <- ets(conDecTrain)
conFit.h <- forecast(conMod.h, h = 6)
autoplot(conFit.h) + autolayer(fitted(conFit.h)) + autolayer(conDec_trend)
checkresiduals(conFit.h)
accuracy(conFit.h, conDec_trend)

#arima
conMod.a <- auto.arima(conDecTrain)
conFit.a <- forecast(conMod.a, h = 6)
autoplot(conFit.a) + autolayer(fitted(conFit.a)) + autolayer(conDec_trend)
checkresiduals(conFit.a)
accuracy(conFit.a, conDec_trend)


#best model - arima: apply to complete data
conModFin.h <- auto.arima(conDec_trend)
conFitFin.h <- forecast(conModFin.h, h = 6)
autoplot(conFitFin.h)


###Plotting presentation graph
conAct <- conFitFin.h %>% fortify()
conAct <- conAct %>% rename(preds = 'Point Forecast', lo80 = 'Lo 80', hi80 = 'Hi 80',
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
        scale_fill_gradient(low = "#FFE1B7", high = "#FF9F1C", guide=FALSE) +
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
qMod.reg <- tslm(qTrain ~ season + trend)
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

#Arima with Fourier
xreg = fourier(qTrain, K = 1)
qMod.f <- auto.arima(qTrain, xreg = xreg, seasonal = FALSE, lambda = 0)
qFit.f <- forecast(qMod.f, xreg = xreg, h = 50)
autoplot(qFit.f) + autolayer(fitted(qFit.f)) + autolayer(TSqTimeDay)
test <- qFit.f %>% fortify()
checkresiduals(qFit.f)
acc.f <- accuracy(qFit.f, qTest)

#compare accuracy:
accComp <- acc.sn %>% as.data.frame()
accComp <- accComp %>% rbind(acc.reg, acc.st)
accComp <- accComp %>% rownames_to_column()
accComp$Model <- c("snTrain", "snTest", "regTrain", "regTest", "arTrain", "arTest")
accComp$rowname <- NULL
accComp <- accComp %>% select(Model, everything())
accComp

####Forecasting future
#Today's date = 20 Oct 2010 for unknown future forecasting.

#work out horizon to end of next quarter (q1, 2011)
finalHor1 <- "2011-03-31" %>% ymd() #end of next quarter
finalHor2 <- "2010-10-20" %>% ymd() # fake "today"
finalHor <- interval(finalHor2,finalHor1) %>% as.duration()/(60*60*24) 
finalHor <- finalHor %>% as.numeric() %>% +1 #+1 to include today's date in prediction    

#adjust time series to run until 'today' (20 Oct 2010)
qTimeFinal <- timeData %>% filter(quarter >= "2008.2" & quarter <= "2010.4")
qTimeDayFinal <- qTimeFinal %>% group_by(Date) %>% summarise(avgKwhpD = mean(kwhpm)*60*24, timecost = mean(timecost))
qTimeDayFinal$poundCost <- (qTimeDayFinal$avgKwhpD * qTimeDayFinal$timecost)/100
TStime <- qTimeDayFinal$avgKwhpD
TSfinal <- TStime[1:932] %>% ts(start = c(2008, 92), frequency = 365.25)

#Using STL/Arima model and full dataset (q2,2008- q4, 2010)       
qMod.final <- stlm(TSfinal, method = "arima") 
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

##Presentation Plot

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

        geom_line(aes(Index, avgKwhpD), size = 1, colour = "#011627", alpha = 0.9) +
        geom_line(aes(Index, preds), size = 1, colour = "#FF9F1C", alpha = 0.9) +
        geom_smooth(aes(Index, preds), se = FALSE, colour = "#E71D36", size = 2) +
        
        geom_vline(xintercept = 2010.801, linetype="dotted", color = "#26532B", size=1.5) +
        geom_text(aes(x=2010.850, label="20 Oct 2010", y=5), 
                  colour="#26532B", angle=90)

g.qFit


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



g.jan10 <- ggplot(dayZoneJan.long, aes(time, avgKwhpm, group = sm, colour = sm, size = sm)) +
        geom_line()+ 
        theme_bw(base_size = 20) +
        theme(axis.text.x=element_text(angle=90, hjust=1)) +
        ylab("Average kWh per minute") + 
        xlab("Time of Day") + 
        ggtitle("Average kWh by time of day Jan 2010") +
        coord_cartesian(ylim = c(0,0.03)) +
        scale_size_manual(values=c(1, 1, 3, 1), guide = FALSE)+
        scale_color_manual(values=c("#FF9F1C", "#E71D36", "#011627", "#2EC4B6"), 
                           labels = c("Kitchen", "Laundry", "Water/Air", "Rest"))
        
g.jan10



#July 2009
dayZoneJul <- dayZone %>% filter(month == 7) %>%
        group_by(hour, quarter) %>% 
        summarise(sm1 = mean(sm1), sm2 = mean(sm2), sm3 = mean(sm3), sm4 = mean(sm4))

dayZoneJul.long <- dayZoneJul %>% gather("sm1", "sm2", "sm3", "sm4", key = "sm", value = "avgKwhpm")

dayZoneJul.long$time <- paste(dayZoneJul.long$hour, dayZoneJul.long$quarter, sep = ".") %>% as.numeric()



g.jul09 <- ggplot(dayZoneJul.long, aes(time, avgKwhpm, group = sm, colour = sm, size = sm)) +
        geom_line()+ 
        theme_bw(base_size = 20) +
        theme(axis.text.x=element_text(angle=90, hjust=1)) +
        ylab("Average kWh per minute") + 
        xlab("Time of Day") + 
        ggtitle("Average kWh by time of day Jul 2009") +
        coord_cartesian(ylim = c(0,0.03)) +
        scale_size_manual(values=c(1, 1, 3, 1), guide = FALSE)+
        scale_color_manual(values=c("#FF9F1C", "#E71D36", "#011627", "#2EC4B6"), 
                           labels = c("Kitchen", "Laundry", "Water/Air", "Rest"))
g.jul09

#compare
grid.arrange(g.jan10, g.jul09)

##Focus on what we can analyse - sm3

#overall trend
TSsm3 <- timeData %>% group_by(year, month) %>% summarise(avgKwhPD = mean(sm3)*60*24)
TSsm3 <- TSsm3$avgKwhPD %>% ts(frequency = 12, start = c(2006, 12))

sm3Dec <- stl(TSsm3, "periodic")
autoplot(sm3Dec)



##investigate possible cause of trend increase by comparing same day in 2007 and 2010, summer + winter

daySum07 <- timeData %>% filter(Date == "2007-06-01")
g.daySum07 <- ggplot(daySum07, aes(Time, sm3)) +
        geom_line() +
        coord_cartesian(ylim = c(0,0.03)) +
        ggtitle("summer 07")

daySum08 <- timeData %>% filter(Date == "2008-06-01")
g.daySum08 <- ggplot(daySum08, aes(Time, sm3)) +
        geom_line() +
        coord_cartesian(ylim = c(0,0.03)) +
        ggtitle("summer 08")

daySum09 <- timeData %>% filter(Date == "2009-06-01")
g.daySum09 <- ggplot(daySum09, aes(Time, sm3)) +
        geom_line() +
        coord_cartesian(ylim = c(0,0.03)) +
        ggtitle("summer 09")


daySum10 <- timeData %>% filter(Date == "2010-06-01")
g.daySum10 <- ggplot(daySum10, aes(Time, sm3)) +
        geom_line() +
        coord_cartesian(ylim = c(0,0.03)) + 
        ggtitle("summer 10")

grid.arrange(g.daySum07, g.daySum08, g.daySum09, g.daySum10)

##

dayWin07 <- timeData %>% filter(Date == "2007-11-01")
g.dayWin07 <- ggplot(dayWin07, aes(Time, sm3)) +
        geom_line() +
        coord_cartesian(ylim = c(0,0.03)) +
        ggtitle("winter 07")

dayWin08 <- timeData %>% filter(Date == "2008-11-01")
g.dayWin08 <- ggplot(dayWin08, aes(Time, sm3)) +
        geom_line() +
        coord_cartesian(ylim = c(0,0.03)) +
        ggtitle("winter 08")

dayWin09 <- timeData %>% filter(Date == "2009-11-01")
g.dayWin09 <- ggplot(dayWin09, aes(Time, sm3)) +
        geom_line() +
        coord_cartesian(ylim = c(0,0.03)) +
        ggtitle("winter 09")


dayWin10 <- timeData %>% filter(Date == "2010-11-01")
g.dayWin10 <- ggplot(dayWin10, aes(Time, sm3)) +
        geom_line() +
        coord_cartesian(ylim = c(0,0.03)) +
        ggtitle("winter 10")

grid.arrange(g.dayWin07, g.dayWin08, g.dayWin09, g.dayWin10)
