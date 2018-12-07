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

####Forecast to end of current quarter
