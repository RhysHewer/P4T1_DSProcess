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

