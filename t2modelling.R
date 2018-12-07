source("scripts/libraries.R")
load("output/timeData.RDS")

#############SIMPLE MODELLING##############
#16 week period
#meanf model
avgMod <- timeData %>% filter(Date >= "2009-07-07" & Date <= "2009-10-26")
avgMod <- avgMod %>%  group_by(Date) %>%
        summarise(totalAvg = mean(kwhpm)) 

TSavgMod <- avgMod$totalAvg %>% ts(frequency = 365.25, start = c(2009, 7))

fcAvg <- tsCV(TSavgMod, forecastfunction = meanf, h = 7*4) #4 weeks (7 days x 4 weeks)
accuracy(fcAvg, TSavgMod)

#drift model
fcDrift <- tsCV(TSavgMod, forecastfunction = rwf, h = 7*4) #4 weeks (7 days x 4 weeks)
accuracy(fcDrift, TSavgMod)


#use better model to forecast
fcDrift.Final <- rwf(TSavgMod, h = 7*4)
autoplot(fcDrift.Final)

#################LINEAR MODELLING#############
#1 year period
linData <- timeData %>% filter(Date >= "2009-07-07" & Date <= "2010-07-06")
linData <- linData %>%  group_by(Date) %>%
        summarise(totalAvg = mean(kwhpm)) 
linData$t <- linData %>% rowid_to_column()

TSlinData <- linData$totalAvg %>% ts(frequency = 365.25, start = c(2009, 7))
autoplot(TSlinData)
TSlinTrain <- linData %>% filter(Date < "2010-05-06")  
TSlinTrain <- TSlinTrain$totalAvg %>% ts(frequency = 365.25, start = c(2009, 7)) #303 in train, predict 62

#linear regression (trend model)
modLN <- tslm(TSlinData ~ trend)
fitLN <- forecast(modLN, h = 7*8)
autoplot(fitLN) + autolayer(fitted(fitLN))

checkresiduals(fitLN)
summary(fitLN)

##linear regression (trend model, log transform)
modLNlg <- tslm(TSlinData ~ trend, lambda = 0)
fitLNlg <- forecast(modLNlg, h = 7*8)
autoplot(fitLNlg) + autolayer(fitted(fitLNlg))

checkresiduals(fitLNlg)

##linear regression (trend model, quadratic)
modLNqd <- tslm(TSlinData ~ trend + I(trend^2))
fitLNqd <- forecast(modLNqd, h = 7*8)
autoplot(fitLNqd) + autolayer(fitted(fitLNqd))

checkresiduals(fitLNqd)
           
##linear regression (trend model, seasonal) # cannot do as seasonal is yearly pattern!!
modLNsn <- tslm(TSlinTrain ~ trend)
fitLNsn <- forecast(modLNsn, h = 64)
autoplot(fitLNsn) + autolayer(fitted(fitLNsn)) + autolayer(TSlinData)

checkresiduals(fitLNsn)
accuracy(fitLNsn, TSlinData)      

stl(TSlinTrain)
                      
################DECOMPOSITION############

#2 year period with classic decomp
year2Dec <- timeData %>% filter(Date >= "2008-07-07" & Date <= "2010-07-06")
year2Dec <- year2Dec %>%  group_by(Date) %>%
        summarise(totalAvg = mean(kwhpm)) 

TSyear2Dec <- year2Dec$totalAvg %>% ts(frequency = 7, start = 2008)
autoplot(TSyear2Dec)
y2DecA <- decompose(TSyear2Dec, type = "additive") 
g.y2DecA <- autoplot(y2DecA)
y2DecM <- decompose(TSyear2Dec, type = "multiplicative") 
g.y2DecM <- autoplot(y2DecM)

grid.arrange(g.y2DecA, g.y2DecM)

y2Dec <- y2Dec %>% fortify()
summary(y2DecA)

#cannot get the dates correct on x axis - why?
# summary does not give any useable output

#16 week period with Moving Averages
TSavgMod <- avgMod$totalAvg %>% ts(frequency = 7, start = c(2009, 7))
autoplot(TSavgMod)
ma7 <- ma(TSavgMod, 7)
g.ma7 <- autoplot(ma7)
g.ma3 <- ma(TSavgMod, 3) %>% autoplot()

grid.arrange(g.ma7, g.ma3)
summary(ma7)

#1 year period with STL
TSlinData <- linData$totalAvg %>% ts(frequency = 7, start = c(2009, 7))
autoplot(TSlinData)

stlDec <- stl(TSlinData, "periodic")
autoplot(stlDec)
summary(stlDec)


#should I be comparing the same time periods across models?

##############HOLT WINTERS###################
#How do I find non-seasonal data?
#Do I create it using decomposition?

###16 week period
TSavgMod <- avgMod$totalAvg %>% ts(frequency = 7, start = c(2009, 7))
TSavgDec <- stl(TSavgMod, "periodic")
autoplot(TSavgDec)

#extract trend
TSavgDec_trend <- TSavgDec$time.series[,2]

#exponential smoothing
hw1 <- HoltWinters(TSavgDec_trend, gamma = NULL, seasonal = "additive")
hw1fc <- forecast(hw1, h = 30)
autoplot(hw1fc)
hw1fcDf <- hw1fc %>% fortify()


####1 year period
TSlinData <- linData$totalAvg %>% ts(frequency = 7, start = c(2009, 7))
TSlinDec <- stl(TSlinData, "periodic")
autoplot(TSlinDec)

#extract trend
TSlinDec_trend <- TSlinDec$time.series[,2]

#exponential smoothing
hw2 <- HoltWinters(TSlinDec_trend, gamma = NULL, seasonal = "additive")
hw2fc <- forecast(hw2, h = 30)
autoplot(hw2fc) + autolayer(fitted(hw2fc))
hw2fcDf <- hw2fc %>% fortify()

###2year period with Holt winters
TSyear2Dec <- year2Dec$totalAvg %>% ts(frequency = 7, start = 2008)

#exponential smoothing
hw3 <- HoltWinters(TSyear2Dec, seasonal = "additive")
hw3fc <- forecast(hw3, h = 30)
g.hw3fc <- autoplot(hw3fc) + autolayer(fitted(hw3fc))
hw3fcDf <- hw3fc %>% fortify()

##compare to decomposed
DecTSyear2 <- stl(TSyear2Dec, "periodic")
DecTSyear2_trend <- DecTSyear2$time.series[,2]
hw4 <- HoltWinters(DecTSyear2_trend, gamma = NULL, seasonal = "additive")
hw4fc <- forecast(hw4, h = 30)
summary(hw4fc)
g.hw4fc <- autoplot(hw4fc) + autolayer(fitted(hw4fc))
hw4fcDf <- hw4fc %>% fortify()

grid.arrange(g.hw3fc, g.hw4fc)

###Monthly info for 2 years
y2mon <- timeData %>% filter(Date >= "2008-07-07" & Date <= "2010-07-06")
y2mon <- y2mon %>%  group_by(year, month) %>%
        summarise(totalAvg = mean(kwhpm)) 

TSy2mon <- y2mon$totalAvg %>% ts(frequency = 12, start = c(2008, 7))
autoplot(TSy2mon)


hw5 <- HoltWinters(TSy2mon, seasonal = "additive")
hw5fc <- forecast(hw5, h=2)
summary(hw5fc)
autoplot(hw5fc)

###ets selection
mod <- ets(TSyear2Dec)
summary(mod) # error - Additive, trend - None, season - Additive
modFc <- forecast(mod, h=30)
autoplot(modFc)

#############TEST/TRAIN EXPERIMENTS###################  
#create weekly period and time series
y2week <- timeData %>% filter(Date >= "2008-07-07" & Date <= "2010-07-06")
y2week <- y2week %>% group_by(year, week) %>%
        summarise(totalAvg = mean(kwhpm))

y2weekTest <- y2week %>% filter(year == 2010 & week >19)
y2weekTest <- y2weekTest$totalAvg %>% ts(frequency = 7, start = c(2010, 7))
y2weekTrain <- y2week %>% filter(!(year == 2010 & week >19))
y2weekTrain <- y2weekTrain$totalAvg %>% ts(frequency = 7, start = c(2008, 7))
TSy2week <- y2week$totalAvg %>% ts(frequency = 7, start = c(2008, 7))


#Holt winters


weekFcHW <- HoltWinters(y2weekTrain, seasonal = "additive")
weekFcHWFC <- forecast(weekFcHW, h=8)
autoplot(weekFcHWFC)

#compare to test set
accuracy(weekFcHWFC, TSy2week)

#plot
autoplot(weekFcHWFC) + autolayer(TSy2week) + autolayer(fitted(weekFcHWFC))
checkresiduals(weekFcHWFC)

###########Plotting practice#############

as.Date(paste(2014, df$Week, 1, sep="-"), "%Y-%U-%u")

DF <- weekFcHWFC %>% fortify()

DF$date <- paste(y2week$year, y2week$week, 1, sep = "-") #%>% 
DF$date <- DF$date %>% as.Date("%Y-%U-%w")

g.test <- ggplot(data = DF, aes(date, Data, group = 1)) +
        geom_line()
g.test
