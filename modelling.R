source("scripts/libraries.R")
load("output/comData.RDS")
load("output/impData.RDS")
load("output/timeData.RDS")

#############BASIC MODELLING - SEASONAL NAIVE###############
predData <- timeData %>% filter(Date >= "2008-08-27" & Date <= "2010-08-26")

predDataMonth <- predData %>% group_by(year, month) %>% summarise(kwhmonth = sum(kwhpm))
predDataMonth$date <- paste(predDataMonth$year, predDataMonth$month, 1, sep = " ") %>% ymd()
predDataMonth$kwhmonth <- predDataMonth$kwhmonth %>% ts(start = c(2008, 8), frequency = 12)

kwhPred <- predDataMonth$kwhmonth

testPred <- snaive(kwhPred, h = 6)
testPredFrame <- testPred %>% fortify()
testPredFrame <- testPredFrame %>% rename(Preds = "Point Forecast")
testPredFrame$totPreds <- rowSums(testPredFrame[,c("Data" , "Preds")], na.rm = TRUE)

g.pred <- ggplot() +
        geom_line(data = testPredFrame, aes(Index, totPreds), colour = "#FF9F1C", size = 2) +
        geom_line(data = testPredFrame, aes(Index, Data), colour = "#011627", size = 2) +
        theme_bw(base_size = 24) +
        ylab("Kilowatt Hours") + 
        xlab("Date") + 
        ggtitle("6 Month Prediction")
g.pred

billPred = sum(testPredFrame$Preds, na.rm = TRUE)*0.1437
