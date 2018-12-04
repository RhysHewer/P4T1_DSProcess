source("scripts/libraries.R")
load("output/timeData.RDS")

#############LINEAR MODELLING##############

####Average model (12 weeks/4 weeks) - extract 16 weeks from 2009
avgMod <- timeData %>% filter(Date >= "2009-07-07" & Date <= "2009-10-26")
avgMod <- avgMod %>%  group_by(Date) %>%
        summarise(totalAvg = mean(kwhpm)) 
avgMod$totalAvg <- avgMod$totalAvg

#convert to TS and predict 4 weeks
avgModTS <- avgMod %>% filter(Date >= "2009-07-07" & Date <= "2009-09-28") %>% 
        select(totalAvg) %>% ts(frequency = 7)

avgModPred <- meanf(avgModTS, h = 4)
autoplot(avgModPred)
checkresiduals(avgModPred) # Residuals check fit of model to 'train' data

#cross validation
avgModFull <- avgMod$totalAvg %>% ts(frequency = 7)
avgModFullPred <- tsCV(avgModFull, meanf, h = 4)
avgModFullPred^2 %>% mean(na.rm=TRUE) %>% sqrt() # RMSE for model
avgModFullOrig <- avgMod %>% filter(Date >= "2009-09-29" & Date <= "2009-10-26") %>% 
        select(totalAvg) %>% ts()

accuracy(avgModFullPred,avgModFullOrig)

###Seasonal Naive model

snModPred <- snaive(avgModTS, h = 4)
autoplot(snModPred)
checkresiduals(snModPred) # Residuals check fit of model to 'train' data

#cross validation
snModFull <- avgMod$totalAvg %>% ts(frequency = 7)
snModFullPred <- tsCV(snModFull, snaive, h = 4)
snModFullPred^2 %>% mean(na.rm=TRUE) %>% sqrt() # RMSE for model

avgModFullOrig <- avgMod %>% filter(Date >= "2009-09-29" & Date <= "2009-10-26") %>% 
        select(totalAvg) %>% ts()

accuracy(snModFullPred,avgModFullOrig)

####
