source("scripts/libraries.R")
load("output/comData.RDS")

#EDA

comData <- comData %>% mutate(year = year(datetime))
comData <- comData %>% mutate(month = month(datetime))

day1 <- comData %>% filter(Date == "28/5/2007") %>% 
        select(Sub_metering_1)
day1 <- ts(day1, frequency = 60*24)
plot(day1)

month1 <- comData %>% filter(year == "2007", month == "5") %>% 
        select(Sub_metering_1)
month1 <- ts(month1, start = c(5), frequency = 60*24*31)
plot(month1)

comData <- comData %>% na.omit()
comData1 <- ts(comData$Sub_metering_1, start = c(2006, 12), frequency = 60*24*365.25)
plot(comData1)
decompose(comData1)

ggplot(comData, aes(datetime, Sub_metering_1)) + geom_line()

comData <- comData %>% na.omit()
totals <- colSums(comData[4:10]) %>% t() %>% as.data.frame() 

grand <- totals$Sub_metering_1 + totals$Sub_metering_2 + totals$Sub_metering_3