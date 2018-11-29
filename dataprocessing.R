#Load libraries
source("scripts/libraries.R")

#load data (separated by ;)
data <- read_csv2("data/household_power_consumption.txt")

#Structural EDA
str(data) 
data %>% sapply(function(x) sum(is.na(x)))

#Change data types [1:2] time, [3:9] time series
dData <- data[,1:2]
dData$datetime <- paste(dData$Date, dData$Time, sep = " ")
dData$datetime <- dData$datetime %>% dmy_hms()
dData$Date <- dData$Date %>% dmy()

str(dData)
dData %>% sapply(function(x) sum(is.na(x)))

numData <- data[,3:9]
numData <- numData %>% sapply(as.numeric) %>% as.data.frame()
str(numData)
numData %>% sapply(function(x) sum(is.na(x)))


#Check for NA conversion
NAindex <- which(is.na(numData$Global_active_power))
NAcheck <- data[NAindex,] #NAs seem consistent with '?' in original data.


#Recombine type-amended datasets (still containing NAs)
comData <- bind_cols(dData, numData)
tail(comData)
tail(data)

#Add year, month, day fields to allow easier subsetting
comData <- comData %>% mutate(year = year(datetime), month = month(datetime), day = day(datetime))

save(comData, file = "output/comData.RDS")
load("output/comData.RDS")

#check for DST issues
DST07 <- comData %>% filter(Date == "25/3/2007" | Date == "28/10/2007") # no issues found


###############NA ISSUES################################ 
comData %>% sapply(function(x) sum(is.na(x)))

#Collate NAs (1440mins = 24hrs) (data period  2006-12-16 17:24:00 - 2010-11-26 21:02:00)
NAident <- comData[!complete.cases(comData),]
NAident <- NAident %>% group_by(Date) %>%
        summarise(total = n())
NAident$Date <- NAident$Date %>% dmy()

majNA <- NAident %>% filter(total >= 1200) # more than 20 hours blank
midNA <- NAident %>% filter(total > 60 & total < 1200) #between 1-20 hours blank
minNA <- NAident %>% filter(total <60) # less than 1 hour

#Plot time period with missing values to compare against imputation
#Subset 2 month period in 2010 with NA, subset same period in 2008 (fewer NAs) for comparison
#Show daily average to allow clearer plotting.
impFilt10 <- comData %>% filter(datetime > "2010-06-01" & datetime < "2010-07-30")
impFilt08 <- comData %>% filter(datetime > "2008-06-01" & datetime < "2008-07-30")
impFiltConv10 <- impFilt10 %>% group_by(Date) %>% summarise(S3 = mean(Sub_metering_3))
impFiltConv08 <- impFilt08 %>% group_by(Date) %>% summarise(S3 = mean(Sub_metering_3))

#visualise initial missing data + comparison period
g.NATSplot10 <- ggplot(impFiltConv10, aes(Date, S3))+
        geom_line()
g.NATSplot10

g.NATSplot08 <- ggplot(impFiltConv09, aes(Date, S3))+
        geom_line()
g.NATSplot08

#Imputation 
tsS3impFilt10 <- ts(impFilt10$Sub_metering_3)
tsS3impFilt10 <- tsS3impFilt10 %>% na.interpolation(option = "linear")

#addition to dataframe + summary
impFilt10$S3 <- tsS3impFilt10
impConv10 <- impFilt10 %>% group_by(Date) %>% summarise(S3 = mean(S3))

#comparison plotting
g.NAimp10 <- ggplot()+
        geom_line(data = impConv10, aes(Date, S3), colour = "red")+
        geom_line(data = impFiltConv10, aes(Date, S3))
        
g.NAimp10

grid.arrange(g.NAimp10, g.NATSplot08)

#################IMPUTATION of NAs#######################
preImp <- comData
preImp %>% sapply(function(x) sum(is.na(x)))

preImp[4:10] <- preImp[4:10] %>% sapply(ts)
preImp[4:10] <- preImp[4:10] %>% sapply(function(x) na.interpolation(x, option = "linear"))

preImp %>% sapply(function(x) sum(is.na(x))) # zero NAs

##check if correct
impCheck <- comData$Sub_metering_3 %>% as.data.frame() %>% cbind(preImp$Sub_metering_3)
impCheckCC <- impCheck %>% filter(complete.cases(impCheck))
identical(impCheckCC$., impCheckCC$`preImp$Sub_metering_3`) # pre-existing values are the same = no errors due to imputation
impCheck <- impCheck %>% filter(!complete.cases(impCheck))

#Visualisation of imputation

impPlot <- preImp %>% filter(datetime > "2010-06-01" & datetime < "2010-07-30")
impPlotFilt <- impPlot %>% group_by(Date) %>% summarise(S3 = mean(Sub_metering_3))

g.impPlot <- ggplot()+
        geom_line(data = impPlotFilt, aes(Date, S3), colour = "#E71D36", size = 2)+
        geom_line(data = impFiltConv10, aes(Date, S3), colour = "#011627", size = 2)+
        theme_bw() +
        ylab("Kilowatt Hours") + 
        xlab("Date") + 
        ggtitle("Imputed Missing Values")
g.impPlot

grid.arrange(g.impPlot, g.NAimp10, g.NATSplot08)



#####POST-IMPUTATION CLEAN UP###############
impData <- preImp



#######FEATURE ENGINEERING###########
#(global_active_power*1000/60 - sub_metering_1 - sub_metering_2 - sub_metering_3) - missing energy metering

impData$sm4 <- (impData$Global_active_power*1000/60) - 
        (impData$Sub_metering_1 + impData$Sub_metering_2 + impData$Sub_metering_3)

impData$totkwh = impData$Sub_metering_1 + impData$Sub_metering_2 + impData$Sub_metering_3 + impData$sm4

#arrange columns in more logical order
impData <- impData %>% select(year, month, day, Date, Time,  datetime, Global_active_power, Global_reactive_power, Voltage, Global_intensity,
                              Sub_metering_1, Sub_metering_2, Sub_metering_3, sm4, totkwh)

#Give columns more useable names
impData <- impData %>% rename(sm1 = Sub_metering_1, sm2 = Sub_metering_2, sm3 = Sub_metering_3, kwhpm = totkwh)


#convert watt-hour values to kilowatt-hour values (prices usually given in kwh)
impData[11:15] <- impData[11:15]/1000

#check that kwh usage per day is reasonable (26 = high/large household)
dayUse <- impData %>% group_by(Date) %>% summarise(dayKWH = sum(totkwh))
dayAvg <- mean(dayUse$dayKWH)

save(impData, file = "output/impData.RDS")
load("output/impData.RDS")



#############################ADDING TIME OF DAY COSTS####################################

timeData <- impData
timeData$weekday <- timeData$datetime %>% wday(label = T, abbr = F) 
timeData$hour <- timeData$datetime %>% hour() 

timeData <- timeData %>% select(weekday, hour, everything())
timeData$timecost <- NA

##Weekend Low - 7.91
timeData[(timeData$weekday == "Saturday" | timeData$weekday == "Sunday") & 
                         (timeData$hour >= 0 & timeData$hour < 7),]$timecost <- 7.91

##Weekend Standard - 16.27
timeData[(timeData$weekday == "Saturday" | timeData$weekday == "Sunday") & 
                 (timeData$hour <= 23 & timeData$hour >= 7),]$timecost <- 16.27


#weekday low - 7.91
timeData[(timeData$weekday != "Saturday" & timeData$weekday != "Sunday") & 
                 (timeData$hour >= 0 & timeData$hour < 7),]$timecost <- 7.91

#weekday standard - 16.27
timeData[(timeData$weekday != "Saturday" & timeData$weekday != "Sunday") & 
                 (timeData$hour >= 7 & timeData$hour < 16),]$timecost <- 16.27

#weekday high - 32.55
timeData[(timeData$weekday != "Saturday" & timeData$weekday != "Sunday") & 
                 (timeData$hour >= 16 & timeData$hour < 20),]$timecost <- 32.55

#weeknight - 16.27
timeData[(timeData$weekday != "Saturday" & timeData$weekday != "Sunday") & 
                 (timeData$hour >= 20 & timeData$hour <= 23),]$timecost <- 7.91


#Check correctly insterted
sum(is.na(timeData$timecost)) # 0 NA
timeCheck <- timeData %>% group_by(weekday, hour) %>% summarise(mean = mean(timecost)) # averages correspond with costs
timeData$cost <- (timeData$kwhpm*timeData$timecost)/100 #convert to units of £

save(timeData, file = "output/timeData.RDS")
load("output/timeData.RDS")

