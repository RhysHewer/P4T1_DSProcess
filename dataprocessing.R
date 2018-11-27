#Load libraries
source("scripts/libraries.R")

#load data (separated by ;)
data <- read_csv2("data/household_power_consumption.txt")

#Structural EDA
str(data) 
data %>% sapply(function(x) sum(is.na(x)))

#Change data types [1:2] time, [3:9] numeric
dData <- data[,1:2]

numData <- data[,3:9]
numData <- numData %>% sapply(as.numeric) %>% as.data.frame()
str(numData)
numData %>% sapply(function(x) sum(is.na(x)))

#Check for NA conversion
NAindex <- which(is.na(numData$Global_active_power))
NAcheck <- data[NAindex,] #NAs seem consistent with '?' in original data.

#Re

