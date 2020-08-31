library(ggplot2)
#load data

getwd()
setwd("/Users/parthavpatel/Desktop/PhiladephiaCrimeRates")
getwd()

CrimeData <- read.csv("2_workshop_data_2.csv")
str(CrimeData)
#1. Overall trend in crimes for the whole period of the time in the dataset.The granularity should be at the Day level.
head(CrimeData$Dispatch_Date_Time)
CrimeData$DateTime <- as.POSIXct(CrimeData$Dispatch_Date_Time, format = "%Y-%m-%d %H:%M:%S", tz = "EST")
head(CrimeData$DateTime)

#Create a Date Column
CrimeData$Date <- as.Date(CrimeData$DateTime, tz = "EST")
str(CrimeData)

#Count number of crimes by day
by_date <- aggregate(CrimeData$Date, by = list(Date = CrimeData$Date), FUN = length)

#Rename Columns
colnames(by_date) <- c("Date","Total")

#Plot the results
ggplot(by_date, aes(Date, Total, color = Date)) + geom_line()

#2. Which are the most and least dangerous hours in Philadelphia?
#Get hours from Datetime column
CrimeData$Hour <- strftime(CrimeData$DateTime,format = "%H" , tz = "EST")

#Aggregate by hour
by_hour <- aggregate(CrimeData$Hour, by = list(Hour = CrimeData$Hour), FUN=length)
colnames(by_hour) <- c("Hour", "Total")
by_hour$Hour <- as.integer(by_hour$Hour)
str(by_hour)


#plot the result
ggplot(by_hour, aes(Hour,Total)) + 
  geom_line(color="Red") + 
  ggtitle("Crime By Hour") + 
  xlab("Hour of the day") +
  ylab("Total Crimes")

 
#3. Is there any seasonality in the crime rates?

#Get Months from Datetime column
CrimeData$Month <- strftime(CrimeData$DateTime,format = "%m" , tz = "EST")

#Aggregate by hour
by_month <- aggregate(CrimeData$Month, by = list(Mpnth = CrimeData$Month), FUN=length)
colnames(by_month) <- c("Month", "Total")
by_month$Month <- as.integer(by_month$Month)
str(by_month)


#plot the result
ggplot(by_month, aes(Month,Total)) + 
  geom_bar(fill="maroon", stat= "identity") + 
  ggtitle("Crime By Month") + 
  xlab("Month of the day") +
  ylab("Total Crimes")

#4. What are the top10 crimes crime types?

#count by type
by_category <- aggregate(CrimeData$Text_General_Code, by = list(CrimeData$Text_General_Code), FUN=length)
colnames(by_category) <- c("Category","Total")
by_category$Category <- as.character(by_category$Category)
str(by_category)
by_category_sorted <- by_category[order(by_category$Total,decreasing = T),]

#Select Top 10 crimes 
top10crimes <- by_category_sorted[1:10,]

#plot the results
ggplot(top10crimes,aes(x = reorder(Category,Total), y=Total)) + 
  geom_bar(aes(fill = Category),stat = "identity") +
  coord_flip()

#5. Which police HQ is in the most need of strengthening?

by_hq = aggregate(CrimeData$Dc_Dist, by = list(CrimeData$Dc_Dist), FUN=length)

#Rename columns
colnames(by_hq) <- c("HeadQuarters","Total")

#plot the result
ggplot(by_hq,aes(reorder(HeadQuarters,-Total),Total)) +
  geom_bar(color = "blue", stat = "identity")

