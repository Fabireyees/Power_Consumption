#library #### 
library(DBI)
library(RMySQL)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(caret)      
library(tidyverse)
library(lubridate)  
library(VIM)        
library(Hmisc)      
library(forecast)   
library(kableExtra) 
library(broom)      
library(knitr)      
library(plotly)

#data #### 
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', dbname='dataanalytics2018', host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')   

#Import the data

tables <- dbListTables(con)
df <- data.frame()
for (i in 2:length(tables)) {
  actual <- dbGetQuery(con, paste("SELECT * FROM ",tables[i]))
  df <- rbind(df, actual)
}

#Importing Weather data - hourly
weather_2007 <- readRDS("C:/Respaldo FR/UBIQUM/iot2/year2007_weather.RDS")
weather_2008 <- readRDS("C:/Respaldo FR/UBIQUM/iot2/year2008_weather.RDS")
weather_2009 <- readRDS("C:/Respaldo FR/UBIQUM/iot2/year2009_weather.RDS")
weather <- rbind(weather_2007, weather_2008, weather_2009)

#Importing weather data - daily
weather_daily <- readRDS("C:/Respaldo FR/UBIQUM/iot2/weather_daily.RDS")

#Importing AC/HW
ACHW <- readRDS("C:/Respaldo FR/UBIQUM/iot2/AC_WH (1).RDS")

#Importing weather data - daily
weather_daily <- readRDS("C:/Respaldo FR/UBIQUM/iot2/weather_daily.RDS")

#Importing AC/HW
ACHW <- readRDS("C:/Respaldo FR/UBIQUM/iot2/AC_WH (1).RDS")

#Data Preprocessing####

#Chaning Date & Time to time series

df$DateTime <- paste(df$Date, df$Time) #DateTime needs to be an only argument. 
df <- df[,c(which(colnames(df) == "DateTime"), 1:ncol(df)-1)] #Changing the DateTime to first column. 
df$DateTime <- as.POSIXct(df$DateTime, "%Y/%m/%d %H:%M:%S")
attr(df$DateTime, "tzone") <- "Europe/Paris" #Correcting Time zone

df<- df[,-which(colnames(df) =="Date" | colnames(df) =="Time")] #Deleting Time and Date

df <- arrange(df, DateTime) #Ordering data


#Adding weather to df
weather$time <- as.POSIXct(weather$time, "%Y-%m-%d %H:%M:%S") #Changing to time series
attr(weather$time, "tzone") <- "Europe/Paris"  #Correcting Time zone

df <- df %>% mutate(year = year(df$DateTime), month = month(df$DateTime), day = day(df$DateTime), hour = hour(df$DateTime)) #Creating variables to merge
weather <- weather %>% select (time, temperature) %>%
  mutate( year =year(weather$time), month =  month(weather$time), day = day(weather$time), hour =  hour(weather$time)) #Creating variables to merge

df <- left_join(df, weather, by = c("year", "month", "day", "hour"), match = "all") #Merging datasets
df<- df[, -which(colnames(df) =="time")] #time eliminated because it is duplicated. 
str(df)

#Adding weather_daily to df
weather_daily$time <- paste(weather_daily$time, "00:00:00") #Adding a random hour to be able to change to format POSIXc
weather_daily$time <- as.POSIXct(weather_daily$time, "%Y-%m-%d %H:%M:%S") #Change to time series

weather_daily <- weather_daily %>% select(time, sunriseTime, sunsetTime) %>%  #Select the variaables to add
  mutate( year = year(weather_daily$time), month = month(weather_daily$time), day = day(weather_daily$time)) #Create the filters


df <- left_join(df, weather_daily, by = c("year", "month", "day"), match = "all") #merging the data
df<- df[, -which(colnames(df) =="time")] #time eliminated because it is duplicated.

#Adding ACHW to df
ACHW$DateTime <- as.POSIXct(ACHW$DateTime, "%Y-%m-%d %H:%M:%S") #Chaning to time series
ACHW <- ACHW %>% select( DateTime, AC, WH) #Selecting variables
df <- left_join(df, ACHW, by = "DateTime", match = "all") #Merging
#Year 2006 and 2010 Eliminated - Data Exploration*
df <- filter(df, year != "2006" & year != 2010)

str(df)

#Units Modfiication

df$Sub_metering_1 <- df$Sub_metering_1/1000 #W/h to KW/h
df$Sub_metering_2 <- df$Sub_metering_2/1000 #W/h to KW/h
df$Sub_metering_3 <- df$Sub_metering_3/1000 #W/h to KW/h
df$Global_active_power <- df$Global_active_power/60 # KW to KW/h

#Select important variables
str(df)

#-Subset data by day-hour-month-week and summarise total usage across the 3 submeters and run models.




str(df)
head(df)


#-Create daily time series 


#-Look at top several rows of new quarterly data set 





#Feature Enginieering####
#Creating Light/Dark
df$sunriseTime <- as.POSIXct(df$sunriseTime, "%Y-%m-%d %H:%M:%S") #Change Sunrise to time series
df$sunsetTime <- as.POSIXct(df$sunsetTime, "%Y-%m-%d %H:%M:%S") #Change Sunset to timeseries 
df <- df %>% mutate( Light = ifelse (df$DateTime > df$sunriseTime & df$DateTime < df$sunsetTime, "Day", "Night")) %>% #Creating Light/night
  select(-sunriseTime, -sunsetTime) #Deleting sunrise and sunset times

#Separating WH and AC --      <<<<<  CALCULATED SAVED AND IMPORTED >>>>>>

#   #Creating a dataset with the values to be separated
sub3 <- df[-which(df$Sub_metering_3 < 0.003),]
#Separate them 
sub3$WH <- 0
sub3$AC <- 0

for (i in 2:(nrow(sub3)-1)) {
  if (sub3$Sub_metering_3[i] > 0.02) { #+1 1 min to full power
    sub3$WH[i] <- sub3$Sub_metering_3[i]*0.6
    sub3$AC[i] <- sub3$Sub_metering_3[i]*0.4   
  } else if (sub3$Sub_metering_3[i] > 0.015) { 
    sub3$WH[i] <- sub3$Sub_metering_3[i]
  } else if (sub3$Sub_metering_3[i] < 0.015 & (sub3$Sub_metering_3[i+1] > 0.015 | sub3$Sub_metering_3[i-1] > 0.015)) {
    #If its less than 0.015 the WH might be starting to work or finishing, if either the previous or next point it is higher than 0.15
    sub3$WH[i] <- sub3$Sub_metering_3[i]
  } else {
    sub3$AC[i] <- sub3$Sub_metering_3[i]
  }
}

#   #For the first and last point
for (i in c(1,nrow(sub3))) {
  if (sub3$Sub_metering_3[i] > 0.02) { #+1 1 min to full power
    sub3$WH[i] <- sub3$Sub_metering_3[i]*0.6
    sub3$AC[i] <- sub3$Sub_metering_3[i]*0.4   
  } else if (sub3$Sub_metering_3[i] > 0.015) { 
    sub3$WH[i] <- sub3$Sub_metering_3[i]
  } else {
    sub3$AC[i] <- sub3$Sub_metering_3[i]
  }
}

#Dataframe with not studied values
nsub3 <- df[which(df$Sub_metering_3 < 0.003),]
nsub3$WH <- 0
nsub3$AC <- 0
#Join sub3 and nsub3
nsub3 <- rbind(sub3, nsub3)
nsub3 <- nsub3[, which(colnames(nsub3) == "DateTime" | colnames(nsub3) == "WH" | colnames(nsub3) == "AC" | colnames(nsub3) == "id")]
#Reordering
nsub3 <- arrange(nsub3, DateTime) 
#Save
saveRDS(nsub3, file = "C:/Respaldo FR/UBIQUM/iot2/year2009_weather.RDS")
head(nsub3)








#Data Exploration####
#Review Data
years = 2007 #Define the year to study
str(filter(df, year==years))
summary(filter(df, year==years))
head(filter(df, year==years))
tail(filter(df, year==years))

#Coments : 
#Years 2006 and 2010 Eliminated because are not completed (in Data Processing)

#Outliers ?

#NA
aggr(df, col=c('navyblue','red'),
     numbers=TRUE, 
     sortVars=TRUE, 
     labels=names(df),
     cex.axis=.7, 
     gap=3, 
     ylab=c("Histogram of missing data","Pattern"), 
     digits=2)

#-Remove rows with NA's (IN CASE BUT IT IS NOT )


#Verify NA's and empty rows

sum(is.na(df))
summary(df)
sum(is.na(nsub3))
sum(is.na(weather))




#
#Visualize the data 
## Plot all of sub-meter 1
plot(df$Sub_metering_1)
## Subset the second week of 2008 - All Observations
houseWeek <- filter(df, year == 2008 & week == 2)
## Plot subset houseWeek
plot(df$Sub_metering_1)
## Subset the 9th day of January 2008 - All observations
houseDay <- filter(df, year == 2008 & month == 1 & day == 9)
## Plot sub-meter 1
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, type = 'scatter', mode = 'lines')
## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations 
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))



#Correlation study####


#Granularity - Month

month10_year2008 <- filter(df, year == 2008 & month == 10)

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Month frequency
plot_ly(month10_year2008, x = ~month10_year2008$DateTime, y = ~month10_year2008$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~month10_year2008$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~month10_year2008$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))



#Granularity -  Day

## Subset the 9th day of January 2008 - 10 Minute frequency
houseDay12 <- filter(df, year == 2008 & month == 1 & day == 12)

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay12, x = ~houseDay12$DateTime, y = ~houseDay12$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay12$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay12$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))



#Granularity -  Week


#Granularity - hours



# Time Series ####
# Subset hour

df_hr <- df %>%
  group_by(year, month, day, hour, ) %>%
  summarise(Sub_Metering_1=round(sum(Sub_metering_1), 3),
            Sub_Metering_2=round(sum(Sub_metering_2), 3),
            Sub_Metering_3=round(sum(Sub_metering_3), 3))


View(df_hr)

# Subset month
df_month <- df %>%
  group_by(year, month, ) %>%
  summarise(Sub_Metering_1=round(sum(Sub_metering_1), 3),
            Sub_Metering_2=round(sum(Sub_metering_2), 3),
            Sub_Metering_3=round(sum(Sub_metering_3), 3))

str(df_month)

# Subset daily

df_daily <- df %>%
  group_by(year, month, day, ) %>%
  summarise(Sub_Metering_1=round(sum(Sub_metering_1), 3),
            Sub_Metering_2=round(sum(Sub_metering_2), 3),
            Sub_Metering_3=round(sum(Sub_metering_3), 3))


str(df_daily)


str(df_hr)

view(df_hr)
## Subset to one observation per hour


#-Create hourly time series 

df_month<- ts(df_hr,
              frequency=12,
              start=c(2007,1),
              end=c(2010,12))

#-Plot quarterly time series

plot(df_hr, 
     plot.type='s',
     col=c('red', 'green', 'blue'),
     main='Total Quarterly kWh Consumption',
     xlab='Year', ylab = 'kWh')
minor.tick(nx=4)

fit1 <- tslm(df_hr[,3] ~ trend + season)

summary(fit1)


glance(fit1)
tidy(fit1)




#-Subset data by quarter and summarise total usage across the 3 submeters
df_qtr <- df %>%
  group_by(year(DateTime), quarter(DateTime)) %>%
  summarise(Sub_Meter_1=round(sum(`Sub-Meter-1`/1000), 3),
            Sub_Meter_2=round(sum(`Sub-Meter-2`/1000), 3),
            Sub_Meter_3=round(sum(`Sub-Meter-3`/1000), 3))

#-Look at top several rows of new quarterly data set 
head(housePWR_qtr)

















## Subset to one observation per week on Mondays at 8:00pm for 2007, 2008 and 2009
house070809weekly <- filter(yourData, weekDay == 2 & hour == 20 & minute == 1)

## Create TS object with SubMeter3
tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, frequency=52, start=c(2007,1)

