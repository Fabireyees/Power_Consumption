#1.LIBRARY ####

library(tidyverse)  #Package for tidying data
library(lubridate)  #For working with dates/times of a time series
library(VIM)        #Visualizing and imputing missing values
library(Hmisc)      #for descriptive statistics
library(forecast)   #forcasting package
library(kableExtra) #fancy table generator
library(broom)      #Tidy statistical summary output
library(knitr)      #report generatioN

pacman:: p_load(caret, party, reshape, TTR, ggplot2, dplyr, DBI,RMySQL,dplyr,tidyr,lubridate, ggplot2, caret,ploty, forecast)

#2.DATA IMPORTATION####
#1. Create a database connection from MySQL.
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', dbname='dataanalytics2018', host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')   

#2. Import the data that will be used
tables <- dbListTables(con)
df <- data.frame()
for (i in 2:length(tables)) {
  actual <- dbGetQuery(con, paste("SELECT * FROM ",tables[i]))
  df <- rbind(df, actual)
}

#3. Importing Weather
weather_2007 <- readRDS("C:/Respaldo FR/UBIQUM/iot2/year2007_weather.RDS")
weather_2008 <- readRDS("C:/Respaldo FR/UBIQUM/iot2/year2008_weather.RDS")
weather_2009 <- readRDS("C:/Users/fabi_/Downloads/year2009_weather.RDS")

#3. Merging Weather
weather <- rbind(weather_2007, weather_2008, weather_2009)

#Importing weather data - daily
weather_daily <- readRDS("C:/Respaldo FR/UBIQUM/iot2/weather_daily.RDS")

#Importing AC/HW
ACHW <- readRDS("C:/Respaldo FR/UBIQUM/iot2/AC_WH (1).RDS")
#3.Data Preprocessing####

#3.1Chaning Date & Time to time series
df$DateTime <- paste(df$Date, df$Time) #DateTime needs to be an only argument. 
df <- df[,c(which(colnames(df) == "DateTime"), 1:ncol(df)-1)] #Changing the DateTime to first column. 
df$DateTime <- as.POSIXct(df$DateTime, "%Y/%m/%d %H:%M:%S")
attr(df$DateTime, "tzone") <- "Europe/Paris" #Correcting Time zone
df<- df[,-which(colnames(df) =="Date" | colnames(df) =="Time")] #Deleting Time and Date
df <- arrange(df, DateTime) #Ordering data

#3.2Check
head(df)
tail(df)

#3.3Adding weather to df
weather$time <- as.POSIXct(weather$time, "%Y-%m-%d %H:%M:%S") #Changing to time series
attr(weather$time, "tzone") <- "Europe/Paris"  #Correcting Time zone

df <- df %>% mutate(year = year(df$DateTime), month = month(df$DateTime), day = day(df$DateTime), week = week(df$DateTime), hour = hour(df$DateTime)) #Creating variables to merge
weather <- weather %>% select (time, temperature) %>%
      mutate( year =year(weather$time), month =  month(weather$time), day = day(weather$time), hour =  hour(weather$time)) #Creating variables to merge
    
    df <- left_join(df, weather, by = c("year", "month", "day", "hour"), match = "all") #Merging datasets
    df<- df[, -which(colnames(df) =="time")] #time eliminated because it is duplicated. 
  str(df)
  
  #3.4Adding weather_daily to df
    weather_daily$time <- paste(weather_daily$time, "00:00:00") #Adding a random hour to be able to change to format POSIXc
    weather_daily$time <- as.POSIXct(weather_daily$time, "%Y-%m-%d %H:%M:%S") #Change to time series
    
    weather_daily <- weather_daily %>% select(time, sunriseTime, sunsetTime) %>%  #Select the variaables to add
      mutate( year = year(weather_daily$time), month = month(weather_daily$time), day = day(weather_daily$time)) #Create the filters
      
  
    df <- left_join(df, weather_daily, by = c("year", "month", "day"), match = "all") #merging the data
    df<- df[, -which(colnames(df) =="time")] #time eliminated because it is duplicated.
    
  #3.5Adding ACHW to df
    ACHW$DateTime <- as.POSIXct(ACHW$DateTime, "%Y-%m-%d %H:%M:%S") #Chaning to time series
    ACHW <- ACHW %>% select( DateTime, AC, WH) #Selecting variables
    df <- left_join(df, ACHW, by = "DateTime", match = "all") #Merging
  #Year 2006 and 2010 Eliminated - Data Exploration*
    df <- filter(df, year != "2006")
    df <- filter(df, year != "2010")
  
  #3.6 Units Modfiication-changing to KW/h
    df$Sub_metering_1 <- df$Sub_metering_1/1000 #W/h to KW/h
    df$Sub_metering_2 <- df$Sub_metering_2/1000 #W/h to KW/h
    df$Sub_metering_3 <- df$Sub_metering_3/1000 #W/h to KW/h
    df$Global_active_power <- df$Global_active_power/60 # KW to KW/h
#4.Feature Enginieering####
    #4.1Creating Light/Dark
    df$sunriseTime <- as.POSIXct(df$sunriseTime, "%Y-%m-%d %H:%M:%S") #Change Sunrise to time series
    df$sunsetTime <- as.POSIXct(df$sunsetTime, "%Y-%m-%d %H:%M:%S") #Change Sunset to timeseries 
    df <- df %>% mutate( Light = ifelse (df$DateTime > df$sunriseTime & df$DateTime < df$sunsetTime, "Day", "Night")) %>% #Creating Light/night
      select(-sunriseTime, -sunsetTime) #Deleting sunrise and sunset times
    str(df)
    #4.2Separating WH and AC based on frequency of usage
    #Creating a dataset with the values to be separated
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
    
    str(sub3)
    
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
    
    summary(nsub3)
    
    

#5 Data prepared for Exploration####
#5.1 Review Data
    years = 2007 #Define the year to study
    str(filter(df, year==years))
    summary(filter(df, year==years))
    head(filter(df, year==years))
    tail(filter(df, year==years))
#5.2 Check Outilers Pattern-wich are not in here anymore.
    aggr(df, col=c('navyblue','red'),
         numbers=TRUE, 
         sortVars=TRUE, 
         labels=names(df),
         cex.axis=.7, 
         gap=3, 
         ylab=c("Histogram of missing data","Pattern"), 
         digits=2)
    
#5.3 Remove rows with NA's (IN CASE BUT IT IS NOT )
    
#5.4 Verify NA's and empty rows
    
    sum(is.na(df))
    summary(df)
    sum(is.na(nsub3))
    sum(is.na(weather))
    
    
#6 Data visualizationn#### 
    #6.1 Visualize the data 
    ##6.2 Plot all of sub-meter 1
    plot(df$Sub_metering_1)
    ##6.3 Subset the second week of 2008 - All Observations
    houseWeek <- filter(df, year == 2008 & week == 2)
    ##6.4 Plot subset houseWeek
    plot(df$Sub_metering_1)
    ##6.5 Subset the 9th day of January 2008 - All observations
    houseDay <- filter(df, year == 2008 & month == 1 & day == 9)
    ##6.6 Plot sub-meter 1
    plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, type = 'scatter', mode = 'lines')
    ##6.7 Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations 
    plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
      add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
      add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
      layout(title = "Power Consumption January 9th, 2008",
             xaxis = list(title = "Time"),
             yaxis = list (title = "Power (watt-hours)"))
#7.Granularity study####
    #7.1 Granularity - Month
    
    month10_year2008 <- filter(df, year == 2008 & month == 10)
    
    ##7.1.1 Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Month frequency
    plot_ly(month10_year2008, x = ~month10_year2008$DateTime, y = ~month10_year2008$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
      add_trace(y = ~month10_year2008$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
      add_trace(y = ~month10_year2008$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
      layout(title = "Power Consumption January 9th, 2008",
             xaxis = list(title = "Time"),
             yaxis = list (title = "Power (watt-hours)"))
    
    #7.2Granularity -  Day
    
    ##7.2.1Subset the 9th day of January 2008 - 10 Minute frequency
    houseDay12 <- filter(df, year == 2008 & month == 1 & day == 12)
    
    ##7.2.2Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
    plot_ly(houseDay12, x = ~houseDay12$DateTime, y = ~houseDay12$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
      add_trace(y = ~houseDay12$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
      add_trace(y = ~houseDay12$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
      layout(title = "Power Consumption January 9th, 2008",
             xaxis = list(title = "Time"),
             yaxis = list (title = "Power (watt-hours)"))
    
#8. Time Series ####
#8.1 Subset hour   
    df_hr <- df %>%
      group_by(year, month, day, hour, ) %>%
      summarise(Sub_Metering_1=round(sum(Sub_metering_1), 3),
                Sub_Metering_2=round(sum(Sub_metering_2), 3),
                Sub_Metering_3=round(sum(Sub_metering_3), 3))
 #8.2 Subset month
    
    df_month <- df %>%
      group_by(year, month, ) %>%
      summarise(Sub_Metering_1=round(sum(Sub_metering_1), 3),
                Sub_Metering_2=round(sum(Sub_metering_2), 3),
                Sub_Metering_3=round(sum(Sub_metering_3), 3))
    
    saveRDS(df_month, file = "C:/Respaldo FR/UBIQUM/iot2/month")
#8.3 Subset daily
    
    df_daily <- df %>%
      group_by(year,month,day) %>%
      summarise(Sub_Metering_1=round(sum(Sub_metering_1), 3),
                Sub_Metering_2=round(sum(Sub_metering_2), 3),
                Sub_Metering_3=round(sum(Sub_metering_3), 3))
    write_csv(df_daily,'daily.csv')
    
#8.4 Subset weekly
    
    df_weekly <- df %>%
      group_by(year, month, week) %>%
      summarise(Sub_Metering_1=round(sum(Sub_metering_1), 3),
                Sub_Metering_2=round(sum(Sub_metering_2), 3),
                Sub_Metering_3=round(sum(Sub_metering_3), 3))
    View(df_weekly)
    write_csv(df_weekly,'weekly.csv')
    
#8.5 Creating MONTH and Weekly series 
    
    df_month_ts<- ts(df_month,
                     frequency=12,
                     start=c(2007,1),
                     end=c(2010,12))
    
    plot.ts(df_month_ts)
    
    
    df_weekly_ts<- ts(df_month,
                      frequency=52,
                      start=c(2007,1),
                      end=c(2010,12))
    
    plot.ts(df_weekly_ts)
    
 #8.6 -Plot quarterly time series
    plot(df_month_s, 
         plot.type='s',
         col=c('red', 'green', 'blue'),
         main='Total Quarterly kWh Consumption',
         xlab='Year', ylab = 'kWh')
    minor.tick(nx=4)

#9. Models- Fiting a linear Model With Time Series Components ####
    fit1_lm<- tslm(df_month_s[,3] ~ trend + season)
    summary(fit1_lm)
    tail(df_lm)  
    glance(fit1_lm)
    tidy(fit1_lm)
    checkresiduals(fit1_lm)  
    
    #-Plot fitted vs actual for monthly linear model.
    #-Fit linear model to montly time series for submeter 3
    fit2 <- tslm(df_month_s[,3] ~ trend + season)
    #-One-row statistical summary of monthly linear model
    glance(fit2)
    checkresiduals(fit2)
    
    #-Forecast 4-quarters of energy usage 
    x <- forecast(fit1, h=4, level=c(80,95))
    
    #-Plot 4-quarter forecast of energy usage
    plot(x, showgap=FALSE, include=3,
         shadecols=c('slategray3','slategray'),
         xlab='Year', ylab='kWh',
         main='4-Quarter Forecast of Quartlerly Energy Consumption \nfor Submeter-3')
    minor.tick(nx=2)  
    #-Forecast 6-months of energy usage
    y <- forecast(fit1,h=6, level=c(80,95))
    
    #-Plot 6-month forecast of energy usage
    plot(y, showgap=FALSE, include=4,
         shadecols=c('slategray3','slategray'),
         xlab ='Year',
         ylab=' kWh',
         main='6-Month Forecast of Monthly Energy Consumption')
    minor.tick(nx=6)
    
    plot.ts(df_month_s)

    

    
#10. Train Sets ####
#10.1 Creating test and train to ts
Sub_metering_1_ts <- df_month_ts[,'Sub_Metering_1']
Sub_metering_2_ts <- df_month_ts[,'Sub_Metering_2']
Sub_metering_3_ts <- df_month_ts[,'Sub_Metering_3']
    
month_submeter1_train <- window(Sub_metering_1_ts, start=c(2007,1), end=c(2009,12))
    
month_submeter2_train <- window(Sub_metering_2_ts, start=c(2007,1), end=c(2009,12))
    
month_submeter3_train <- window(Sub_metering_3_ts, start=c(2007,1), end=c(2009,12))
    
ndiffs(df_month_ts)
diff(df_month_ts)
    
#plot
plot(Sub_metering_1_ts)
plot(Sub_metering_2_ts)Se



#11.Submeter 3-Monnth ####
#11.1 Plots and SMA
plot.ts(Sub_metering_3_ts)
df_month_ts_3SMA3 <- SMA(Sub_metering_3_ts,n=6)
plot.ts(df_month_ts_3SMA3)
df_month_ts3_components <- decompose(df_month_ts_3SMA3) #to investigate trend and seasonality
plot(df_month_ts3_components)
summary(df_month_ts3_components)
#Seasonally Adjusting
df_month_ts3_components <- decompose(df_month_ts_3SMA3)
df_month_ts_3_seasonallyadjusted <- df_month_ts_3SMA3 - df_month_ts3_components$seasonal
plot(df_month_ts_3_seasonallyadjusted)
ndiffs(Sub_metering_3_ts)
diff(Sub_metering_3_ts)
fit1_LM_S3 <- tslm(month_submeter3_train ~ trend + season) 
summary(fit1_LM_S3)

# Create the forecast for sub-meter 3. Forecast ahead 12 time periods 
forecastfit1_LM_S3 <- forecast(fit1_LM_S3, h=12)
#Plot the forecast for sub-meter 3. 
plot(forecastfit1_LM_S3)  

## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfit1_LM_S3, ylim = c(200, 400), ylab= "Watt-Hours", xlab="Time")
## Accuracy 
accuracy_LM_S3<-accuracy(forecastfit1_LM_S3, Sub_metering_3_ts)
accuracy_LM_S3

## Holt Winters Exponential Smoothing & Plot
dfts_month_3_h <- HoltWinters(month_submeter3_train, beta=FALSE, gamma=FALSE)
plot(dfts_month_3_h, ylim = c(200,400))
## HoltWinters forecast & plot
dfts_month_3for <- forecast(Sub_metering_3_ts, h=12)
plot(dfts_month_3for, ylim = c(200, 400), ylab= "Watt-Hours", xlab="Time - Sub-meter 3")

## Forecast HoltWinters with diminished confidence levels
dfts_month_3forC <- forecast(month_submeter3_train, h=25, level=c(10,25))
## Plot only the forecasted area
plot(dfts_month_3forC, ylim = c(200, 400), ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(2011))

## Accuracy Holt Winters Exponential
accuracy_HC_S3 <- accuracy(dfts_month_3forC, Sub_metering_3_ts)
accuracy_HC_S3

## Autoarima 
Month_Submeter3_auto.arima <- auto.arima(month_submeter3_train)
Month_Submeter3_auto.arima_forecast <- forecast(Month_Submeter3_auto.arima,h=12)
autoplot(Month_Submeter3_auto.arima_forecast)
Month_Submeter3_auto.arima_forecast

## Accuracy Autoarima
accuracy_AUTO.ARIMAS3 <- accuracy(Month_Submeter3_auto.arima_forecast, Sub_metering_3_ts)
accuracy_AUTO.ARIMAS3


## Arima

acf(Sub_metering_3_ts, lag.max=20)
pacf(Sub_metering_3_ts, lag.max=20)
pacf(Sub_metering_3_ts, lag.max=20, plot=FALSE)

ndiffs(Sub_metering_3_ts) #d=0 is stacionary
nsdiffs(Sub_metering_3_ts) #D 

modelarima <- Arima(month_submeter3_train, order=c(0,0,1), seasonal = c(0,0,1))
modelarima

modelarima2 <- Arima(month_submeter3_train, order=c(0,0,0), seasonal = c(0,1,1))
modelarima2


print(modelarima);print(modelarima2);print(modelarima3)


#forecastarima
predictionsforecast1_arima_sub3<- forecast(modelarima,h=12)
predictionsforecast1_arima_sub3


predictionsf2<- forecast(modelarima2,h=12)
predictionsf2


acf(predictionsf1$residuals, lag.max=20)

Box.test(predictionsf1$residuals, lag=20, type="Ljung-Box")

acf(predictionsf2$residuals, lag.max=20)

Box.test(predictionsf2$residuals, lag=20, type="Ljung-Box")

acf(predictionsf3$residuals, lag.max=20)

Box.test(predictionsf3$residuals, lag=20, type="Ljung-Box")


plot(predictionsf2)
plot(predictionsf1)


accuracy_Arima_sub3 <- accuracy(predictionsforecast1_arima_sub3, Sub_metering_3_ts)
accuracy_Arima_sub3


#Fit an AR(2) model to each rolling origin subset
far2 <- function(x, h){forecast(Arima(x, order=c(1,1,0)), h=h)}
e <- tsCV(dfts_month_3, far2, h=1)
e




#make time plot of forecasterrors

plot.ts(predictionsf$residuals)
plot.ts(predictionsf2$residuals)
plot.ts(predictionsf3$residuals)
# make a histogram

plotForecastErrors(predictionsf2$residuals)


modelcv <- CVar(lynx, k=5, lambda=0.15)
print(modelcv)


e <- tsCV(dj, rwf, drift=TRUE, h=1)
sqrt(mean(e^2, na.rm=TRUE))


e <- tsCV(Sub_metering_3_ts, Arima(x, order=c(1,1,1), h=1)
          sqrt(mean(e^2, na.rm=TRUE)) 
e          
          
          
          
















