library(lubridate)
library(dplyr)
library(RMySQL)
pacman:: p_load(caret, Hmisc, party, reshape, ggplot2, ggplot, dplyr,reader,lubridate,tidyr,caret,tidyverse,lubridate,VIM,forecast,kableExtra,broom,knitr)


con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', dbname='dataanalytics2018', host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')
dbListTables(con)
dbListFields(con,'yr_2006')
              
yr_2006 <- dbGetQuery(con,"SELECT Date, Time, Global_active_power, Global_reactive_power, Global_intensity, Voltage, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2006")
yr_2007 <- dbGetQuery(con,"SELECT Date, Time, Global_active_power, Global_reactive_power, Global_intensity, Voltage, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2007")
yr_2008 <- dbGetQuery(con,"SELECT Date, Time, Global_active_power, Global_reactive_power, Global_intensity, Voltage, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2008")
yr_2009 <- dbGetQuery(con,"SELECT Date, Time, Global_active_power, Global_reactive_power, Global_intensity, Voltage, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2009")
yr_2010 <- dbGetQuery(con,"SELECT Date, Time, Global_active_power, Global_reactive_power, Global_intensity, Voltage, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2010")
 


head(yr_2006)
tail(yr_2006)
summary(yr_2007)
head(yr_2007)
tail(yr_2007)
head(yr_2008)
tail(yr_2008)
head(yr_2009)
tail(yr_2009)
head(yr_2010)
tail(yr_2010)


#Combine tables
newDF <- bind_rows(yr_2006 , yr_2007 , yr_2008 , yr_2009 , yr_2010 )
summary(newDF)
head(newDF)
tail(newDF)

## Combine Date and Time attribute values in a new attribute column
newDF <-cbind(newDF,paste(newDF$Date,newDF$Time), stringsAsFactors=FALSE)
newDF
summary(newDF)
head(newDF)

## Give the new attribute in the 6th column a header name 
## NOTE: if you downloaded more than 5 attributes you will need to change the column number)
colnames(newDF)[10] <-"DateTime"
head(newDF)

## Move the DateTime attribute within the dataset
newDF <- newDF[,c(ncol(newDF), 1:(ncol(newDF)-1))]
head(newDF)


## Convert DateTime from POSIXlt to POSIXct 
newDF$DateTime <- as.POSIXct(newDF$DateTime, "%Y/%m/%d %H:%M:%S")
head(newDF)

## Add the time zone
attr(newDF$DateTime, "tzone") <- "Europe/Paris"

## Inspect the data types
str(newDF)
class(newDF$DateTime)
range(newDF$DateTime)

#-remove data from year 2006
newDF <- filter(newDF, year(DateTime) != 2006)

summary(newDF)
head(newDF)

colnames(newDF)[8] <- 'Kitchen'
colnames(newDF)[9] <- 'Laundry_Room'
colnames(newDF)[10] <- 'EWH_AC'


sum(is.na(newDF))
summary(newDF)



#Comparing ####

newDF_tidy <- newDF %>%
  gather(Meter, Watt_hr, `Kitchen`, `Laundry_Room`, `EWH_AC`)  

newDF_tidy$Meter <- factor(newDF_tidy$Meter)

glimpse(newDF_tidy$Meter)
summary(newDF_tidy)

#-Year_Proportional Plot


newDF_tidy %>%
  group_by(year(DateTime), Meter) %>%
  summarise(sum=sum(Watt_hr)) %>%
  ggplot(aes(x=factor(`year(DateTime)`), sum, group=Meter,fill=Meter)) +
  labs(x='Year', y='Proportion of Energy Usage') +
  ggtitle('Proportion of Total Yearly EC') +
  geom_bar(stat='identity', position='fill', color='black') +
  theme(panel.border=element_rect(colour='black', fill=NA)) +
  theme(text = element_text(size = 14))



#-Filter and plot data for weeks 1-8

newDF_tidy %>%
  filter(week(DateTime) == c(1:8)) %>%
  mutate(Day=lubridate::wday(DateTime, label=TRUE, abbr=TRUE)) %>%
  group_by(Day, Meter) %>%
  summarise(sum=sum(Watt_hr/1000)) %>%
  ggplot(aes(x=factor(Day), y=sum)) +
  labs(x='Day of the Week', y='KWH') +
  ylim(0,85) +
  ggtitle('Total Energy Usage by Day for Weeks in Winter Months') +
  geom_bar(stat='identity', aes(fill = Meter), colour='black',position='dodge') +
  theme(panel.border=element_rect(colour='black', fill=NA)) +
  theme(text = element_text(size = 14)) +
  scale_fill_manual(values=c('Kitchen' = 'darkblue','Laundry_Room'='grey','EWH_AC'='lightblue'))

#-Filter and plot data for weeks 18-25

newDF_tidy %>%
  filter(week(DateTime) == c(18:25)) %>%
  mutate(Day=lubridate::wday(DateTime, label=TRUE, abbr=TRUE)) %>%
  group_by(Day, Meter) %>%
  summarise(sum=sum(Watt_hr/1000)) %>%
  ggplot(aes(x=factor(Day), y=sum)) +
  labs(x='Day of the Week', y='KWH') +
  ylim(0,85) +
  ggtitle('Total Energy Usage by Day for Weeks in Summer Months') +
  geom_bar(stat='identity', aes(fill = Meter), colour='black', position = "dodge") +
  theme(panel.border=element_rect(colour='black', fill=NA)) +
  theme(text = element_text(size = 14)) +
  scale_fill_manual(values=c('Kitchen' = 'darkblue','Laundry_Room'='grey','EWH_AC'='lightblue'))



# Create new time period attributes with lubridate
newDF$Year <- year(newDF$DateTime)
newDF$Quarter <- quarter(newDF$DateTime)
newDF$Month <- month(newDF$DateTime)
newDF$Week <- week(newDF$DateTime)
newDF$Weekday <- weekdays(newDF$DateTime)
newDF$Day <- day(newDF$DateTime)
newDF$Hour <- hour(newDF$DateTime)
newDF$Minute <- minute(newDF$DateTime)


str(newDF)
summary(newDF)
head(newDF)


# Define active power, which represents the active energy consumed every minute 
# (in watt hour) in the household by electrical equipment not measured in 
# sub-meterings 1, 2 and 3

newDF$AP_kWh <- ((newDF$Global_active_power * 1000/60) - 
                               newDF$Kitchen - 
                               newDF$Laundry_Room - 
                               newDF$EWH_AC)

newDF$GAP_kWh <- newDF$Global_active_power * 1000/60

# Create another variable that shows year and month in one column
newDF$Date <- as.yearmon(paste(newDF$Year, newDF$Month,sep = "-"))


## Data Visualization ----
# Bar graphs for "global active power" and "active power" in each season
ggplot(newDF, aes(x = Year, y = Global_active_power)) + 
  stat_summary(fun.y = "mean", geom = "bar")

ggplot(newDF, aes(x = Year, y = Global_reactive_power)) + 
  stat_summary(fun.y = "mean", geom = "bar")

ggplot(newDF, aes(x = Year, y = Voltage)) + 
  stat_summary(fun.y = "mean", geom = "bar")

ggplot(newDF, aes(x = Quarter, y = Global_active_power)) + 
  stat_summary(fun.y = "mean", geom = "bar")

# Smooth line graph for average sub meter measurements and electic consumptions

newDF %>% 
  group_by(DATE = DateTime) %>% 
  summarise(MeanAP = mean(Global_active_power)) %>%
  ggplot(aes(DATE, MeanAP)) + 
  geom_line(color = "red") + 
  geom_smooth(se = F) +
  labs(title = "Average Active Power Used by Day") + 
  ylab("kWh") + 
  xlab("Time") + 
  theme_light() -> plot1
ggplotl(plot1)


#sub3 ####
#-Subset data for weeks 1-8 and assign to variable w

w <- newDF_tidy %>%
  filter(week(DateTime) == c(1:8)) %>%
  filter(Meter == 'EW_AC_H') %>% 
  mutate(Day=lubridate::wday(DateTime, label=TRUE, abbr=TRUE)) %>%
  group_by(Day, Meter) %>%
  summarise(sum=sum(Watt_hr/1000))


#-Subset data for weeks 18-25 and assign to variable ww

ww <- newDF_tidy %>%
  filter(week(DateTime) == c(18:25)) %>%
  filter(Meter == 'EW_AC_H') %>% 
  mutate(Day=lubridate::wday(DateTime, label=TRUE, abbr=TRUE)) %>%
  group_by(Day, Meter) %>%
  summarise(sum=sum(Watt_hr/1000))

#-Overlay line plots of the two 8-week time periods

ggplot(w) +
  labs(x='Day of the Week', y='kWh') +
  ylim(0,65) +
  ggtitle('Total Energy Usage on Submeter 3 for High\n Consumption Period in Winter and Summer Months') +
  geom_line(aes(x=Day, y=sum, group=1,colour='winter')) +
  geom_line(data = ww, aes(x=Day, y=sum, group=1, color='summer')) +
  scale_colour_manual(values=c('winter'='blue', 'summer'='red')) +
  labs(colour='Season') +
  guides(colour=guide_legend(reverse=TRUE)) +
  theme(panel.border=element_rect(colour='black', fill=NA))+
  theme(text = element_text(size = 14))




#-Subset data by quarter and summarise total usage across the 3 submeters
newDF_qtr <- newDF%>%
  group_by(year(DateTime), quarter(DateTime)) %>%
  summarise(Kitchen=round(sum(`Kitchen`/1000), 3),
            Laundry_Room=round(sum(`Laundry_Room`/1000), 3),
            EWH_AC=round(sum(`EWH_AC`/1000), 3))


#-Look at top several rows of new quarterly data set 
View(newDF_qtr)


newDF_qtr_tidy <- newDF_qtr %>%
  gather(Meter, Watt_hr, `Kitchen`, `Laundry_Room`, `EWH_AC`)  

newDF_qtr_tidy$Meter <- factor(newDF_qtr_tidy$Meter)

glimpse(newDF_qtr_tidy)


## Subset the second week of 2008 - All Observations

newDF <- filter(newDF, year == 2008 & week == 2)

## Plot subset houseWeek


hist(newDF$Global_active_power, main="Global Active Power", xlab = "Global Active Power (kilowatts)", col="red")
plot(newDF$Global_active_power~newDF$DateTime, type="l", ylab="Global Active Power (kilowatts)", xlab="")
