#-Subset data by hour and summarise total usage across the 3 submeters


df_hr <- df %>%
  group_by(year, month, day, hour, ) %>%
  summarise(Sub_Metering_1=round(sum(Sub_metering_1), 3),
            Sub_Metering_2=round(sum(Sub_metering_2), 3),
            Sub_Metering_3=round(sum(Sub_metering_3), 3))


view(df_hr)


df_hr <- df %>%
  group_by(year, month, , ) %>%
  summarise(Sub_Metering_1=round(sum(Sub_metering_1), 3),
            Sub_Metering_2=round(sum(Sub_metering_2), 3),
            Sub_Metering_3=round(sum(Sub_metering_3), 3))



df_daily <- df %>%
  group_by(year, month, day, ) %>%
  summarise(Sub_Metering_1=round(sum(Sub_metering_1), 3),
            Sub_Metering_2=round(sum(Sub_metering_2), 3),
            Sub_Metering_3=round(sum(Sub_metering_3), 3))


view(df_daily)

#-Create hourly time series 

df_m <- ts(df_hr[,1:3],
                     frequency=4,
                     start=c(2007,1),
                     end=c(2010,3))



#-Plot quarterly time series

plot(df_hr_q, 
     plot.type='s',
     col=c('red', 'green', 'blue'),
     main='Total Quarterly kWh Consumption',
     xlab='Year', ylab = 'kWh')
minor.tick(nx=4)


fit1 <- tslm(df_hr_q[,3] ~ trend + season)

summary(fit1)


glance(fit1)
tidy(fit1)



fit <- 














#-Plot fitted vs actual for quarterly linear model.

ggplot(fit1, aes(x=fit1$fitted.values, y=df_hr_q[,3])) +
  geom_point(color='blue', size=4) +
  labs(x='Fitted Value', y='Actual') +
  geom_abline(intercept = 0, slope=1,  linetype='dashed') +
  ggtitle('Fitted vs. Actual Values for Quarterly Linear Model') +
  theme(panel.border=element_rect(colour='black', fill=NA))+
  theme(text = element_text(size = 14))




#-Plot fitted vs residuals for quarterly linear model.

ggplot(fit1, aes(x=fit1$fitted.values, y=fit1$residuals)) +
  geom_point(color='blue', size=4) +
  labs(x='Fitted Values', y='Residuals') +
  geom_hline(yintercept = 0, linetype='dashed') +
  ggtitle('Residuals Plot of Quarterly Linear Model') +
  theme(panel.border=element_rect(colour='black', fill=NA))+
  theme(text = element_text(size = 14))

checkresiduals(fit1)

str(df_hr_q)
str(df)






#-Create monthly time series
housePWR_m <- ts(df_hr[,5:7],
                      frequency = 12,
                      start=c(2007,1),
                      end=c(2010,3))

#-Plot monthly time series
plot(housePWR_m, 
     plot.type='s',
     xlim=c(2007, 2011),
     col=c('red', 'green', 'blue'),
     main='Total Monthly kWh Consumption',
     xlab='Year/Month', ylab = 'kWh')
minor.tick(nx=12)
























library(tidyr)
library(dplyr)

#-Create long form of data set

df_tidy <- df %>%
  gather(Meter, Watt_hr, `Sub_Metering_1`, `Sub_Metering_2`, `Sub_Metering_3`)  

#-Convert meter feature to categorical
df_tidy$Meter <- factor(df_tidy$Meter)

#-peak at data 
glimpse(df_tidy)




#-Hour of day bar chart
df_tidy %>%
  filter(month(DateTime) == c(1,2,11,12)) %>%
  group_by(hour(DateTime), Meter) %>%
  summarise(sum=round(sum(Watt_hr)/1000),3) %>%
  ggplot(aes(x=factor(`hour(DateTime)`), y=sum)) +
  labs(x='Hour of the Day', y='kWh') +
  ggtitle('Total Energy Usage by Hour of the Day') +
  geom_bar(stat='identity', aes(fill = Meter), colour='black') +
  theme(panel.border=element_rect(colour='black', fill=NA)) +
  theme(text = element_text(size = 14))