
str(df)


# Create a database connection 
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
weather_2009 <- readRDS("C:/Users/fabi_/Downloads/year2009_weather.RDS")



Weekly <- select(df, "SELECT ,DateTime,id, Global_active_power,Global_reactive_power,Global_intensity_Voltage_Sub_meteting_1,Sub_metering_2,Sub_metering_3,year,temperature,AC,WH,Light")

Weekly <- data.frame(rbind(df %>% group_by(month)) %>% filter (Sub_metering_1, Sub_metering_2, Sub_metering_3)

group.exp <- df %>% group_by(month)
group.exp


View(group.exp)






# Subset month

df_month_2 <- df %>%
  group_by(Week) %>%
  filter(DateTime,id, Global_active_power,Global_reactive_power,Global_intensity_Voltage_Sub_meteting_1,Sub_metering_2,Sub_metering_3)

by_month <- group_by(id, Global_active_power,Global_reactive_power,Global_intensity_Voltage_Sub_meteting_1,Sub_metering_2,Sub_metering_3, month)
by_month


# Subset hour

df_hr <- df %>%
  group_by(year, month, day, hour, ) %>%
  summarise(Sub_Metering_1=round(sum(Sub_metering_1), 3),
            Sub_Metering_2=round(sum(Sub_metering_2), 3),
            Sub_Metering_3=round(sum(Sub_metering_3), 3))


# Subset month

df_month <- df %>%
  group_by(year, month, ) %>%
  summarise(Sub_Metering_1=round(sum(Sub_metering_1), 3),
            Sub_Metering_2=round(sum(Sub_metering_2), 3),
            Sub_Metering_3=round(sum(Sub_metering_3), 3))

str(df_month)
View(df_month)




          
          

