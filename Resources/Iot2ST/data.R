install.packages("prophet")
library(prophet)
library(forecast)

test <- read_csv("C:/Respaldo FR/DD/test.csv")
View(test)
train <- read_csv("C:/Respaldo FR/DD/train.csv")
View(train)



# Chaning Date & Time to time series ####
train$date <- as.POSIXct(train$date, "%d/%m/%Y") #Changing to time series
train$year <- year(train$date)
train$month <- month(train$date)
train$day <- day(train$date)

store1 <- filter(train, store==1)
store2 <- filter(train, store==2)
store3 <- filter(train, store==3)
store4 <- filter(train, store==4)
store5 <- filter(train, store==5)
store6 <- filter(train, store==6)
store7 <- filter(train, store==7)
store8 <- filter(train, store==8)
store9 <- filter(train, store==9)
store10 <- filter(train, store==10)


v <-  list(c(store1,store2,store3,store4,store5,store6,store7,store8,store9,store10))

#Time Series ####

#8.2 Subset month
train_month <- train %>%
  group_by(year, month, store) %>%
  summarise(sales = sum(sales))
view(train_month)


view(train_month)


#8.3 Subset daily ####
train_daily <- train %>%
  group_by(year,month,day,store) %>%
  summarise(sales= sum(sales))
view(train_daily)

# Creating monnth and Weekly series 

train_month_ts<- ts(train_month$month,
                 frequency=12,
                 start=c(2013,1),
                 end=c(2017,12))

plot.ts(train_month_ts)

train_days_ts<- ts(train_daily$day,
                  frequency=365,
                  start=c(2013,1),
                  end=c(2017,364))

train_days_ts
#Plot quarterly time series
plot(train_month_ts,
     plot.type='s',
     col=c('red', 'green', 'blue'),
     main='Total Quarterly Sales',
     xlab='Year', ylab = 'sales')
minor.tick(nx=4)
#Models ####
days_train <- window(train_days_ts, start=c(2013,1), end=c(2017,12))

ndiffs(train_month_ts)
diff(train_month_ts)

modelarima <- Arima(train_days_ts, order=c(0,0,1), seasonal = c(0,0,1))
modelarima

predictionsforecast1<- forecast(modelarima,h=12)
predictionsforecast1

history <- data.frame(
  ds = seq(as.Date('2017-01-01'), as.Date('2017-01-01'), by = 'd'),
  y = sin(1:366/200) + rnorm(366)/10)

m <- prophet(history)
m <- prophet(history)

{
  m <- prophet(train, daily.seasonality = TRUE)
  future <- make_future_dataframe(m, periods = 90)
  forecast  <- predict(m,future)
  print(i)
  i i+1
  xts :: last(forecast1^[,c ("yhat"),90])
  

start_time <- Sys.time()
i <-  1 

#split ####
#3
i<-1
predictfunction <- function(ts){
  fit <- HoltWinters(ts)
  forecast <- forecast(fit,h=90)
  print(i)
  i <<- i + 1
  forecast$mean
}
#4 rename o 3
split <- c("store", "item")
#1
train_splitting <- split(train, train[,split])

#2
a <- 1
ts_list <- list()
for (i in train_splitting){
  ts_list[[a]] <- ts(i[, "sales"], frequency=365, start=c(2013,1), end=c(2017,365))
  print(a)
  a <- a + 1
}

#4
prediction <- sapply(ts_list, predictfunction)
prediction
#5
prediction_vector <- as.numeric(prediction)


write.csv(sample_submission,"C:/Respaldo FR/DD/predictions.csv")



sample_submission <- read_csv("C:/Respaldo FR/DD/sample_submission.csv")
#6
sample_submission$sales <- prediction_vector
#7
sub1 <- sample_submission

head(sub1)


write.csv(sub1,"C:/Respaldo FR/DD/sub1.csv")




