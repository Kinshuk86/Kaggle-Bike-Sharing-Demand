## Below code is to get the Bike Rental Data in R-Studio

# Check the working Directory
getwd()

# Set the working Directory to be the Dataset Directory
setwd("C:/Users/Kinshuk/Desktop/Bike Sharing Demand")

# Read the data file from the working directory
bike_test <- read.csv("test.csv")
bike_train <- read.csv("train.csv")

## Check the structure and summary of the data
str(bike_train)
summary(bike_train)

## Get the glimpse of first and last few rows of Data

head(bike_train)
tail(bike_train)

## Data Wrangling
install.packages("dplyr")
library(dplyr)
tbl_df(bike_train)  # To view the data as per the size of the console

## DO not change the Original dataset. Apply Wrangling on Training dataset

bike_sharing_train <- bike_train
## Change Season column with proper value

bike_sharing_train <- mutate(bike_sharing_train, 
                       season = ifelse(season ==1, 'Spring', season),
                       season = ifelse(season ==2, 'Summer', season),  
                       season = ifelse(season ==3, 'Fall', season),
                       season = ifelse(season ==4, 'Winter', season))

bike_test <- mutate(bike_test, 
                             season = ifelse(season ==1, 'Spring', season),
                             season = ifelse(season ==2, 'Summer', season),  
                             season = ifelse(season ==3, 'Fall', season),
                             season = ifelse(season ==4, 'Winter', season))
## Extract Year column with proper Value

bike_sharing_train$year=substr(bike_sharing_train$datetime,1,4)

bike_test$year=substr(bike_test$datetime,1,4)

## Extract Hour from datetime and add new column
bike_sharing_train$hour=substr(bike_sharing_train$datetime,12,13)
bike_sharing_train$hour=as.factor(bike_sharing_train$hour)
bike_sharing_train$hour=as.integer(bike_sharing_train$hour)

bike_test$hour=substr(bike_test$datetime,12,13)
bike_test$hour=as.factor(bike_test$hour)
bike_test$hour=as.integer(bike_test$hour)

## Change Month column with proper Value

bike_sharing_train$month=substr(bike_sharing_train$datetime,6,7)
bike_sharing_train <- mutate(bike_sharing_train,
                        month = ifelse(month ==1, 'Jan', month),
                        month = ifelse(month ==2, 'Feb', month),
                        month = ifelse(month ==3, 'Mar', month),
                        month = ifelse(month ==4, 'Apr', month),
                        month = ifelse(month ==5, 'May', month),
                        month = ifelse(month ==6, 'Jun', month),
                        month = ifelse(month ==7, 'Jul', month),
                        month = ifelse(month ==8, 'Aug', month),
                        month = ifelse(month ==9, 'Sep', month),
                        month = ifelse(month ==10, 'Oct', month),
                        month = ifelse(month ==11, 'Nov', month),
                        month = ifelse(month ==12, 'Dec', month))

bike_test$month=substr(bike_test$datetime,6,7)
bike_test <- mutate(bike_test,
                             month = ifelse(month ==1, 'Jan', month),
                             month = ifelse(month ==2, 'Feb', month),
                             month = ifelse(month ==3, 'Mar', month),
                             month = ifelse(month ==4, 'Apr', month),
                             month = ifelse(month ==5, 'May', month),
                             month = ifelse(month ==6, 'Jun', month),
                             month = ifelse(month ==7, 'Jul', month),
                             month = ifelse(month ==8, 'Aug', month),
                             month = ifelse(month ==9, 'Sep', month),
                             month = ifelse(month ==10, 'Oct', month),
                             month = ifelse(month ==11, 'Nov', month),
                             month = ifelse(month ==12, 'Dec', month))
## Change Weekday Column with Proper Value

date = substr(bike_sharing_train$datetime,1,10)
days <- weekdays(as.Date(date))
bike_sharing_train$weekday = days

date1 = substr(bike_test$datetime,1,10)
days1 <- weekdays(as.Date(date1))
bike_test$weekday = days1

## Change Holiday Column with Proper Value

bike_sharing_train <- mutate(bike_sharing_train,
                        holiday = ifelse(holiday ==1, 'Public Holiday', 'Not A Public Holiday'))

bike_test <- mutate(bike_test,
                             holiday = ifelse(holiday ==1, 'Public Holiday', 'Not A Public Holiday'))
## Change Working Day with Proper Value

bike_sharing_train <- mutate(bike_sharing_train,
                        workingday = ifelse(workingday ==1, 'On Day', 'Off Day'))

bike_test <- mutate(bike_test,
                             workingday = ifelse(workingday ==1, 'On Day', 'Off Day'))

## Change Weather Situation with Proper Value

bike_sharing_train <- mutate(bike_sharing_train,
                        weather = ifelse(weather ==1, 'Clear', weather),
                        weather = ifelse(weather ==2, 'Mist', weather),
                        weather = ifelse(weather ==3, 'Light Rain/Snow', weather),
                        weather = ifelse(weather ==4, 'Heavy Rain/Snow', weather))

bike_test <- mutate(bike_test,
                             weather = ifelse(weather ==1, 'Clear', weather),
                             weather = ifelse(weather ==2, 'Mist', weather),
                             weather = ifelse(weather ==3, 'Light Rain/Snow', weather),
                             weather = ifelse(weather ==4, 'Heavy Rain/Snow', weather))
## Histogram of Variables to get some insights

bike_sharing_train$season=as.factor(bike_sharing_train$season)
bike_sharing_train$weather=as.factor(bike_sharing_train$weather)
bike_sharing_train$holiday=as.factor(bike_sharing_train$holiday)
bike_sharing_train$workingday=as.factor(bike_sharing_train$workingday)

bike_test$season=as.factor(bike_test$season)
bike_test$weather=as.factor(bike_test$weather)
bike_test$holiday=as.factor(bike_test$holiday)
bike_test$workingday=as.factor(bike_test$workingday)

install.packages("ggplot2")
install.packages("gridExtra")
library("ggplot2")
library(gridExtra)

 p1 <- ggplot(aes(x = season), data = bike_sharing_train) +
  geom_histogram() +
  ggtitle("Season Vs Count") 

p2 <- ggplot(aes(x = humidity), data = bike_sharing_train) +
  geom_histogram() +
  scale_x_discrete(breaks = seq(0, 100, 10)) +
  ggtitle("Humidity Vs Count")

p3 <- ggplot(aes(x = holiday), data = bike_sharing_train) +
  geom_histogram() +
  ggtitle("Holiday Vs Count")

p4 <- ggplot(aes(x = workingday), data = bike_sharing_train) +
  geom_histogram() +
  ggtitle("Workingday Vs Count")

p5 <- ggplot(aes(x = temp), data = bike_sharing_train) +
  geom_histogram() + 
  ggtitle("Actual Temperature Vs Count")

p6 <- ggplot(aes(x = atemp), data = bike_sharing_train) +
  geom_histogram()+
  ggtitle("Feels Like Temperature Vs Count")

p7 <- ggplot(aes(x = windspeed), data = bike_sharing_train) +
  geom_histogram() +
  ggtitle("Windspeed Vs Count")

p8 <- ggplot(aes(x = weather), data = bike_sharing_train) +
  geom_histogram() +
  ggtitle("Weather Situation Vs Count")

p9 <- ggplot(aes(x = weekday), data = bike_sharing_train) +
  geom_histogram() +
  ggtitle("Day Of Week Vs Count")

p10 <- ggplot(aes(x = hour), data = bike_sharing_train) +
  geom_histogram() +
  scale_x_discrete(breaks = seq(0, 23, 2)) +
  ggtitle("Hourly Trend Vs Count")

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, ncol = 2)


## Trying to see if Boxplot predicts something different

par(mfrow = c(1,1))  

boxplot(bike_sharing_train$count~bike_sharing_train$hour,xlab="hour", ylab="count of users")

# Scaling Y-axis to see the affect

boxplot(log(bike_sharing_train$count)~bike_sharing_train$hour,xlab="hour", ylab="count of users")
boxplot(sqrt(bike_sharing_train$count)~bike_sharing_train$hour,xlab="hour", ylab="count of users")


## Analysis on Two Variables

boxplot(bike_sharing_train$registered~bike_sharing_train$weekday,xlab="Day", ylab="Registered")
boxplot(bike_sharing_train$casual~bike_sharing_train$weekday,xlab="Day", ylab="Casual")

boxplot(bike_sharing_train$registered~bike_sharing_train$weather,xlab="Day", ylab="Registered")
boxplot(bike_sharing_train$casual~bike_sharing_train$weather,xlab="Day", ylab="Casual")

boxplot(bike_sharing_train$count~bike_sharing_train$year, xlab ="year", ylab ="count")
boxplot(bike_sharing_train$registered~bike_sharing_train$year, xlab ="year", ylab ="registered count")
boxplot(bike_sharing_train$casual~bike_sharing_train$year, xlab ="year", ylab ="casual count")


## Correlation Among the variables

corrleation =data.frame(bike_sharing_train$registered,bike_sharing_train$casual,bike_sharing_train$count,bike_sharing_train$temp,bike_sharing_train$humidity,bike_sharing_train$atemp,bike_sharing_train$windspeed)
cor(corrleation)

# Add new pedictor columns in test dataset

bike_test$registered = 0
bike_test$casual = 0
bike_test$count = 0
# Modelling Analysis Starts

## Linear Regression
RentReg = lm(registered ~ hour + atemp + year + temp + humidity + season + windspeed + workingday + weekday + weather + holiday, data = bike_sharing_train)
summary(RentReg)

SSE <- sum(RentReg$residuals)
SSE
RMSE <- sqrt(SSE/nrow(bike_sharing_train))
RMSE

reg_pred <- predict(RentReg, bike_test)
SSE1 <- sum((reg_pred - bike_sharing_train$registered)^2)
SSE1
SST <- sum((mean(bike_sharing_train$registered) - bike_test$registered)^2)
SST
R2 <- 1 - SSE1/SST
R2

RMSE1 <- sqrt(SSE1/nrow(bike_test))
RMSE1

#Second Linear Regression Model
RentReg1 = lm(registered ~ hour + atemp + year + temp + humidity + season + windspeed + workingday + weather, data = bike_sharing_train)
summary(RentReg1)

SSE2 <- sum(RentReg1$residuals)
SSE2
RMSE2 <- sqrt(SSE2/nrow(bike_sharing_train))
RMSE2

reg_pred1 <- predict(RentReg1, bike_test)
SSE3 <- sum((reg_pred1 - bike_sharing_train$registered)^2)
SSE3
SST1 <- sum((mean(bike_sharing_train$registered) - bike_test$registered)^2)
SST1
R21 <- 1 - SSE2/SST1
R21

RMSE3 <- sqrt(SSE3/nrow(bike_test))
RMSE3


## Decision Trees Model
install.packages("rattle")
library(rpart)
library(rattle)

install.packages("rpart.plot")
library(rpart.plot)
library(RColorBrewer)
set.seed(26)

## randomly choose 70% of the data set as training data

bike.train.indices <- sample(1:nrow(bike_sharing_train), 0.7*nrow(bike_sharing_train), replace=F)
bike.train <- bike_sharing_train[bike.train.indices,]
dim(bike.train)

## select the 30% left as the testing data
bike.test <- bike_sharing_train[-bike.train.indices,]
dim(bike.test)

model1 = rpart(registered ~ hour + atemp + temp + humidity + season, data=bike_sharing_train)
fancyRpartPlot(model1)
prp(model1)

model2 = rpart(casual ~ hour + atemp + temp + humidity + season,data=bike_sharing_train)
fancyRpartPlot(model2)
prp(model2)

summary(model1)
summary(model2)


## MODEL EVALUATION
bike_prediction <- predict(model1, bike_test)
bike_pred_cas <- predict(model2, bike_test)

bike_test$registered <- bike_prediction
bike_test$registered <- as.integer(bike_test$registered)

bike_test$casual <- bike_pred_cas
bike_test$casual <- as.integer(bike_test$casual)

bike_test$count <- bike_test$registered + bike_test$casual

s <- data.frame(datetime=bike_test$datetime,count=bike_test$count)
write.csv(s, file="Dtree.csv",row.names=FALSE)

## Second Algorithm Random Forest
install.packages("randomForest")
library(randomForest)
set.seed(415)

bike_sharing_train$season=as.factor(bike_sharing_train$season)
bike_sharing_train$holiday=as.factor(bike_sharing_train$holiday)
bike_sharing_train$workingday=as.factor(bike_sharing_train$workingday)
bike_sharing_train$weather=as.factor(bike_sharing_train$weather)
bike_sharing_train$hour=as.factor(bike_sharing_train$hour)
bike_sharing_train$month=as.factor(bike_sharing_train$month)
bike_sharing_train$weekday=as.factor(bike_sharing_train$weekday)

bike_test$season=as.factor(bike_test$season)
bike_test$holiday=as.factor(bike_test$holiday)
bike_test$workingday=as.factor(bike_test$workingday)
bike_test$weather=as.factor(bike_test$weather)
bike_test$hour=as.factor(bike_test$hour)
bike_test$month=as.factor(bike_test$month)
bike_test$weekday=as.factor(bike_test$weekday)

RFM1 <- randomForest(registered ~ hour + workingday + weekday + holiday + temp + humidity + atemp +windspeed + season + weather + year, data = bike_sharing_train, importance=TRUE, ntree = 250)
print(RFM1)

RFM2 <- randomForest(casual ~ hour + workingday + weekday + holiday + temp + humidity + atemp +windspeed + season + weather + year, data = bike_sharing_train, importance=TRUE, ntree = 250)
print(RFM2)

Reg_predict <- predict(RFM1, bike_test, type = "response")
Cas_predict <- predict(RFM2, bike_test, type = "response")


bike_test$reg <- Reg_predict
bike_test$cas <- Cas_predict
bike_test$cnt <- bike_test$reg + bike_test$cas

bike_test$reg <- as.integer(bike_test$reg)
bike_test$cas <- as.integer(bike_test$cas)
bike_test$cnt <- as.integer(bike_test$cnt)
r <- data.frame(datetime=bike_test$datetime,count=bike_test$cnt)
q <- data.frame(datetime=bike_test$datetime,count=bike_test$cnt)
write.csv(r, file="RanFor.csv",row.names=FALSE)
write.csv(q, file="RanFor1.csv",row.names=FALSE)
## show variable importance
importance(RFM1)
varImpPlot(RFM1)

importance(RFM2)
varImpPlot(RFM2)

RFM3 <- randomForest(registered ~ hour + year + humidity + workingday + temp + atemp, data = bike_sharing_train, importance=TRUE, ntree = 250)
print(RFM3)

RFM4 <- randomForest(casual ~ hour + year + humidity + workingday + temp + atemp, data = bike_sharing_train, importance=TRUE, ntree = 250)
print(RFM4)

Reg_predict1 <- predict(RFM3, bike_test, type = "response")
Cas_predict1 <- predict(RFM4, bike_test, type = "response")
varImpPlot(RFM3)
varImpPlot(RFM4)

bike_test$reg1 <- Reg_predict1
bike_test$cas1 <- Cas_predict1
bike_test$cnt1 <- bike_test$reg1 + bike_test$cas1

bike_test$reg1 <- as.integer(bike_test$reg1)
bike_test$cas1 <- as.integer(bike_test$cas1)
bike_test$cnt1 <- as.integer(bike_test$cnt1)
p <- data.frame(datetime=bike_test$datetime,count=bike_test$cnt1)
write.csv(p, file="RanFor2.csv",row.names=FALSE)


bike_sharing_train$reg2=bike_sharing_train$registered+1
bike_sharing_train$cas2=bike_sharing_train$casual+1
bike_sharing_train$logcas=log(bike_sharing_train$cas2)
bike_sharing_train$logreg=log(bike_sharing_train$reg2)


RFM3 <- randomForest(logreg ~ hour + year + humidity + workingday + temp + atemp + season + holiday, data = bike_sharing_train, importance=TRUE, ntree = 250)
print(RFM3)
summary(RFM3)
RFM4 <- randomForest(logcas ~ hour + year + humidity + workingday + temp + atemp + season + holiday, data = bike_sharing_train, importance=TRUE, ntree = 250)
print(RFM4)

Reg_predict2 <- predict(RFM3, bike_test, type = "response")
Cas_predict2 <- predict(RFM4, bike_test, type = "response")
summary(Reg_predict2)
bike_test$reg3 <- Reg_predict2
bike_test$cas3 <- Cas_predict2
bike_test$cnt3 <- bike_test$reg3 + bike_test$cas3

bike_test$reg3 <- as.integer(bike_test$reg3)
bike_test$cas3 <- as.integer(bike_test$cas3)
bike_test$cnt3 <- as.integer(bike_test$cnt3)

bike_test$reg1=exp(bike_test$reg1)-1
bike_test$cas1=exp(bike_test$cas1)-1
bike_test$cnt1=bike_test$cas1+bike_test$reg1
t <- data.frame(datetime=bike_test$datetime,count=bike_test$cnt1)
write.csv(t, file="RanFor7.csv",row.names=FALSE)
