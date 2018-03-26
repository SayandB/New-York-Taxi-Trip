library(caret)
library(rpart)
library(randomForest)
library(xgboost)
library(readr)

train <- read_csv("C:/Users/sayan/Desktop/train_2/train.csv")
test <- read_csv("C:/Users/sayan/Desktop/test_2/test.csv")

data<-train
time <- substr(data$pickup_datetime,12,16)

data$hour<- substr(time, 1,2)
data$hour <- as.integer(data$hour)
data$min <- substr(time, 4,5)
data$min <- as.integer(data$min)
data$pickup_datetime <- as.Date(data$pickup_datetime, "%d-%m-%Y %H:%M")

data$day <- weekdays(as.Date(data$pickup_datetime))
data$day[data$day=="Sunday"]<-1
data$day[data$day=="Monday"]<-2
data$day[data$day=="Tuesday"]<-3
data$day[data$day=="Wednesday"]<-4
data$day[data$day=="Thursday"]<-5
data$day[data$day=="Friday"]<-6
data$day[data$day=="Saturday"]<-7
data$day <- as.integer(data$day)

data$month <- months(as.Date(data$pickup_datetime))
data$month[data$month=="January"]<- 1
data$month[data$month=="February"]<- 2
data$month[data$month=="March"]<- 3
data$month[data$month=="April"]<- 4
data$month[data$month=="May"]<- 5
data$month[data$month=="June"]<- 6
data$month <- as.integer(data$month)

data <- data[,c(-1,-3,-4)]

data$store_and_fwd_flag[data$store_and_fwd_flag=="N"]<-0
data$store_and_fwd_flag[data$store_and_fwd_flag=="Y"]<-1
data$store_and_fwd_flag<-as.integer(data$store_and_fwd_flag)
temp <- data
temp <- temp[,c(-8)]
a < as.matrix(temp)
temp <-a 

#Cleaning test data
data <- test
time <- substr(data$pickup_datetime,12,16)

data$hour<- substr(time, 1,2)
data$hour <- as.integer(data$hour)
data$min <- substr(time, 4,5)
data$min <- as.integer(data$min)
data$pickup_datetime <- as.Date(data$pickup_datetime, "%d-%m-%Y %H:%M")

data$day <- weekdays(as.Date(data$pickup_datetime))
data$day[data$day=="Sunday"]<-1
data$day[data$day=="Monday"]<-2
data$day[data$day=="Tuesday"]<-3
data$day[data$day=="Wednesday"]<-4
data$day[data$day=="Thursday"]<-5
data$day[data$day=="Friday"]<-6
data$day[data$day=="Saturday"]<-7
data$day <- as.integer(data$day)

data$month <- months(as.Date(data$pickup_datetime))
data$month[data$month=="January"]<- 1
data$month[data$month=="February"]<- 2
data$month[data$month=="March"]<- 3
data$month[data$month=="April"]<- 4
data$month[data$month=="May"]<- 5
data$month[data$month=="June"]<- 6
data$month <- as.integer(data$month)

data <- data[,c(-1,-3)]

data$store_and_fwd_flag[data$store_and_fwd_flag=="N"]<-0
data$store_and_fwd_flag[data$store_and_fwd_flag=="Y"]<-1
data$store_and_fwd_flag<-as.integer(data$store_and_fwd_flag)
t <- data
b <- as.matrix(t)
t <- b

data <- train

fit <- xgboost(data = temp, 
               booster = "gbtree", 
               max.depth = 10,
               objective = "reg:linear",
               label = data$trip_duration,
               nrounds = 1000)

mat <- xgb.importance(feature_names=colnames(temp), model=fit)
xgb.plot.importance(importance_matrix = mat[1:11])

res <- predict(fit, t)
res1 <- as.data.frame(res)
res1$id <- test$id
write.csv(result_nyc ,file = "nyc.csv")
