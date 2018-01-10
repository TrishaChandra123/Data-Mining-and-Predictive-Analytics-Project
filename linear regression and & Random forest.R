#loading the dataset 
trial1 <- read.csv("/Users/sgunuganti/Documents/SEM2/Data Mining/dataset_Facebook.csv")

#entering mean values to replace missing values 
trial1$like <- ifelse(is.na(trial1$like), mean(trial1$like, na.rm=TRUE), trial1$like)
trial1$share <- ifelse(is.na(trial1$share), mean(trial1$share, na.rm=TRUE), trial1$share)

#determining class and conversion to factors
class(train.set1$Page.total.likes)
trial1$Category <- as.factor(trial1$Category)
class(trial1$Category)
class(trial1$Type)
class(trial1$Post.Hour)
class(trial1$Paid)
trial1$Post.Hour <- as.factor(trial1$Post.Hour)
trial1$Post.Weekday <- as.factor(trial1$Post.Weekday)
trial1$Post.Month <- as.factor(trial1$Post.Month)
trial1$Paid <- as.factor(trial1$Paid)

#spliiting 70:30; training ad test
set.seed(12345)
train.set <- sample(nrow(trial1),0.7*nrow(trial1))
train.set1 <- trial1[train.set,]
test.set1 <- trial1[-train.set,]

#1 Lifetime.Post.Total.Reach
fitlm1 <- lm(train.set1$Lifetime.Post.Total.Reach ~ . , data = train.set1[,-c(53:63)])
summary(fitlm1)

predictionlm1 <- predict(fitlm1,newdata=test.set1[-c(52:63)], interval = "confidence")
predictionlm1

performance.measures <- c("AE","RMSE","MAE") 
x1 <- mean(train.set1$Lifetime.Post.Total.Reach - predictionlm1)
x2 <- sqrt(mean((train.set1$Lifetime.Post.Total.Reach - predictionlm1)^2))
x3 <- mean(abs(train.set1$Lifetime.Post.Total.Reach - predictionlm1))
output <- c(x1, x2, x3)
metric.data <- data.frame(performance.measures, output)
metric.data

#2 Lifetime.Post.Total.Impressions 
fitlm2 <- lm(train.set1$Lifetime.Post.Total.Impressions ~ ., data = train.set1[,-c(52,54:63)])
summary(fitlm2)

predictionlm2 <- predict(fitlm2,newdata=test.set1[-c(52:63)], interval = "confidence")
predictionlm2

performance.measures2 <- c("AE","RMSE","MAE") 
x21 <- mean(train.set1$Lifetime.Post.Total.Impressions - predictionlm2)
x22 <- sqrt(mean(train.set1$Lifetime.Post.Total.Impressions - predictionlm2)^2)
x23 <- mean(abs(train.set1$Lifetime.Post.Total.Impressions - predictionlm2))
output2 <- c(x21, x22, x23)
metric.data2 <- data.frame(performance.measures2, output2)
metric.data2

#3 Lifetime.Engaged.Users 
fitlm3 <- lm(train.set1$Lifetime.Engaged.Users ~ . , data = train.set1[,-c(52:53,55:63)])
summary(fitlm3)

predictionlm3 <- predict(fitlm3,newdata=test.set1[-c(52:63)], interval = "confidence")
predictionlm3

performance.measures3 <- c("AE","RMSE","MAE") 
x31 <- mean(train.set1$Lifetime.Engaged.Users - predictionlm3)
x32 <- sqrt(mean(train.set1$Lifetime.Engaged.Users - predictionlm3)^2)
x33 <- mean(abs(train.set1$Lifetime.Engaged.Users - predictionlm3))
output3 <- c(x31, x32, x33)
metric.data3 <- data.frame(performance.measures3, output3)
metric.data3

#4 Lifetime.Post.Consumers
fitlm4 <- lm(train.set1$Lifetime.Post.Consumers ~ ., data = train.set1[,-c(52:54,56:63)])
summary(fitlm4)

predictionlm4 <- predict(fitlm4,newdata=test.set1[-c(52:63)], interval = "confidence")
predictionlm4

performance.measures4 <- c("AE","RMSE","MAE") 
x41 <- mean(train.set1$Lifetime.Post.Consumers - predictionlm4)
x42 <- sqrt(mean(train.set1$Lifetime.Post.Consumers - predictionlm4)^2)
x43 <- mean(abs(train.set1$Lifetime.Post.Consumers - predictionlm4))
output4 <- c(x41, x42, x43)
metric.data4 <- data.frame(performance.measures4, output4)
metric.data4

#5 Lifetime.Post.Consumptions 
fitlm5 <- lm(train.set1$Lifetime.Post.Consumptions ~ ., data = train.set1[,-c(52:55,57:63)])
summary(fitlm5)

predictionlm5 <- predict(fitlm5,newdata=test.set1[-c(52:63)], interval = "confidence")
predictionlm5

performance.measures5 <- c("AE","RMSE","MAE") 
x51 <- mean(train.set1$Lifetime.Post.Consumptions - predictionlm5)
x52 <- sqrt(mean(train.set1$Lifetime.Post.Consumptions - predictionlm5)^2)
x53 <- mean(abs(train.set1$Lifetime.Post.Consumptions - predictionlm5))
output5 <- c(x51, x52, x53)
metric.data5 <- data.frame(performance.measures5, output5)
metric.data5

#6 Lifetime.Post.Impressions.by.people.who.have.liked.your.Page
fitlm6 <- lm(train.set1$Lifetime.Post.Impressions.by.people.who.have.liked.your.Page ~ ., data = train.set1[,-c(52:56,58:63)])
summary(fitlm6)

predictionlm6 <- predict(fitlm6,newdata=test.set1[-c(52:63)], interval = "confidence")
predictionlm6

performance.measures6 <- c("AE","RMSE","MAE") 
x61 <- mean(train.set1$Lifetime.Post.Impressions.by.people.who.have.liked.your.Page - predictionlm6)
x62 <- sqrt(mean(train.set1$Lifetime.Post.Impressions.by.people.who.have.liked.your.Page - predictionlm6)^2)
x63 <- mean(abs(train.set1$Lifetime.Post.Impressions.by.people.who.have.liked.your.Page - predictionlm6))
output6 <- c(x61, x62, x63)
metric.data6 <- data.frame(performance.measures6, output6)
metric.data6

#7 Lifetime.Post.reach.by.people.who.like.your.Page
fitlm7 <- lm(train.set1$Lifetime.Post.reach.by.people.who.like.your.Page ~ ., data = train.set1[,-c(52:57,59:63)])
summary(fitlm7)

predictionlm7 <- predict(fitlm7,newdata=test.set1[-c(52:63)], interval = "confidence")
predictionlm7

performance.measures7 <- c("AE","RMSE","MAE") 
x71 <- mean(train.set1$Lifetime.Post.reach.by.people.who.like.your.Page - predictionlm7)
x72 <- sqrt(mean(train.set1$Lifetime.Post.reach.by.people.who.like.your.Page - predictionlm7)^2)
x73 <- mean(abs(train.set1$Lifetime.Post.reach.by.people.who.like.your.Page- predictionlm7))
output7 <- c(x71, x72, x73)
metric.data7 <- data.frame(performance.measures7, output7)
metric.data7

#8 Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post
fitlm8 <- lm(train.set1$Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post ~ ., data = train.set1[,-c(52:58,60:63)])
summary(fitlm8)

predictionlm8 <- predict(fitlm8,newdata=test.set1[-c(52:63)], interval = "confidence")
predictionlm8

performance.measures8 <- c("AE","RMSE","MAE") 
x81 <- mean(train.set1$Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post - predictionlm8)
x82 <- sqrt(mean(train.set1$Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post - predictionlm8)^2)
x83 <- mean(abs(train.set1$Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post - predictionlm8))
output8 <- c(x81, x82, x83)
metric.data8 <- data.frame(performance.measures8, output8)
metric.data8

#9 comment
fitlm9 <- lm(train.set1$comment ~ ., data = train.set1[,-c(52:59,61:63)])
summary(fitlm9)

predictionlm9 <- predict(fitlm9,newdata=test.set1[-c(52:63)], interval = "confidence")
predictionlm9

performance.measures9 <- c("AE","RMSE","MAE") 
x91 <- mean(train.set1$comment - predictionlm9)
x92 <- sqrt(mean(train.set1$comment - predictionlm9)^2)
x93 <- mean(abs(train.set1$comment - predictionlm9))
output9 <- c(x91, x92, x93)
metric.data9 <- data.frame(performance.measures9, output9)
metric.data9

#10 like 
fitlm10 <- lm(train.set1$like ~ ., data = train.set1[,-c(52:60,62:63)])
summary(fitlm10)

predictionlm10 <- predict(fitlm10,newdata=test.set1[-c(52:63)], interval = "confidence")
predictionlm10

performance.measures10 <- c("AE","RMSE","MAE") 
x101 <- mean(train.set1$like - predictionlm10)
x102 <- sqrt(mean(train.set1$like - predictionlm10)^2)
x103 <- mean(abs(train.set1$like- predictionlm10))
output10 <- c(x101, x102, x103)
metric.data10 <- data.frame(performance.measures10, output10)
metric.data10

#11 share
fitlm11 <- lm(train.set1$share ~ ., data = train.set1[,-c(52:61,63)])
summary(fitlm11)

predictionlm11 <- predict(fitlm11,newdata=test.set1[-c(52:63)], interval = "confidence")
predictionlm11

performance.measures11 <- c("AE","RMSE","MAE") 
x111 <- mean(train.set1$share - predictionlm11)
x112 <- sqrt(mean(train.set1$share - predictionlm11)^2)
x113 <- mean(abs(train.set1$share - predictionlm11))
output11 <- c(x111, x112, x113)
metric.data11 <- data.frame(performance.measures11, output11)
metric.data11


#12 Total.Interactions 
fitlm12 <- lm(train.set1$Total.Interactions ~ ., data = train.set1[,-c(52:62)])
summary(fitlm12)

predictionlm12 <- predict(fitlm12,newdata=test.set1[-c(52:63)], interval = "confidence")
predictionlm12

performance.measures12 <- c("AE","RMSE","MAE") 
x121 <- mean(train.set1$Total.Interactions - predictionlm12)
x122 <- sqrt(mean(train.set1$Total.Interactions - predictionlm12)^2)
x123 <- mean(abs(train.set1$Total.Interactions- predictionlm12))
output12 <- c(x121, x122, x123)
metric.data12 <- data.frame(performance.measures12, output12)
metric.data12

#consolidating the performance results
metric.datalm <- cbind(metric.data, metric.data2, metric.data3, metric.data4, metric.data4, metric.data5, metric.data6, metric.data7, metric.data8, metric.data9, metric.data10, metric.data11, metric.data12)

#writing performance results to a CSV
write.csv(metric.datalm, file = "/Users/sgunuganti/Documents/SEM2/Data Mining/metric.datalm.csv")


#install required packages for implementing Rnadom Forest
install.packages("tree")
library(tree)
install.packages("randomForest")
library(randomForest)

# specific seed for all models
set.seed(12345)

#1 Share
rf1 <- randomForest(train.set1$share ~ .,data = train.set1[,-c(8:17,19)], mtry = 3, importance = TRUE,na.action=na.exclude)
rf1
yhat.rf1 = predict(rf1 ,newdata=test.set1)
length(yhat.rf1)
mean((yhat.rf1 - test.set1$share)^2)
plot(yhat.rf1, test.set1$share)
abline(0,1)
y1 <- importance(rf1)
varImpPlot(rf1)


# Like
rf2 <- randomForest(train.set1$like ~ ., data = train.set1[,-c(8:16,18:19)], mtry = 3, importance = TRUE,na.action=na.exclude)
rf2
yhat.rf2 = predict(rf2 ,newdata=test.set1)
length(yhat.rf2)
mean((yhat.rf2 - test.set1$like)^2)
plot(yhat.rf2, test.set1$like)
abline(0,1)
y2 <- importance(rf2)
varImpPlot(rf2)

# Comment
rf3 <- randomForest(train.set1$comment ~ ., data = train.set1[,-c(8:15,17:19)], mtry = 3, importance = TRUE,na.action=na.exclude)
rf3
yhat.rf3 = predict(rf3 ,newdata=test.set1)
length(yhat.rf3)
mean((yhat.rf3 - test.set1$comment)^2)
plot(yhat.rf3, test.set1$comment)
abline(0,1)
y3 <- importance(rf3)
varImpPlot(rf3)

#4 Total Interactions
rf4 <- randomForest(train.set1$Total.Interactions ~ ., data = train.set1[,-c(8:18)], mtry = 3, importance = TRUE,na.action=na.exclude)
rf4
yhat.rf4 = predict(rf4 ,newdata=test.set1)
length(yhat.rf4)
mean((yhat.rf4 - test.set1$Total.Interactions)^2)
plot(yhat.rf4, test.set1$Total.Interactions)
abline(0,1)
y4 <- importance(rf4)
varImpPlot(rf4)

#5 Lifetime.Post.Consumers
rf5 <- randomForest(train.set1$Lifetime.Post.Consumers ~ ., data = train.set1[,-c(8:10,12:19)], mtry = 3, importance = TRUE)
rf5
yhat.rf5 = predict(rf5 ,newdata=test.set1[1:7])
length(yhat.rf5)
mean((yhat.rf5 - test.set1$Lifetime.Post.Consumers)^2)
plot(yhat.rf5, test.set1$Lifetime.Post.Consumers)
abline(0,1)
y5 <- importance(rf5)
y5
varImpPlot(rf5)

#6 Lifetime.Post.Total.Impressions
rf6 <- randomForest(train.set1$Lifetime.Post.Total.Impressions ~ ., data = train.set1[,-c(8,10:19)], mtry = 3, importance = TRUE,na.action=na.exclude)
rf6
yhat.rf6 = predict(rf6 ,newdata=test.set1)
yhat.rf6
length(yhat.rf6)
mean((yhat.rf6 - test.set1$Lifetime.Post.Consumers)^2)
plot(yhat.rf6, test.set1$Lifetime.Post.Consumers)
abline(0,1)
y6 <- importance(rf6)
varImpPlot(rf6)

#Consolidating the best results 
rf.featureimportance <- cbind(y1, y2, y3, y4)
rf.featureimportance

#writing to CSV 
write.csv(rf.featureimportance, file = "/Users/sgunuganti/Documents/SEM2/Data Mining/rf.featureimportance.csv")


