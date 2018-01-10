#enter the path where the file is stored to load input dataset
fbdata <- read.csv("dataset_Facebook.csv")
View(fbdata)

#creating factors of independent variables
fbdata$Category <- as.factor(fbdata$Category)
fbdata$Post.Hour <- as.factor(fbdata$Post.Hour)
fbdata$Post.Weekday <- as.factor(fbdata$Post.Weekday)
fbdata$Post.Month <- as.factor(fbdata$Post.Month)
fbdata$Paid <- as.factor(fbdata$Paid)

#Replacing the null values with mean value
fbdata$like <- ifelse(is.na(fbdata$like), mean(fbdata$like, na.rm=TRUE), fbdata$like)
fbdata$share <- ifelse(is.na(fbdata$share), mean(fbdata$share, na.rm=TRUE), fbdata$share)

#installing package tree
install.packages("tree")

#seting seed
set.seed(12345)
library(tree)

#splitting the dataset into training ans testing in 70:30
train <- sample(nrow(fbdata),0.7*nrow(fbdata))
df_train <- fbdata[train,]
df_test <- fbdata[-train,]

#1 Running regression tree and calculating performance metrics 
fbtree1 <- tree(Lifetime.Post.Total.Reach~ Page.total.likes + Type + Category + Post.Month + Post.Weekday + Post.Hour + Paid, df_train)
summary(fbtree1)
plot(fbtree1)
text(fbtree1, pretty=0)
fbtree1
predictionlm1 <- predict(fbtree1,newdata=df_test[,-c(9:19)])


performance.measures1 <- c("AE","RMSE","MAE") 
x11 <- mean(df_test$Lifetime.Post.Total.Reach-predictionlm1)
x12 <- sqrt(mean(df_test$Lifetime.Post.Total.Reach-predictionlm1)^2)
x13 <- mean(abs(df_test$Lifetime.Post.Total.Reach-predictionlm1))
output1 <- c(x11, x12, x13)
metric.data1 <- data.frame(performance.measures1, output1)
metric.data1



#2 Running regression tree and calculating performance metrics 
fbtree2 <- tree(Lifetime.Post.Total.Impressions~ Page.total.likes + Type + Category + Post.Month + Post.Weekday + Post.Hour + Paid, df_train)
summary(fbtree2)
plot(fbtree2)
text(fbtree2, pretty=0)
fbtree2
predictionlm2 <- predict(fbtree2,newdata=df_test[,-c(8,10:19)])

performance.measures2 <- c("AE","RMSE","MAE") 
x21 <- mean(df_test$Lifetime.Post.Total.Impressions-predictionlm2)
x22 <- sqrt(mean(df_test$Lifetime.Post.Total.Impressions-predictionlm2)^2)
x23 <- mean(abs(df_test$Lifetime.Post.Total.Impressions-predictionlm2))
output2 <- c(x21, x22, x23)
metric.data2 <- data.frame(performance.measures2, output2)
metric.data2


#3 Running regression tree and calculating performance metrics 
fbtree3 <- tree(Lifetime.Engaged.Users~ Page.total.likes + Type + Category + Post.Month + Post.Weekday + Post.Hour + Paid, df_train)
summary(fbtree3)
plot(fbtree3)
text(fbtree3, pretty=0)
fbtree3
predictionlm3 <- predict(fbtree3,newdata=df_test[,-c(8:9,11:19)])

performance.measures3 <- c("AE","RMSE","MAE") 
x31 <- mean(df_test$Lifetime.Engaged.Users-predictionlm3)
x32 <- sqrt(mean(df_test$Lifetime.Engaged.Users-predictionlm3)^2)
x33 <- mean(abs(df_test$Lifetime.Engaged.Users-predictionlm3))
output3 <- c(x31, x32, x33)
metric.data3 <- data.frame(performance.measures3, output3)
metric.data3

#4 Running regression tree and calculating performance metrics 
fbtree4 <- tree(Lifetime.Post.Consumers~ Page.total.likes + Type + Category + Post.Month + Post.Weekday + Post.Hour + Paid, df_train)
summary(fbtree4)
plot(fbtree4)
text(fbtree4, pretty=0)
fbtree4
predictionlm4 <- predict(fbtree4,newdata=df_test[,-c(8:10,12:19)])

performance.measures4 <- c("AE","RMSE","MAE") 
x41 <- mean(df_test$Lifetime.Post.Consumers-predictionlm4)
x42 <- sqrt(mean(df_test$Lifetime.Post.Consumers-predictionlm4)^2)
x43 <- mean(abs(df_test$Lifetime.Post.Consumers-predictionlm4))
output4 <- c(x41, x42, x43)
metric.data4 <- data.frame(performance.measures4, output4)
metric.data4

#5 Running regression tree and calculating performance metrics 
fbtree5 <- tree(Lifetime.Post.Consumptions~ Page.total.likes + Type + Category + Post.Month + Post.Weekday + Post.Hour + Paid, df_train)
summary(fbtree5)
plot(fbtree5)
text(fbtree5, pretty=0)
fbtree5
predictionlm5 <- predict(fbtree5,newdata=df_test[,-c(8:11,13:19)])

performance.measures5 <- c("AE","RMSE","MAE") 
x51 <- mean(df_test$Lifetime.Post.Consumptions-predictionlm5)
x52 <- sqrt(mean(df_test$Lifetime.Post.Consumptions-predictionlm5)^2)
x53 <- mean(abs(df_test$Lifetime.Post.Consumptions-predictionlm5))
output5 <- c(x51, x52, x53)
metric.data5 <- data.frame(performance.measures5, output5)
metric.data5

#6 Running regression tree and calculating performance metrics 
fbtree6 <- tree(Lifetime.Post.Impressions.by.people.who.have.liked.your.Page~ Page.total.likes + Type + Category + Post.Month + Post.Weekday + Post.Hour + Paid, df_train)
summary(fbtree6)
plot(fbtree6)
text(fbtree6, pretty=0)
fbtree6
predictionlm6 <- predict(fbtree6,newdata=df_test[,-c(8:12,14:19)])

performance.measures6 <- c("AE","RMSE","MAE") 
x61 <- mean(df_test$Lifetime.Post.Impressions.by.people.who.have.liked.your.Page-predictionlm6)
x62 <- sqrt(mean(df_test$Lifetime.Post.Impressions.by.people.who.have.liked.your.Page-predictionlm6)^2)
x63 <- mean(abs(df_test$Lifetime.Post.Impressions.by.people.who.have.liked.your.Page-predictionlm6))
output6 <- c(x61, x62, x63)
metric.data6 <- data.frame(performance.measures6, output6)
metric.data6


#7 Running regression tree and calculating performance metrics 
fbtree7 <- tree(Lifetime.Post.reach.by.people.who.like.your.Page~ Page.total.likes + Type + Category + Post.Month + Post.Weekday + Post.Hour + Paid, df_train)
summary(fbtree7)
plot(fbtree7)
text(fbtree7, pretty=0)
fbtree7
predictionlm7 <- predict(fbtree7,newdata=df_test[,-c(8:13,15:19)])

performance.measures7 <- c("AE","RMSE","MAE") 
x71 <- mean(df_test$Lifetime.Post.reach.by.people.who.like.your.Page-predictionlm7)
x72 <- sqrt(mean(df_test$Lifetime.Post.reach.by.people.who.like.your.Page-predictionlm7)^2)
x73 <- mean(abs(df_test$Lifetime.Post.reach.by.people.who.like.your.Page-predictionlm7))
output7 <- c(x71, x72, x73)
metric.data7 <- data.frame(performance.measures7, output7)
metric.data7

#8 Running regression tree and calculating performance metrics 
fbtree8 <- tree(Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post~ Page.total.likes + Type + Category + Post.Month + Post.Weekday + Post.Hour + Paid, df_train)
summary(fbtree8)
plot(fbtree8)
text(fbtree8, pretty=0)
fbtree8
predictionlm8 <- predict(fbtree8,newdata=df_test[,-c(8:14,16:19)])

performance.measures8 <- c("AE","RMSE","MAE") 
x81 <- mean(df_test$Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post-predictionlm8)
x82 <- sqrt(mean(df_test$Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post-predictionlm8)^2)
x83 <- mean(abs(df_test$Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post-predictionlm8))
output8 <- c(x81, x82, x83)
metric.data8 <- data.frame(performance.measures8, output8)
metric.data8

#9 Running regression tree and calculating performance metrics 
fbtree9 <- tree(comment~ Page.total.likes + Type + Category + Post.Month + Post.Weekday + Post.Hour + Paid, df_train)
summary(fbtree9)
plot(fbtree9)
text(fbtree9, pretty=0)
fbtree9
predictionlm9 <- predict(fbtree9,newdata=df_test[,-c(8:15,17:19)])

performance.measures9 <- c("AE","RMSE","MAE") 
x91 <- mean(df_test$comment-predictionlm9)
x92 <- sqrt(mean(df_test$comment-predictionlm9)^2)
x93 <- mean(abs(df_test$comment-predictionlm9))
output9 <- c(x91, x92, x93)
metric.data9 <- data.frame(performance.measures9, output9)
metric.data9

#10 Running regression tree and calculating performance metrics 
fbtree10 <- tree(like~ Page.total.likes + Type + Category + Post.Month + Post.Weekday + Post.Hour + Paid, df_train)
summary(fbtree10)
plot(fbtree10)
text(fbtree10, pretty=0)
fbtree10
predictionlm10 <- predict(fbtree10,newdata=df_test[,-c(8:16,18:19)])

performance.measures10 <- c("AE","RMSE","MAE") 
x101 <- mean(df_test$like-predictionlm10)
x102 <- sqrt(mean(df_test$like-predictionlm10)^2)
x103 <- mean(abs(df_test$like-predictionlm10))
output10 <- c(x101, x102, x103)
metric.data10 <- data.frame(performance.measures10, output10)
metric.data10

#11 Running regression tree and calculating performance metrics 
fbtree11 <- tree(share~ Page.total.likes + Type + Category + Post.Month + Post.Weekday + Post.Hour + Paid, df_train)
summary(fbtree11)
plot(fbtree11)
text(fbtree11, pretty=0)
fbtree11
predictionlm11 <- predict(fbtree11,newdata=df_test[,-c(8:17,19)])

performance.measures11 <- c("AE","RMSE","MAE") 
x111 <- mean(df_test$share-predictionlm11)
x112 <- sqrt(mean(df_test$share-predictionlm11)^2)
x113 <- mean(abs(df_test$share-predictionlm11))
output11 <- c(x111, x112, x113)
metric.data11 <- data.frame(performance.measures11, output11)
metric.data11


#12 Running regression tree and calculating performance metrics 
fbtree12 <- tree(Total.Interactions~ Page.total.likes + Type + Category + Post.Month + Post.Weekday + Post.Hour + Paid, df_train)
summary(fbtree12)
plot(fbtree12)
text(fbtree12, pretty=0)
fbtree12
predictionlm12 <- predict(fbtree12,newdata=df_test[,-c(8:18)])

performance.measures12 <- c("AE","RMSE","MAE") 
x121 <- mean(df_test$Total.Interactions-predictionlm12)
x122 <- sqrt(mean(df_test$Total.Interactions-predictionlm12)^2)
x123 <- mean(abs(df_test$Total.Interactions-predictionlm12))
output12 <- c(x121, x122, x123)
metric.data12 <- data.frame(performance.measures12, output12)
metric.data12

#Compiling all the errors and writing the result in a CSV
Dependent_variables <- c("Lifetime Post Total Reach","Lifetime Post Total Impressions","Lifetime Engaged Users","Lifetime Post Consumers","Lifetime Post Consumptions","Lifetime Post Impressions by people who have liked your Page","Lifetime Post reach by people who like your Page","Lifetime People who have liked your Page and engaged with your post","Comment","Like","Share","Total Interactions")
AE_all <- c(x11,x21,x31,x41,x51,x61,x71,x81,x91,x101,x111,x121)
RMSE_all <- c(x12,x22,x32,x42,x52,x62,x72,x82,x92,x102,x112,x122)
MAE_all <- c(x13,x23,x33,x43,x53,x63,x73,x83,x93,x103,x113,x123)
All_Errors <- data.frame(Dependent_variables,AE_all,RMSE_all,MAE_all)
write.csv(All_Errors, file = "C:/Users/LENOVO/Downloads/Errors.csv")


