#Project2
sdm.prj2 = read.csv('/Users/vinithavudhayagiri/Downloads/project2.txt',sep = ",",header=FALSE)

dim(sdm.prj2)

names(sdm.prj2)

summary(sdm.prj2)

attach(sdm.prj2)

#To measure the correlation coefficient value between two vectors.
cor(sdm.prj2)   


##Logistic regression
Logistic = glm ( V5 ~ V1 + V2 + V3 + V4 , family = binomial ,data = sdm.prj2 )
coef (Logistic)
summary (Logistic)
summary (Logistic)$coef
summary (Logistic)$coef[, 4]

#The probabilities are computed for the training data that was used to fit the logistic regression model.
probability = predict (Logistic , type = "response")
probability[1:10]

#The following two commands create a vector of class predictions based on whether the predicted probability of a market increase is greater than or less than 0.6.
predict = rep ("0", 1372)
predict[probability > .6] = "1"
mean (predict == V5)
table (predict , V5)

#splitting training data and test data
data.sample <- sample(c(TRUE, FALSE), nrow(sdm.prj2), replace=TRUE, prob=c(0.45,0.55))
train.data <- sdm.prj2[!data.sample, ]
dim(train.data)
test.data<- sdm.prj2[data.sample, ]
dim(test.data)

final <- predict (Logistic , newdata = test.data, type = "response")
final <- ifelse(final > 0.5,1,0)

Error.data <- mean(final != test.data$V5)
print(paste('Accuracy', 1 - Error.data))


##K-FOLD from 1 to 10 Cross Validation for Logistic Regression 
library(caret)
library(dplyr)
set.seed(100)
train.ctrl <- trainControl(method = "cv", number = 10, savePredictions=TRUE)
Logistic.fit<- train(factor(V5) ~ V1 + V2 + V3 + V4, data = sdm.prj2, method = "glm", family = "binomial", trControl=train.ctrl, tuneLength = 0)
Logistic.fit

predict <- Logistic.fit$pred
predict$equal <- ifelse(predict$pred == predict$obs, 1,0)

fold <- predict %>%                                      
  group_by(Resample) %>%                         
  summarise_at(vars(equal),                     
               list(Accuracy = mean))              
fold

## Quadratic Discriminant Analysis

library(MASS)
library(ggplot2)

qda.fit = qda(V5~V1+V2+V3+V4, data=train.data)
qda.fit

qda.class=predict(qda.fit,train.data)$class
table(qda.class)
mean(qda.class==V5)

##K-FOLD from 1 to 10 Cross Validation for Quadratic Discriminant Analysis
library(caret)
library(dplyr)
set.seed(100)
train.ctrl <- trainControl(method = "cv", number = 10, savePredictions=TRUE)
qda.fit<- train(factor(V5) ~ V1 + V2 + V3 + V4, data = sdm.prj2, method = "qda", trControl=train.ctrl, tuneLength = 0)
qda.fit

predict <- qda.fit$pred
predict$equal <- ifelse(predict$pred == predict$obs, 1,0)

fold <- predict %>%                                      
  group_by(Resample) %>%                         
  summarise_at(vars(equal),                     
               list(Accuracy = mean))              
fold



## Linear Discriminant Analysis

sdm.prj2[1:4] = scale(sdm.prj2[1:4])
lda <- lda(V5 ~ V1 + V2 + V3 + V4, data=train.data)
lda
lda.pred <- predict(lda, train.data)
names(lda.pred)
head(lda.pred$class)
head(lda.pred$posterior)
mean(lda.pred$class == test.data$V5)

##K-FOLD from 1 to 10 Cross Validation for Linear Discriminant Analysis
library(caret)
library(dplyr)
set.seed(100)
train.ctrl <- trainControl(method = "cv", number = 10, savePredictions=TRUE)
lda.fit<- train(factor(V5) ~ V1 + V2 + V3 + V4, data = sdm.prj2, method = "lda", trControl=train.ctrl, tuneLength = 0)
lda.fit

predict <- lda.fit$pred
predict$equal <- ifelse(predict$pred == predict$obs, 1,0)

fold <- predict %>%                                      
  group_by(Resample) %>%                         
  summarise_at(vars(equal),                     
               list(Accuracy = mean))              
fold


## KNN Classifier

library(e1071)
library(class)
library(caTools)

test = scale(test.data[, 1:4])
train = scale(train.data[, 1:4])
knn.clf = knn(train = train, test = test, cl = train.data$V5, k = 5)
clf<- table(test.data$V5,knn.clf)
clf
mean (test.data$V5 ==knn.clf)
knn.clf = knn(train = train, test = test, cl = train.data$V5, k = 10)
clf<- table(test.data$V5,knn.clf)
clf
mean (test.data$V5 ==knn.clf)

#K-FOLD from 1 to 10 Cross Validation for KNN
library(caret)
library(dplyr)
set.seed(100)
train.ctrl <- trainControl(method = "cv", number = 10, savePredictions=TRUE)
knn.fit<- train(factor(V5) ~ V1 + V2 + V3 + V4, data = sdm.prj2, method = "knn", trControl=train.ctrl, tuneLength = 0)
knn.fit

predict <- knn.fit$pred
predict$equal <- ifelse(predict$pred == predict$obs, 1,0)

fold <- predict %>%                                      
  group_by(Resample) %>%                         
  summarise_at(vars(equal),                     
               list(Accuracy = mean))              
fold

