#1
library(readxl)
health_data <- read_excel("/Users/vinithavudhayagiri/Downloads/Health.xlsx")

dim(health_data)
health_data
data<-na.omit(health_data)
dim(health_data)
hist(health_data$X1)
names(health_data)

par(mfrow = c(1, 1))
boxplot(`X2`~`X1`, data = health_data)

par(mfrow = c(1, 1))
boxplot(`X3`~`X1`, data = health_data)

par(mfrow = c(1, 1))
boxplot(`X4`~`X1`, data = health_data)

par(mfrow = c(1, 1))
boxplot(`X5`~`X1`, data = health_data)

train <- sample(c(TRUE, FALSE), nrow(health_data), replace = TRUE, prob=c(0.80, 0.20))
test <- (!train)
dim(health_data[train, ])

#Linear Regression
lm.fit <- lm(`X1` ~ ., data = health_data[train, ])
summary(lm.fit)

lm.fit.pred <- predict(lm.fit,health_data[test, ])
summary(lm.fit.pred)
mean((health_data$`X1`[test] - lm.fit.pred)^2)



# Polyfit regression

lm.poly.fit <- lm(`X1` ~ poly(`X2`, 2) + poly(`X3`, 2) +poly(`X4`, 2) + poly(`X5`, 2), data = health_data[train, ])

summary(lm.poly.fit)

# to find test error
lm.poly.fit.pred <- predict(lm.poly.fit,health_data[test, ])
mean((health_data$`X1`[test] - lm.poly.fit.pred)^2)

#Ridge regression

library (glmnet)

x.rr <- model.matrix(`X1` ~ ., health_data)[, -1]
y.rr <- health_data$`X1`
cv.out.rr <- cv.glmnet(x.rr[train, ], y.rr[train], alpha = 0)
best.lambda <- cv.out.rr$lambda.min
ridge.reg <- glmnet(x.rr[train, ], y.rr[train], alpha = 0, lambda = best.lambda)
ridge.pred <- predict(ridge.reg, s = best.lambda, newx = x.rr[test, ])
mean((ridge.pred - y.rr[test])^2)

#Lasso regression

cv.out.l <- cv.glmnet(x.rr[train, ], y.rr[train], alpha = 1)
best.lambda.l <- cv.out.l$lambda.min
lasso.mod <- glmnet(x.rr[train, ], y.rr[train], alpha = 1, lambda = best.lambda.l)
lasso.pred <- predict(lasso.mod, s = best.lambda.l, newx = x.rr[test, ])
mean((lasso.pred - y.rr[test])^2)



#PCR 
library(pls)
pcr.fit<-pcr(`X1`~ ., data = health_data[train, ]
             ,scale=TRUE,validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type = "MSEP")
pcr.pred<-predict(pcr.fit,health_data[test, ],ncomp = 3)
mean((health_data$`X1`[test]-pcr.pred)^2)

#PLS
pls.fit<-plsr(`X1`~ ., data = health_data[train, ],subset=train
              ,scale=TRUE,validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type = "MSEP")
pls.pred<-predict(pls.fit,health_data[test, ],ncomp = 1)
mean((health_data$`X1`[test]-pls.pred)^2)

#LOOCV
glm.fit <- glm (`X1` ~  `X2`+`X3`+`X4`+`X5`, data = health_data)
coef (glm.fit)

lm.fit <- lm (`X1` ~  `X2`+`X3`+`X4`+`X5`, data = health_data)
coef (lm.fit)
library (boot)
glm.fit <- glm (`X1` ~  `X2`+`X3`+`X4`+`X5`, data = health_data)
glm.fit
cv.err <- cv.glm (health_data , glm.fit)
cv.err$delta


cv.error <- rep(0, 10) 
for (i in 1:10) {
  glm.fit <- glm(`X1` ~ poly(`X2`, i)+poly(`X3`, i)+poly(`X4`, i)+poly(`X5`, i), data = health_data)
  cv.error[i] <- cv.glm(health_data, glm.fit)$delta[1]
}
cv.error

#K-Fold K=5
set.seed (17)

cv.error.5 <- rep (0, 5)

for (i in 1:5) {
  glm.fit <- glm (`X1` ~ poly(`X2`, i) + poly(`X3`, i) +poly(`X4`, i) + poly(`X5`, i) , data = health_data)
  cv.error.5[i] <- cv.glm (health_data , glm.fit , K = 5)$delta[1]
}
cv.error.5

#K-Fold K=10
set.seed (17)
cv.error.10 <- rep (0, 10)

for (i in 1:10) {
  glm.fit <- glm (`X1` ~ poly(`X2`, i) + poly(`X3`, i) +poly(`X4`, i) + poly(`X5`, i) , data = health_data)
  cv.error.10[i] <- cv.glm (health_data , glm.fit , K = 10)$delta[1]
}
cv.error.10
#2
library(readxl)
cycle_data <- read_excel("/Users/vinithavudhayagiri/Downloads/CCPP/Folds5x2_pp.xlsx")
dim(cycle_data)
cycle_data
data<-na.omit(cycle_data)
dim(cycle_data)
hist(cycle_data$PE)
names(cycle_data)

par(mfrow = c(1, 1))
boxplot(`AT`~`PE`, data = cycle_data)

par(mfrow = c(1, 1))
boxplot(`V`~`PE`, data = cycle_data)

par(mfrow = c(1, 1))
boxplot(`AP`~`PE`, data = cycle_data)

par(mfrow = c(1, 1))
boxplot(`RH`~`PE`, data = cycle_data)

train <- sample(c(TRUE, FALSE), nrow(cycle_data), replace = TRUE, prob=c(0.80, 0.20))
test <- (!train)
dim(cycle_data[train, ])

#Linear Regression
lm.fit <- lm(`PE` ~ ., data = cycle_data[train, ])
summary(lm.fit)

lm.fit.pred <- predict(lm.fit,cycle_data[test, ])
summary(lm.fit.pred)
mean((cycle_data$`PE`[test] - lm.fit.pred)^2)

# Polyfit regression

lm.poly.fit <- lm(`PE` ~ poly(`AT`, 2) + poly(`V`, 2) +poly(`AP`, 2) + poly(`RH`, 2), data = cycle_data[train, ])

summary(lm.poly.fit)

# to find test error
lm.poly.fit.pred <- predict(lm.poly.fit,cycle_data[test, ])
mean((cycle_data$`PE`[test] - lm.poly.fit.pred)^2)

#Ridge regression

library (glmnet)

x.rr <- model.matrix(`PE` ~ ., cycle_data)[, -1]
y.rr <- cycle_data$`PE`
cv.out.rr <- cv.glmnet(x.rr[train, ], y.rr[train], alpha = 0)
best.lambda <- cv.out.rr$lambda.min
ridge.reg <- glmnet(x.rr[train, ], y.rr[train], alpha = 0, lambda = best.lambda)
ridge.pred <- predict(ridge.reg, s = best.lambda, newx = x.rr[test, ])
mean((ridge.pred - y.rr[test])^2)

#Lasso regression

cv.out.l <- cv.glmnet(x.rr[train, ], y.rr[train], alpha = 1)
best.lambda.l <- cv.out.l$lambda.min
lasso.mod <- glmnet(x.rr[train, ], y.rr[train], alpha = 1, lambda = best.lambda.l)
lasso.pred <- predict(lasso.mod, s = best.lambda.l, newx = x.rr[test, ])
mean((lasso.pred - y.rr[test])^2)

#PCR 
library(pls)
pcr.fit<-pcr(`PE`~ ., data = cycle_data[train, ]
             ,scale=TRUE,validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type = "MSEP")
pcr.pred<-predict(pcr.fit,cycle_data[test, ],ncomp = 4)
mean((cycle_data$`PE`[test]-pcr.pred)^2)

#PLS
pls.fit<-plsr(`PE`~ ., data = cycle_data[train, ],subset=train
          ,scale=TRUE,validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type = "MSEP")
pls.pred<-predict(pls.fit,cycle_data[test, ],ncomp = 4)
mean((cycle_data$`PE`[test]-pls.pred)^2)

#LOOCV

set.seed(1)
glm.fit <- glm (`PE` ~ `AT` + `V` + `AP` + `RH`, data = cycle_data)
coef (glm.fit)

lm.fit <- lm (`PE` ~ `AT` + `V` + `AP` + `RH`, data = cycle_data)
coef (lm.fit)

library (boot)
glm.fit <- glm (`PE` ~ `AT` + `V` + `AP` + `RH`, data = cycle_data)
cv.err <- cv.glm (cycle_data , glm.fit)
cv.err$delta

#K-Fold K=5
set.seed (17)
cv.error.5 <- rep (0, 5)

for (i in 1:5) {
  glm.fit <- glm (`PE` ~ poly(`AT`, i) + poly(`V`, i) +poly(`AP`, i) + poly(`RH`, i) , data = cycle_data)
  cv.error.5[i] <- cv.glm (cycle_data , glm.fit , K = 5)$delta[1]
}
cv.error.5

#K-Fold K=10
set.seed (17)
cv.error.10 <- rep (0, 10)

for (i in 1:10) {
  glm.fit <- glm (`PE` ~ poly(`AT`, i) + poly(`V`, i) +poly(`AP`, i) + poly(`RH`, i) , data = cycle_data)
  cv.error.10[i] <- cv.glm (cycle_data , glm.fit , K = 10)$delta[1]
}
cv.error.10


