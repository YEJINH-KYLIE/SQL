
# 0804 의사결정나무  vs 로지스틱 회기 분석 성능 비교 
library(tidyverse)

getwd()
setwd(dir = './data')
list.files(pattern = 'RDA')
load(file = 'Bank_Dataset.RDA')

fitL <- glm(formula = PersonalLoan ~ ., 
            data = trainSet, 
            family = binomial(link = 'logit'))

summary(object = fitL)

pchisq(q = fitL$null.deviance - fitL$deviance, 
       df = fitL$df.null - fitL$df.residual, 
       lower.tail = FALSE)

trainSet <- trainSet %>% select(-Age, -Experience, -Mortgage)

fitL <- glm(formula = PersonalLoan ~ ., 
            data = trainSet, 
            family = binomial(link = 'logit'))

summary(object = fitL)

pchisq(q = fitL$null.deviance - fitL$deviance, 
       df = fitL$df.null - fitL$df.residual, 
       lower.tail = FALSE)

fitL$coefficients %>% exp() %>% round(digits = 2L)

library(reghelper)

beta <- beta(model = fitL)
beta$coefficients[, 1] %>% abs() %>% round(digits = 2L) %>% sort()

real <- testSet$PersonalLoan
probL <- predict(object = fitL, newdata = testSet, type = 'response')

boxplot(formula = probL ~ real)

predL <- ifelse(test = probL >= 0.5, yes = '1', no = '0') %>% 
  as.factor()

library(caret)
confusionMatrix(data = predL, reference = real, positive = '1')

library(MLmetrics)
F1_Score(y_true = real, y_pred = predL, positive = '1')

library(pROC)
roc(response = real, predictor = probL) %>% 
  plot(main = 'ROC 곡선', col = 'blue', lwd = 2)

auc(response = real, predictor = probL)


list.files(pattern = 'RDS')
fitD <- readRDS(file = 'DecisionTree.RDS')

predD <- predict(object = fitD, newdata = testSet, type = 'class')
probD <- predict(object = fitD, newdata = testSet, type = 'prob')
probD <- probD[, 2]

roc(response = real, predictor = probD) %>% 
  plot(add = TRUE, col = 'red', lwd = 2)

auc(response = real, predictor = probD)

confusionMatrix(data = predD, reference = real, positive = '1')
F1_Score(y_true = real, y_pred = predD, positive = '1')



# 회귀나무
house <- read.csv(file = 'https://bit.ly/median_house_value')
str(object = house)
summary(object = house)
hist(x = house$MedianHouseValue)
house <- house %>% filter(MedianHouseValue <= 500000)

set.seed(seed = 1234)
n <- nrow(x = house)
index <- sample(x = n, size = n*0.3, replace = FALSE)
trainSet <- house %>% slice(index)
testSet <- house %>% slice(-index)

trainSet$MedianHouseValue %>% mean()
testSet$MedianHouseValue %>% mean()

getwd()
save(list = c('trainSet', 'testSet', 'house'), 
     file = 'House_Dataset.RDA')

library(rpart)
set.seed(seed = 1234)
fitR <- rpart(formula = MedianHouseValue ~ .,
              data = trainSet, 
              control = rpart.control(minsplit = 20,
                                      cp = 0.01, 
                                      maxdepth = 10))

summary(object = fitR)

library(rpart.plot)
rpart.plot(x = fitR, type = 2, extra = 101, fallen.leaves = FALSE)

printcp(x = fitR)
plotcp(x = fitR)

set.seed(seed = 1234)
fitR <- rpart(formula = MedianHouseValue ~ .,
              data = trainSet, 
              control = rpart.control(minsplit = 10,
                                      cp = 0.001, 
                                      maxdepth = 30))

printcp(x = fitR)
plotcp(x = fitR)

fitP <- prune.rpart(tree = fitR, cp = 0.0010178)
rpart.plot(x = fitP, type = 2, extra = 101, fallen.leaves = FALSE)

real <- testSet$MedianHouseValue
predR <- predict(object = fitR, newdata = testSet, type = 'vector')
predP <- predict(object = fitP, newdata = testSet, type = 'vector')

# 회기계수 확인
errorR <- real - predR
errorR^2 %>% mean()
errorR^2 %>% mean() %>% sqrt()
errorR %>% abs() %>% mean()
(errorR/real) %>% abs() %>% mean()

errorP <- real - predP
errorP^2 %>% mean()
errorP^2 %>% mean() %>% sqrt()
errorP %>% abs() %>% mean()
(errorP/real) %>% abs() %>% mean()

full <- lm(formula = MedianHouseValue ~ ., data = trainSet)
fitL <- step(object = full, direction = 'both')

null <- lm(formula = MedianHouseValue ~ 1, data = trainSet)
fitZ <- step(object = null, 
             scope = list(lower = null, upper = full),
             direction = 'both')

summary(object = fitL)

par(mfrow = c(2, 2))
plot(x = fitL)

shapiro.test(x = fitL$residuals)

# install.packages('nortest')
library(nortest)
ad.test(x = fitL$residuals)

#선형회기 분석툴 성능 비교
library(car)
ncvTest(model = fitL)
durbinWatsonTest(model = fitL)
crPlots(model = fitL)
influencePlot(model = fitL)

predL <- predict(object = fitL, newdata = testSet, type = 'response')

# 회기계수 확인 
errorL <- real - predL
errorL^2 %>% mean()
errorL^2 %>% mean() %>% sqrt()
errorL %>% abs() %>% mean()
(errorL/real) %>% abs() %>% mean()

saveRDS(object = fitR, file = 'RegressionTree.RDS')



install.packages('tree')
library(tree)

fitT <- tree(formula = MedianHouseValue ~ Longitude + Latitude,
             data = house)

seq(from = 0, to = 1, by = 0.1)
seq(from = 0, to = 1, length = 11)

decile <- quantile(x = house$MedianHouseValue, 
                   probs = seq(from = 0, to = 1, length = 11))

decile

# grade <- cut(x = house$MedianHouseValue,
#              breaks = decile)
# 
# grade

grade <- cut(x = house$MedianHouseValue,
             breaks = decile, 
             include.lowest = TRUE,
             right = TRUE)

grade
table(grade, useNA = 'ifany')
table(grade, useNA = 'always')

# 지도형태의 산점도 그리기 
cols <- grey(level = seq(from = 1, to = 0, by = -0.1))[grade]
plot(x = house$Longitude, 
     y = house$Latitude,
     pch = 20, 
     col = cols, 
     xlab = '경도',
     ylab = '위도')

# 회기계수 나무 그리기 
partition.tree(tree = fitT, 
               ordvars = c('Longitude', 'Latitude'),
               col = 'red', 
               cex = 0.8, 
               font = 2, 
               add = TRUE)



# 도요타 데이터 해석
list.files(pattern = 'RDS')
df <- readRDS(file = 'Toyota.RDS')

set.seed(seed = 1234)
n <- nrow(x = df)
index <- sample(x = n, size = n*0.7, replace = FALSE)
trainSet <- df %>% slice(index)
testSet <- df %>% slice(-index)

set.seed(seed = 1234)
fitR <- rpart(formula = Price ~ ., 
              data = trainSet, 
              control = rpart.control(minsplit = 10,
                                      cp = 0.001, 
                                      maxdepth = 30))

printcp(x = fitR)
which.min(x = fitR$cptable[, 4])
plotcp(x = fitR)

rpart.plot(x = fitR, type = 2, extra = 101, fallen.leaves = FALSE)

real <- testSet$Price
predR <- predict(object = fitR, newdata = testSet, type = 'vector')

regMeasure <- function(real, pred) {
  error <- abs(x = real - pred)
  result <- data.frame(mse = error^2 %>% mean(),
                       rmse = error^2 %>% mean() %>% sqrt(),
                       mae = error %>% abs() %>% mean(),
                       mape = (error/real) %>% abs() %>% mean())
  return(result)
}

regMeasure(real = real, pred = predR)


full <- lm(formula = Price ~ ., data = trainSet)
fitL <- step(object = full, direction = 'both')
summary(object = fitL)

par(mfrow = c(2, 2))
plot(x = fitL)
par(mfrow = c(1, 1))

shapiro.test(x = fitL$residuals)
ncvTest(model = fitL)
durbinWatsonTest(model = fitL)
crPlots(model = fitL)
influencePlot(model = fitL)

predL <- predict(object = fitL, newdata = testSet, type = 'response')

regMeasure(real = real, pred = predL)
regMeasure(real = real, pred = predR)


