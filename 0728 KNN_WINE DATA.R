
library(tidyverse)

wine <- read.csv(file = 'https://bit.ly/white_wine_quality', sep = ';')
glimpse(x = wine)

wine$quality %>% 
  table() %>% 
  prop.table() %>% 
  cumsum() %>% 
  round(digits = 4L) * 100

tbl <- table(wine$quality)

bp <- barplot(height = tbl, 
              ylim = c(0, 2400),
              # col = 'gray70', 
              col = c(rep(x = 'gray70', times = 4), 
                      rep(x = 'red', times = 3)),
              xlab = 'Quality Score',
              main = 'White Wine Quality')

text(x = bp,
     y = tbl, 
     labels = tbl, 
     pos = 3, 
     font = 2)


install.packages('RColorBrewer')
library(RColorBrewer)
display.brewer.all()
display.brewer.pal(n = 11, name = 'Spectral')
myPal1 <- brewer.pal(n = 7, name = 'Spectral')
myPal1

myPal2 <- gray(level = seq(from = 0.2, to = 0.8, length = 7))
myPal3 <- colorRampPalette(colors = c('red', 'yellow', 'purple'))(7)

bp <- barplot(height = tbl, 
              ylim = c(0, 2400),
              col = myPal3,
              xlab = 'Quality Score',
              main = 'White Wine Quality')


wine$grade <- ifelse(test = wine$quality <= 6, 
                     yes = 'good', 
                     no = 'best')

wine$grade <- as.factor(x = wine$grade)
table(wine$grade, wine$quality)

wine$quality <- NULL


boxplot(formula = alcohol ~ grade, data = wine)

set.seed(seed = 1234)
n <- nrow(x = wine)
index <- sample(x = n, size = n * 0.7, replace = FALSE)
trainSet <- wine %>% slice(index)
testSet <- wine %>% slice(-index)

table(trainSet$grade) %>% prop.table()
table(testSet$grade) %>% prop.table()

# install.packages('kknn')
library(kknn)

k <- trainSet %>% nrow() %>% sqrt() %>% ceiling()
print(x = k)

fitN <- kknn(formula = grade ~ .,
             train = trainSet, 
             test = testSet, 
             k = k, 
             kernel = 'rectangular')

str(object = fitN)

real <- testSet$grade
predN <- fitN$fitted.values

# install.packages('caret')
# install.packages('e1071')
library(caret)
confusionMatrix(data = predN, reference = real, positive = 'best')

# install.packages('MLmetrics')
library(MLmetrics)
F1_Score(y_true = real, y_pred = predN, positive = 'best')

library(tidyverse)
library(pROC)
probN <- fitN$prob[, 1]
roc(response = real, predictor = probN) %>% plot()
auc(response = real, predictor = probN)



install.packages('DMwR')
library(DMwR)
trainBal <- SMOTE(form = grade ~ .,
                  data = trainSet, 
                  perc.over = 200, 
                  k = 10, 
                  perc.under = 150)

table(trainSet$grade) %>% prop.table()
table(trainBal$grade) %>% prop.table()

levels(x = trainSet$grade)
levels(x = trainBal$grade)

fitB <- kknn(formula = grade ~ .,
             train = trainBal, 
             test = testSet, 
             k = k, 
             kernel = 'rectangular')

predB <- fitB$fitted.values

confusionMatrix(data = predB, reference = real, positive = 'best')
F1_Score(y_true = real, y_pred = predB, positive = 'best')

probB <- fitB$prob[, 1]
roc(response = real, predictor = probB) %>% plot()


roc(response = real, predictor = probN) %>% 
  plot(main = 'ROC 곡선', col = 'red')

roc(response = real, predictor = probB) %>% 
  plot(col = 'blue', add = TRUE)

auc(response = real, predictor = probN)
auc(response = real, predictor = probB)



fitW <- kknn(formula = grade ~ .,
             train = trainSet, 
             test = testSet, 
             k = k, 
             kernel = 'triangular')

predW <- fitW$fitted.values

confusionMatrix(data = predW, reference = real, positive = 'best')
F1_Score(y_true = real, y_pred = predW, positive = 'best')

probW <- fitW$prob[, 1]
roc(response = real, predictor = probW) %>% 
  plot(col = 'black', lwd = 2, add = TRUE)

auc(response = real, predictor = probW)


fitWB <- kknn(formula = grade ~ .,
              train = trainBal, 
              test = testSet, 
              k = k, 
              kernel = 'triangular')

predWB <- fitWB$fitted.values

confusionMatrix(data = predWB, reference = real, positive = 'best')
F1_Score(y_true = real, y_pred = predWB, positive = 'best')

probWB <- fitWB$prob[, 1]
roc(response = real, predictor = probWB) %>% 
  plot(col = 'orange', lwd = 3, lty = 2, add = TRUE)

auc(response = real, predictor = probWB)





# 최적의 k를 찾기 위한 교차검증
kvec <- seq(from = 3, to = 159, by = 2)
print(x = kvec)

library(kknn)
library(MLmetrics)

result <- c()

for (k in kvec) {
  
  cat('현재 ', k, '로 작업 중!\n', sep = '')
  
  fit <- kknn(formula = grade ~ .,
              train = trainSet, 
              test = testSet, 
              k = k, 
              kernel = 'triangular')
  
  real <- testSet$grade
  pred <- fit$fitted.values
  
  f1 <- F1_Score(y_true = real, y_pred = pred, positive = 'best')
  result <- c(result, f1)
  
}

print(x = result)

plot(x = kvec, y = result, type = 'b', col = 'red')

max(result)
which(x = result == max(result))

abline(h = max(result), col = 'black', lty = 3)
abline(v = 7, col = 'black', lty = 3)
text(x = 7, y = 0.5, labels = str_c('k는', kvec[3]), font = 2, col = 'red')



fitW <- kknn(formula = grade ~ .,
             train = trainSet, 
             test = testSet, 
             k = 7, 
             kernel = 'triangular')

predW <- fitW$fitted.values

confusionMatrix(data = predW, reference = real, positive = 'best')
F1_Score(y_true = real, y_pred = predW, positive = 'best')

probW <- fitW$prob[, 1]

roc(response = real, predictor = probN) %>% 
  plot(main = 'ROC 곡선', col = 'red')

roc(response = real, predictor = probW) %>% 
  plot(col = 'black', lwd = 2, add = TRUE)

auc(response = real, predictor = probW)
