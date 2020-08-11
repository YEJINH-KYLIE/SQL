
library(tidyverse)
getwd()
setwd(dir = './data')
list.files(pattern = 'RDA')
load(file = 'Bank_Dataset.RDA')

# install.packages('randomForest')
library(randomForest)

# Xtrain <- trainSet[, -8]
# ytrain <- trainSet[, 8]
# 
# Xtest <- testSet[, -8]
# ytest <- testSet[, 8]

set.seed(seed = 1234)
fitC <- randomForest(x = trainSet[, -8],
                     y = trainSet[, 8],
                     xtest = testSet[, -8],
                     ytest = testSet[, 8],
                     ntree = 1000,
                     mtry = 3,
                     importance = TRUE,
                     do.trace = TRUE,
                     keep.forest = TRUE)

fitC$err.rate
plot(x = fitC$err.rate[, 1], type = 'l')
plot(x = fitC, lwd = 2)

importance(x = fitC)
varImpPlot(x = fitC)

fitC %>% 
  treesize(terminal = TRUE) %>% 
  hist(main = 'The Number of ternimal nodes')

# fitC$test$predicted

pred1 <- predict(object = fitC, newdata = testSet, type = 'response')
real <- testSet$PersonalLoan

library(caret)
confusionMatrix(data = pred1, reference = real, positive = '1')

library(MLmetrics)
F1_Score(y_true = real, y_pred = pred1, positive = '1')

prob1 <- predict(object = fitC, newdata = testSet, type = 'vote')
print(x = prob1)

prob1 <- prob1[, 2]

library(pROC)
roc(response = real, predictor = prob1) %>% 
  plot(main = 'ROC 곡선', col = 'red', lwd = 2)

auc(response = real, predictor = prob1)


list.files(pattern = 'RDS')
fitDT <- readRDS(file = 'DecisionTree.RDS')

pred0 <- predict(object = fitDT, newdata = testSet, type = 'class')
pred0


confusionMatrix(data = pred0, reference = real, positive = '1')
F1_Score(y_true = real, y_pred = pred0, positive = '1')

prob0 <- predict(object = fitDT, newdata = testSet, type = 'prob')
prob0 <- prob0[, 2]

roc(response = real, predictor = prob0) %>% 
  plot(add = TRUE, col = 'blue', lwd = 2, lty = 2)

auc(response = real, predictor = prob0)


boxplot(formula = Income ~ PersonalLoan, data = bank)
plot(formula = Income ~ PersonalLoan, data = bank)
plot(formula = Family ~ PersonalLoan, data = bank)

library(gmodels)
CrossTable(x = bank$PersonalLoan, y = bank$Education)


grid <- expand.grid(ntree = c(300, 500, 700, 1000),
                    mtry = c(3, 4, 5, 6, 7))
print(x = grid)

grid$error <- NA
i <- 1

for (i in 1:nrow(x = grid)) {
  set.seed(seed = 1234)
  # cat(i, '행 실행 중 [ntree:', grid[i, 'ntree'], ', mtry:', grid[i, 'mtry'], ']\n\n')
  cat(str_glue('{i} 행 실행 중 [ntree: {grid$ntree[i]}, mtry: {grid$mtry[i]}]'), '\n\n')
  
  fit <- randomForest(x = trainSet[, -8],
                      y = trainSet[, 8],
                      ntree = grid$ntree[i],
                      mtry = grid$mtry[i],
                      do.trace = 50)
  
  grid$error[i] <- fit$err.rate[, 1] %>% tail(n = 1)
}

plot(x = grid$error,
     type = 'b',
     main = 'Grid Search Result')

abline(h = min(grid$error), 
       col = 'red', 
       lty = 2)

loc <- which.min(x = grid$error)
print(x = loc)

bestPara <- grid[loc, ]
print(x = bestPara)

set.seed(seed = 1234)
bestC <- randomForest(x = trainSet[, -8], 
                      y = trainSet[, 8], 
                      ntree = bestPara$ntree,
                      mtry = bestPara$mtry,
                      do.trace = 50)

predB <- predict(object = bestC, newdata = testSet, type = 'response')
predB

confusionMatrix(data = predB, reference = real, positive = '1')
F1_Score(y_true = real, y_pred = predB, positive = '1')

probB <- predict(object = bestC, newdata = testSet, type = 'vote')
probB <- probB[, 2]

roc(response = real, predictor = probB) %>% 
  plot(add = TRUE, col = 'black', lwd = 3, lty = 2)

auc(response = real, predictor = probB)


saveRDS(object = bestC, file = 'RFC.RDS')

