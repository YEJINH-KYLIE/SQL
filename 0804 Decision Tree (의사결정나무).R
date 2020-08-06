
# 데이터 불러오기 
library(tidyverse)
bank <- read.csv(file = 'https://bit.ly/universal_bank')
str(object = bank)
summary(object = bank)

# 데이터 전처리 : 불필요한 칼럼, 이상치 행 삭제
bank <- bank %>% select(-ID, -ZIP.Code) %>% filter(Experience >= 0)

# 명목형 칼럼은 범주형으로 변환 
bank[, c(6, 8:12)] <- map_df(.x = bank[, c(6, 8:12)], .f = as.factor)
summary(object = bank)

# 목표변수의 빈도수 파악 
bank$PersonalLoan %>% table() %>% prop.table()

# 데이터셋 분할 
set.seed(seed = 1234)
n <- nrow(x = bank)
index <- sample(x = n, size = n*0.7, replace = FALSE)

trainSet <- bank %>% slice(index)
testSet <- bank %>% slice(-index)

trainSet$PersonalLoan %>% table() %>% prop.table()
testSet$PersonalLoan %>% table() %>% prop.table()

# RDA 파일로 저장 
getwd()
setwd(dir = './data')

save(list = c('bank', 'trainSet', 'testSet'), 
     file = 'Bank_Dataset.RDA')

# 의사결정나무 분류모형 함수 불러오기 & 적합
# CP : 교차검증을 자동시행해주는데, 미리 set seed  를 해줘야 교차검증 시 갖은 값을 가진다 
library(rpart)
set.seed(seed = 1234)
fitC <- rpart(formula = PersonalLoan ~ ., 
              data = trainSet, 
              control = rpart.control(minsplit = 20,
                                      cp = 0.01, 
                                      maxdepth = 10))

# 분류모형 서머리로 보기
# xerror 가장 작은지 확인
summary(object = fitC)

# 분류모형 나무모형 시각화
install.packages('rpart.plot')
library(rpart.plot)
rpart.plot(x = fitC, type = 2, extra = 101, fallen.leaves = FALSE)
rpart.plot(x = fitC, type = 2, extra = 101, fallen.leaves = TRUE)

# 가지치기를 하기위해 과적합 시작되는 CP 찾기 
printcp(x = fitC)
plotcp(x = fitC)

# 성능 좋은 파라미터 설정 
set.seed(seed = 1234)
fitC <- rpart(formula = PersonalLoan ~ ., 
              data = trainSet, 
              control = rpart.control(minsplit = 10,
                                      cp = 0.001, 
                                      maxdepth = 30))

printcp(x = fitC)
plotcp(x = fitC)

# 과적합되는 타이밍의 CP 입력하고, 가지치기 
fitP <- prune.rpart(tree = fitC, cp = 0.0098)

# 나무모형시각화 : 가지치기 전 vs 후 비교 
rpart.plot(x = fitC, type = 2, extra = 101, fallen.leaves = FALSE)
rpart.plot(x = fitP, type = 2, extra = 101, fallen.leaves = FALSE)

# 실제값과 가지치기 전, 후 예측값 입력 
real <- testSet$PersonalLoan
predC <- predict(object = fitC, newdata = testSet, type = 'class')
predP <- predict(object = fitP, newdata = testSet, type = 'class')

print(x = real)
print(x = predC)
print(x = predP)

# 가지치기 전, 후 성능 비교 
library(caret)
confusionMatrix(data = predC, reference = real, positive = '1')

library(MLmetrics)
F1_Score(y_true = real, y_pred = predC, positive = '1')

confusionMatrix(data = predP, reference = real, positive = '1')
F1_Score(y_true = real, y_pred = predP, positive = '1')



probC <- predict(object = fitC, newdata = testSet, type = 'prob')
probC <- probC[, 2]

library(pROC)
roc(response = real, predictor = probC) %>% 
  plot(main = 'ROC 곡선')

probP <- predict(object = fitP, newdata = testSet, type = 'prob')
probP <- probP[, 2]

roc(response = real, predictor = probP) %>% 
  plot(col = 'red', lwd = 2, add = TRUE)


auc(response = real, predictor = probC)
auc(response = real, predictor = probP)


getwd()
saveRDS(object = fitP, file = 'DecisionTree.RDS')

