
library(tidyverse)
univ <- read.csv(file = 'https://bit.ly/university_admit')
glimpse(x = univ)
summary(object = univ)

# univ$admit <- as.factor(x = univ$admit)
# 'admit', 'rank'는 범주형으로 바꾸어준다. Map 함수 활용
univ[, c(1, 4)] <- map_df(.x = univ[, c(1, 4)], .f = as.factor)
summary(object = univ)
table(univ$admit) %>% prop.table()

boxplot(formula = gre ~ admit, data = univ)
abline(h = mean(x = univ$gre), col = 'red', lty = 2)

boxplot(formula = gpa ~ admit, data = univ)
abline(h = mean(x = univ$gpa), col = 'red', lty = 2)

table(univ$admit, univ$rank) %>% prop.table(margin = 2)

# 샘플링해서 훈련셋, 테스트셋 만들어주기 
set.seed(seed = 1234)
n <- nrow(x = univ)
index <- sample(x = n, size = n*0.7, replace = FALSE)
trainSet <- univ %>% slice(index)
testSet <- univ %>% slice(-index)

table(trainSet$admit) %>% prop.table()
table(testSet$admit) %>% prop.table()

# 이항 로지스틱 회귀모형 적용 
fitC <- glm(formula = admit ~ .,
            data = trainSet, 
            family = binomial(link = 'logit'))

summary(object = fitC)

#서머리를 통해 더미 변수 확인 & 이탈도 확인 


pchisq(q = fitC$null.deviance - fitC$deviance,
       df = fitC$df.null - fitC$df.residual,
       lower.tail = FALSE)

# 각 변수 별 스코어들을 확인 
result <- summary(object = fitC)
result$coefficients
class(x = result$coefficients)
coefs <- result$coefficients
colnames(x = coefs) <- c('coef', 'se', 'z-stat', 'p-value')
coefs

# 입력변수의 오즈비 출력 : 음수값 없애기 위한 로그값 씌워짐
fitC$coefficients %>% exp()

# 표준화 회기 계수 출력 
library(reghelper)
beta(model = fitC)

# cut-off 를 임으로 0.5 로 입력하고, 목표변수의 추정확률 생성 과정
# 시험셋으로 목표변수의 추정확률 생성
probC <- predict(object = fitC, newdata = testSet, type = 'response')
probC

# 분리 기준점(cut-off)을 0.5로 설정하여 라벨링하고 범주형 벡터로 변환
predC <- ifelse(test = probC >= 0.5, yes = '1', no = '0')
predC <- as.factor(x = predC)
predC
# 시험셋의 실제값을 'real'에 할당 
real <- testSet$admit
real

## 분류모형의 성능평가 
# 혼동 행렬 추출하여 정확도, 민감도, 정밀도, 특이도 확인 
library(caret)
confusionMatrix(data = predC, reference = real, positive = '1')

# F1 점수  출력
library(MLmetrics)
F1_Score(y_true = real, y_pred = predC, positive = '1')

# ROC 그래프를 그려서 AUC 값 확인 
library(pROC)
roc(response = real, predictor = probC) %>% 
  plot(main = 'ROC 곡선')

auc(response = real, predictor = probC)

# MCC 계수 (컷오프 값을 검정하는 매튜의 상관계수), -1~1 사이의 값이고 높을수록 완벽!
library(mccr)
mccr(act = real, pred = predC)

# MCC가 최대가 되는 cut-off 찾아라!
cuts <- seq(from = 0.01, to = 1.00, by = 0.01)

mccs <- c()
for (cut in cuts) {
  pred <- ifelse(test = probC >= cut, yes = '1', no = '0')
  pred <- factor(x = pred, levels = c(0, 1))
  mcc <- mccr(act = real, pred = pred)
  mccs <- c(mccs, mcc)
}
mccs

plot(x = cuts, y = mccs, type = 'l')
abline(h = max(mccs), col = 'red', lty = 3, lwd = 2)
locs <- which(x = mccs == max(mccs))
which.max(x = mccs)
cuts[locs]

predC <- ifelse(test = probC >= 0.41, yes = '1', no = '0')
predC <- as.factor(x = predC)
predC

confusionMatrix(data = predC, reference = real, positive = '1')
F1_Score(y_true = real, y_pred = predC, positive = '1')

roc(response = real, predictor = probC) %>% 
  plot(main = 'ROC 곡선')

auc(response = real, predictor = probC)

# 로지스틱 회귀 모형의 추정확률을 박스플랏으로 비교, 컷오프 0.5 도 넣어줌
# 컷오프가 너무 높게 설정되어 실제 합격한 사람을 불합격화하는 모습을 볼수 있다 
boxplot(formula = probC ~ real)
abline(h = 0.5, col = 'red')

# 목표변수의 실제값 비중을 확인해보고,  
rate <- table(real) %>% prop.table()
rate
rate[2]

# 위를 컷오프로 입력해서 새로운 분리 기준점의 성능평가를 실시한다
# 성능 지표 향상 수준 확인해보기 !
predC <- ifelse(test = probC >= rate[2], yes = '1', no = '0')
predC <- as.factor(x = predC)
predC

confusionMatrix(data = predC, reference = real, positive = '1')
F1_Score(y_true = real, y_pred = predC, positive = '1')

roc(response = real, predictor = probC) %>% 
  plot(main = 'ROC 곡선')

auc(response = real, predictor = probC)

# 박스플랏 다시그려 확인 ! 
boxplot(formula = probC ~ real)
abline(h = rate[2], col = 'red')

## 중요한점 : 샘플링을 다시하면 적절한 컷오프는 바뀌게 된다. 따라서 여러번 샘플링을 한 결과의 평균으로 최적의 컷오프를 찾도록 한다. 