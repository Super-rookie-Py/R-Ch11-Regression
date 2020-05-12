###  Ch11.회귀분석

###  2020/05/12 keonwoo park

## 03.다중선형회귀분석

# 예제: 온라인게임의 몰입에 영향을 주는 연구를 하고자한다.
# 문헌연구를 통해 도구, 보상, 정보, 디자인, 공동체 
# 등 5가지 요인중 가장 영향을 주는 요인은 
# 무엇인가.


# 01.데이터 불러오기
mreg <- read.csv('Ch1103.다중회귀분석(MREG).csv',
                   header=T,
                   na.strings='.')
str(mreg)


# 02. 기본통계치 확인
library(psych)
describe(mreg)
pairs.panels(mreg)


# 03.다중회귀분석
# 전체변수 일괄입력
# backword: 변수제거   (변수가 많을 때 사용)
# forward: 변수추가    (변수가 적을 때 사용)
# stepwise: backward 와 forward 동시
# AIC( Akaike information creitertion), BIC ( Bayesian..)

library(car)
mreg.model <- lm(flow ~ design+info+comm+op+fb, #변수가 많을 때는 . 찍기
                 data=mreg)
anova(mreg.model)
summary(mreg.model)
vif(mreg.model) # 독립변수들 간에 비슷한 변수가 있는가?
# 10보다 크면 다중공선성문제 있음


# 표준화 회귀계수
install.packages('lm.beta')
library('lm.beta')
lm.beta <- lm.beta(mreg.model)
summary(lm.beta)
# standaradized가 가장 높은값이 가장 큰 영향을 준다.


# backward: 변수제거
# 모든 변수 투입확인
mreg.model.b <- lm(flow ~ ., mreg) # ~. 모든값 
summary(mreg.model.b)

# AIC값이 줄어들어야 의미가 있따.
mreg.model.b <- step(mreg.model.b,
                     direction = 'backward',
                     trace = T) # trace setp 별로 모두 보여주기 


# forward: 변수추가
# 모든 변수 투입확인
mreg.model.f <- lm(flow ~1, mreg) # ~. 모든값 
summary(mreg.model.f)

# AIC값이 줄어들어야 의미가 있따.
mreg.model.f <- step(mreg.model.f,
                     direction = 'forward',
                     scope = (flow ~ design+info+comm+op+fb),
                     trace=T)# trace setp 별로 모두 보여주기 


