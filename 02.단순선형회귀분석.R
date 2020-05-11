###  Ch11.회귀분석

###  2020/05/11 keonwoo park

## 02.단순선형회귀분석(인과 관계를 검정하는 분석방법)
# 원인(독립변수)가 다른 변수(종속변수)에 영향을 미칠 때.

# 직원 100명의 건강검진자료 중 콜레스테롤이 높으면 중성지방도 높다고 말할 수 있는가 ?? 

# 1. 데이터 불러오기
reg.df <- read.csv("ch1102.단순 선형회귀분석(REG).csv",
                   header = T,
                   na.strings = '.')

# 02.기술통계분석
reg.df <-reg.df[c(-61:-62),] #61, 62 번째 이상치값 제외 
plot(fat~col, data=reg.df)
abline(lm(fat~col, data=reg.df),
       col='red',
       lty = 4)

# 03.단순회귀분석
reg.model <- lm(fat~col,
                data=reg.df)
anova(reg.model)
summary(reg.model)

opar <-par(no.readonly =T)
  par(mfrow=c(2,2))
  plot(reg.model)  
par(opar)  

# 수치로 가정검정
library(car)

# 잔차의 등분산성 검정
car::ncvTest(reg.model)

# 잔차의 정규분포 검정
shapiro.test(reg.model$residuals)

# 이상치 검정, sd, hat, d 통합검정
influencePlot(reg.model, id.method='identify')



# 04. 모델을 이용한 예측: 콜레스테롤이 130, 150일 때,
reg.new <-data.frame(col=c(130,150))
predict(reg.model, newdata = reg.new)
