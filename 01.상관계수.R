###  Ch11.회귀분석

###  2020/05/11 keonwoo park

## 01.상관계수 (얼마만큼 밀접하게 연관이 있는가)

# 1데이터 불러오기
corr <- read.csv("ch1101.상관분석(CORR).csv",
                 header=T,
                 na.strings='.')
str(corr)

# 2.기본통계치 확인
library(psych)
describe(corr)
pairs.panels(corr)

plot(키 ~ 몸무게, data=corr)
abline(lm(키~몸무게,data=corr),
       col = 'red',
       lty=4)

# 3.상관분석
cor(corr, use='complete.obs',
    method=c('pearson'))

cor.test(corr$키,
         corr$몸무게,
         method = c('pearson'))
