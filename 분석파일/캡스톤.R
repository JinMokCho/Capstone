install.packages("readxl")

library(readxl)

data <- read_excel("C:/Users/삼성/OneDrive - 계명대학교/바탕 화면/캡스톤/불성실컷_239행_시중인뱅비율추가.xlsx")

# 고객 라이프스타일
lifestyle_columns <- c(
  '고객 라이프스타일 [취미나 관심사에 투자하는 시간이 많다.]',
  '고객 라이프스타일 투자 [자기계발 활동을 즐긴다. (워크샵, 강연 등)]',
  '고객 라이프스타일 투자 [투자와 재정관리에 관심을 가진다.]',
  '고객 라이프스타일 투자 [지적 호기심 충족을 위해 시간을 투자한다.]'
)

# 모바일뱅킹 가치관 적합성
value_fit_columns <- c(
  '모바일뱅킹 가치관 적합성 [모바일 금융 거래 시 보안성을 신뢰한다.]',
  '모바일뱅킹 가치관 적합성 [모바일 뱅킹앱  사용자 인터페이스(UI)에 만족한다.]',
  '모바일뱅킹 가치관 적합성 [주로 사용하는 모바일 뱅킹앱의 개인 맞춤화 서비스가 잘 구축되어 있다.]',
  '모바일뱅킹 가치관 적합성 [주로 사용하는 모바일 뱅킹앱의 다양성에 만족한다.(기능, 상품, 컨텐츠)]'
)

# 모바일뱅킹 능력 적합성
ability_fit_columns <- c(
  '모바일뱅킹 능력 적합성 [모바일 뱅킹앱의 사용법을 배우는 데 어려움이 없다.]',
  '모바일뱅킹 능력 적합성 [모바일 뱅킹앱을 통해  금융 업무를 하는 데 어려움이 없다.]',
  '모바일뱅킹 능력 적합성. [모바일뱅킹 앱을 통해 복잡한 금융 거래를 진행할 수 있다. (신규 계좌 개설, 투자 상품 가입 등)]',
  '모바일뱅킹 능력 적합성 [모바일뱅킹 앱이 재정 관리에 긍정적인 영향을 미친다.]'
)

# 모바일뱅킹 고객 만족도
satisfaction_columns <- c(
  '모바일뱅킹 고객 만족도[주로 사용하는 모바일 뱅킹앱에 만족한다.]',
  '모바일뱅킹 고객 만족도 [주로 사용하는 모바일 뱅킹앱을 지인에게 추천하고 싶다.]',
  '모바일뱅킹 고객 만족도 [주로 사용하는 모바일 뱅킹앱을 지속 사용할 것이다.]',
  '모바일뱅킹 고객 만족도 [주로 사용하는 모바일 뱅킹앱에서 만족스러운 기능과 서비스를 제공하고 있다.]'
)

# 각 요인별 대표 지표 생성 (각 열의 평균값)
data$'고객 라이프스타일' <- rowMeans(data[lifestyle_columns], na.rm = TRUE)
data$'가치관 적합성' <- rowMeans(data[value_fit_columns], na.rm = TRUE)
data$'능력 적합성' <- rowMeans(data[ability_fit_columns], na.rm = TRUE)
data$'고객 만족도' <- rowMeans(data[satisfaction_columns], na.rm = TRUE)

# 대표 지표들만 포함한 데이터프레임 생성
factors_df <- data[c('고객 라이프스타일', '가치관 적합성', '능력 적합성', '고객 만족도')]

# 상관행렬 계산
correlation_matrix <- cor(factors_df)

# 상관행렬 출력
print(correlation_matrix)

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

# 상관행렬 산점도 플롯 생성
chart.Correlation(factors_df, histogram = TRUE, pch = 20,  text.size = 8, main = "요일별 상관관계")


# 다중회귀 분석을 위한 데이터프레임 생성
regression_data <- data.frame(
  '고객라이프스타일투자' = data$'고객 라이프스타일',
  '가치관적합성' = data$'가치관 적합성',
  '능력적합성' = data$'능력 적합성',
  '고객만족도' = data$'고객 만족도'
)

# 다중회귀 분석 실행
regression_model <- lm(고객만족도 ~ ., data = regression_data)

# 다중회귀 분석 결과 출력
summary(regression_model)




# 사용 기간과 고객 만족도 간의 단순 선형 회귀 분석

# 단순회귀 분석을 위한 데이터프레임 생성
regression_data <- data.frame(
  '사용기간' = data$'기간_숫자',
  '고객만족도' = data$'고객 만족도'
)

# 단순회귀 모델 생성
regression_model <- lm(사용기간 ~ 고객만족도 , data = regression_data)

# 회귀 모델 요약 출력
summary(regression_model)

# 회귀 직선 시각화
plot(regression_data$고객만족도, regression_data$사용기간, 
     xlab = '고객 만족도', ylab = '사용기간', 
     main = '사용 기간과 고객 만족도 간의 단순 선형 회귀', 
     pch = 20, col = 'blue')
abline(regression_model, col = 'red')


# 한 달 평균 이용 횟수와 고객 만족도 간의 단순 선형 회귀 분석

# 단순회귀 분석을 위한 데이터프레임 생성
regression_data <- data.frame(
  '한달평균이용횟수' = data$'MAU_숫자',
  '고객만족도' = data$'고객 만족도'
)

# 단순회귀 모델 생성
regression_model <- lm(한달평균이용횟수 ~ 고객만족도 , data = regression_data)

# 회귀 모델 요약 출력
summary(regression_model)

# 회귀 직선 시각화
plot(regression_data$고객만족도, regression_data$한달평균이용횟수,
     xlab = '고객 만족도', ylab = '한달평균이용횟수', 
     main = '한 달 평균 이용 횟수와 고객 만족도 간의 단순 선형 회귀', 
     pch = 20, col = 'blue')
abline(regression_model, col = 'red')







