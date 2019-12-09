# p.395
# 패키지 설치 및 로딩
# xlsx 패키지를 위해 먼저 JAVA 구축여부 학인(부록 참조)
install.packages("xlsx")
install.packages("nnet")
library(xlsx)
library(nnet)

# p.396
# 시계열 데이터 읽기
data <- read.xlsx2(file.choose(), 1)
data
data$종가 <- gsub(",", "", data$종가) 
data$종가 <- as.numeric(data$종가) 

df <- data.frame(일자=data$년.월.일, 종가=data$종가)
df <- df[order(df$일자), ]    
n <- nrow(df)
rownames(df) <- 1:n

# p.398
# 데이터 정규화
norm <- (df$종가 - min(df$종가)) / (max(df$종가) - min(df$종가)) * 0.9 + 0.05
df   <- cbind(df, 종가norm=norm)
df

# p.399
# 학습 데이터와 테스트 데이터 분리
n80 <- round(n * 0.8, 0)
n80
df.learning <- df[1:n80, ]
df.learning
df.test     <- df[(n80+1):n, ]
df.test

# p.400
# 인공신경망 구조
INPUT_NODES   <- 10
HIDDEN_NODES  <- 10
OUTPUT_NODES  <- 5              
ITERATION     <- 100

# p.402
# 입출력 데이터 파일 생성 함수
getDataSet <- function(item, from, to, size) {
  dataframe <- NULL
  to <- to - size + 1                      # 마지막 행의 시작날짜 번호
  for(i in from:to) {                      # 각 행의 날짜 시작번호에 대한 반복
    start <- i                             # 각 행의 시작날짜 번호
    end   <- start + size - 1              # 각 행의 끝날짜 번호
    temp  <- item[c(start:end)]            # item에서 start~end 구간의 데이터 추출 
    dataframe <- rbind(dataframe, t(temp)) # t() 함수를 사용하여 열 단위의 데이터를 행으로 전환 후 데이터 프레임에 추가
  }  
  return(dataframe)                        # 데이터 프레임 반환 
}

# p.403
# 입력 데이터 만들기
in_learning <- getDataSet(df.learning$종가norm, 1, 92, INPUT_NODES)
in_learning

# p.404
# 목표치 데이터 만들기
out_learning <- getDataSet(df.learning$종가norm, 11, 97, OUTPUT_NODES)
out_learning

# 학습
model <- nnet(in_learning, out_learning, size=HIDDEN_NODES, maxit=ITERATION)

# p.407
# 입력 데이터 만들기
in_test <- getDataSet(df.test$종가norm, 1, 19, INPUT_NODES)
in_test

# 예측
predicted_values <- predict(model, in_test, type="raw")
Vpredicted <- (predicted_values - 0.05) / 0.9 * (max(df$종가) - min(df$종가)) + min(df$종가)
Vpredicted

# p.408 
# 예측 기간의 실제 값 데이터 만들기
Vreal <- getDataSet(df.test$종가, 11, 24, OUTPUT_NODES)
Vreal

# p.409
# 오차 계산 및 출력
ERROR <- abs(Vreal - Vpredicted)
MAPE <- rowMeans(ERROR / Vreal) * 100
MAPE

mean(MAPE)

# p.410
# 예측 입력 데이터 만들기
in_forecasting <- df$종가norm[112:121]
in_forecasting 

# 예측
predicted_values <- predict(model, in_forecasting, type="raw")
Vpredicted <- (predicted_values - 0.05) / 0.9 * (max(df$종가) - min(df$종가)) + min(df$종가)
Vpredicted

# p.411
# 그래프 출력
plot(82:121, df$종가[82:121], xlab="기간", ylab="종가", xlim=c(82,126), ylim=c(1800, 2100), type="o")
lines(122:126, Vpredicted, type="o", col="red")
abline(v=121, col="blue", lty=2)


#####################
