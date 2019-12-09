#install.packages("xlsx")
#install.packages("nnet")

library(xlsx)
library(nnet)

data <- read.xlsx2(file.choose(),1)
data

data$종가 <- gsub(",","",data$종가) #종가 항목 데이터 내의 콤마 삭제
data$종가 <- as.numeric(data$종가) # 종가 항목의 문자 데이터를 숫자로 변환

df <- data.frame(일자 = data$년.월.일,종가=data$종가)

df <- df[order(df$일자),] # 원데이터를 오름차순으로 정렬

n <- nrow(df)#데이터 프레임의 행의 수 n = 121

rownames(df) <- 1:n # 날짜 순으로 정렬된 데이터 프레임의 행 번호를 1부터 121까지 수정

#데이터 정규화 page 398
pnorm <- (df$종가-min(df$종가))/(max(df$종가)-min(df$종가)) * 0.9 + 0.05
df <- cbind(df,종가norm=pnorm) # 데이터 정규화
df

#학습과 테스트 데이터 분리
n80 <- round(n*0.8,0) # 데이터 세트의 80%에 해당하는 행번호를 소수점 0자리(정수)로 반올림
df.learing <- df[1:n80,] # 학습 데이터 세트 만들기 1~97행
df.learing

df.test <- df[(n80+1):n,] #테스트 데이터 세트 만들기 98~121행
df.test

INPUT_NODES <- 10
HIDDEN_NODES <- 10
OUTPUT_NODES <- 5
ITERATION <- 100

#입출력 데이터 파일 생성 함수
getDataSet <- function(item,from,to,size){
  dataframe <- NULL
  to <- to - size + 1
  for (i in from:to){
    start <- i
    end <- start + size -1
    temp <- item[c(start:end)]
    dataframe <- rbind(dataframe,t(temp))
  }
  return(dataframe)
}

in_learning <- getDataSet(df.learing$종가norm,1,92,INPUT_NODES)
in_learning # 1~92번째 데이터로부터 각 행이 입력 노드 수의 크기가 되는 입력 데이터 세트를 만듦

out_learning <- getDataSet(df.learing$종가norm,11,97,OUTPUT_NODES)
out_learning # 11~97번째 데이터로부터 각행이 출력 노드 수의 크기가 되는 출력 데이터 세트를 만듦

model <- nnet(in_learning, out_learning,size=HIDDEN_NODES,maxit = ITERATION)
#입력 데이터 세트 출력 데이터 세트 그리고 은닉층 노드 수(size)에 해당하는 모형 생성과 학습, maxit는 학습 수를 뜻함

in_test <- getDataSet(df.test$종가norm,1,19,INPUT_NODES)
# 테스트 데이터 세트의 정규화된 종가에서 1~19번째 데이터로부터
# 각 행이 입력 노드 수의 크기가 되는입력 데이터 세트를 만듦
in_test

#예측
predicted_values <- predict(model,in_test,type="raw")
Vpredicted <- (predicted_values-0.05)/0.9*(max(df$종가)-min(df$종가)) + min(df$종가)
Vpredicted

#예측 기간의 실제 값 데이터 만들기
Vreal <- getDataSet(df.test$종가,11,24,OUTPUT_NODES)
Vreal

#오차 계산 및 출력
ERROR <- abs(Vreal - Vpredicted) #각 행별 각 열들 간 절대오차
MAPE <- rowMeans(ERROR/Vreal) *100 #각 행별 각 열의 실제 값 대비 절대오차를 행별로 평균(rowMeans)하여 백분율 나타냄
MAPE

mean(MAPE)

in_forecasting <- df$종가norm[112:121] # 입력 데이터 만들기
in_forecasting

predicted_values <- predict(model,in_forecasting,type="raw")#예측
Vpredicted <- (predicted_values-0.05)/0.9 * (max(df$종가)-min(df$종가)) + min(df$종가)
Vpredicted


plot(71:100,df$종가[71:100],xlab="기간",ylab="종가",xlim=c(71,105),ylim=c(1800,2100),type="o")
#71~100시점에 대한 과거 종가 데이터 출력, 예측치 출력을 위해 x축은 71~105로 설정
lines(101:105,Vpredicted,type="o",col="red") # 그래프 101~105 시점에 예측치 추가
abline(v=100,col="blue", lty=2) #실제치와 예측치 구분을 위해 100시점에 수직점선 추가

# 그래프 출력
plot(82:121, df$종가[82:121], xlab="기간", ylab="종가", xlim=c(82,126), ylim=c(1800, 2100), type="o")
lines(122:126, Vpredicted, type="o", col="red")
abline(v=121, col="blue", lty=2)
