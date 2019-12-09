iteration <- 5000
plot(0,0,xlab="동전을 던진 횟수", ylab = "앞면이 나오는 비율",xlim=c(0,iteration),ylim=c(0,1))
abline(a=0.5,b=0,col="red") 
# 앞면이 나오는 수학적인 확률 0.5 표시
#abline은 직성을 그리며, a는 y절편 b는 기울기를 뜻함
sum <- 0
for(x in 1:iteration){
  coin <- sample(c("앞면","뒷면"),1, replace=T) #동전의 앞면 또는 뒷면에서 한 면을 추출
  #주사위 2개를 동시에 던지는 경우 하나씩 2번 던지는 경우와 같음
  #dice <- sample(1:6,2,replace =T)
  #던진 주사위 같은 수가 나왔는지 비교하는 방법
  #sum <- 0
  #if(dice[1]==dice[2])
  # sum<-sum + 1
  #sum
  if(coin=="앞면")
    sum = sum + 1
  prob <- sum/x #시행 횟수까지 앞면이 나온 비율
  points(x,prob)
}

#동전을 5000번 던질 때 앞면이 나온 횟수
#시행 횟수 : 5000
iteration <- 5000 
x <- sample(c(0,1),iteration,replace = T) #앞면을 1 뒷면을 0으로 두고 5000번 표본 추출
round(sum(x)/iteration*100,1) # round 함수를 이용해서 확률을 소수 한 자리로 표현

# 379 page
iteration <- 1000
n_circle <- 0
for (i in 1:iteration){
  x <- runif(1,min=0,max=1) # 난수 발생에 의한 점 위치
  y <- runif(i,min=0,max=1)
  
  dist <- sqrt(x^2+y^2) # 원점에서 점까지의 거리
  
  if(dist<=1)
    n_circle <- n_circle+1 #거리가 1이하면 원 내의 점의 수 1증가
}
pi <- 4*n_circle/iteration #원주율 계산
pi