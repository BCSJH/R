# p.315
# Chap 12- 네트워크 만들기
#install.packages("igraph")

# p.345
# 페이스북 사용자 데이터 읽기와 그래프 출력
library(igraph)
sn <- read.table(file.choose(), header=F)
head(sn)

tail(sn)

# p.346
sn.df <- graph.data.frame(sn, directed=FALSE)
plot(sn.df)

# p.347
# 페이스북 사용자 1번과 연결된 사용자들의 그래프
sn1 <- subset(sn, sn$V1==1)
sn1.df <- graph.data.frame(sn1, directed=FALSE)
plot(sn1.df)

# p.348
# 네트워크의 크기: 노드의 총수를 나타나며, 
# 페이스북 사용자 데이터 세트는 총 4,039명의 사용자로 구성
vcount(sn.df)

# 노드간의 연결된 edge의 총수,
# 페이스북 사용자간 총 연결수는 총 88,234개로 구성
ecount(sn.df)

# 네트워크에 있는 노들들의 이름
# 설명: 모든 노드를 포함하는 노드들의 연속을 만듦
# 반환 값: 모든 노드를 포함하는 노드들의 연속
V(sn.df)$name

# p.350
# 정규화된 연결 정도 중심성 
degree(sn.df, normalized=TRUE)


tmax <- centralization.degree.tmax(sn.df) # 이론적인 최대 연결정도 중심
# 정규화된 연결 정도 중심화
centralization.degree(sn.df, normalized=FALSE)$centralization / tmax

# 노드 중에서 연결정도가 최대인 것 
vmax <- V(sn.df)$name[degree(sn.df)==max(degree(sn.df))]
vmax

# vmax에 해당하는 노드의 연결정도
# vmax에 해당하는 "---"노드는 ???개의 연결을 가짐
degree(sn.df, vmax)


# p.351
summary(degree(sn.df))

# 사용자별 연결정도 그래프
plot(degree(sn.df), xlab="사용자 번호", ylab="연결 정도", type='h')

# 연결정도 분포 및 출력
# 설명: 노드에 연결된 edge의 분포
# 반환값: 각 노드의 연결 분포를 나타내는 벡터
sn.df.dist <- degree.distribution(sn.df)

# 출력결과:
# 연결정도가 작은 노드들이 많고, 연결정도가 큰 노드들은 적은 분포를 나타냄.
# 이러한 모양의 분포를 멱함수 분포라고 하며,
# 네트워크의 많은 분포는 정규분포와는 다르게 멱함수 분포를 띠고 있음
plot(sn.df.dist, xlab="연결 정도", ylab="확률")

# p.352
# 근접 
# 정규화된 근접 중심성
# 근접 중심성은 한 점이 다른 모든 점들간에 얼마나 가까운가를 나타내는 지표
closeness(sn.df, normalized=TRUE)

# 이론적인 최대 근접 중심성
tmax <- centralization.closeness.tmax(sn.df)
# 정규화된 근접 중심화
centralization.closeness(sn.df, normalized=FALSE)$centralization / tmax

# 정규화된 중개 중심
# 중계 중심성은 연결망에서 한 노드가 다른 노드들 사이에 위치하는 정도를 나타내는 지표
betweenness(sn.df, normalized=TRUE)

# p.353
# 이론적인 최대 중개 중심성 및 정규화된 중개 중심성
tmax <- centralization.betweenness.tmax(sn.df)
centralization.betweenness(sn.df, normalized=FALSE)$centralization / tmax

# sn.df에 대한 네트워크 밀도
# 밀도는 네트워크에서 전체 노드가 서로 간에 얼마나 많은 관계룰 맺고 있는가를 나타내는 지표
# 총 연결 정도를 연결 가능한 수로 나눈 비율(1%)
graph.density(sn.df)


# p.354
# 경로
# 네트워크 경로들에 대한 평균
# 임의의 두 사용자는 평균적으로 3.7단계를 거치면 연결됨
average.path.length(sn.df)

# sn 데이터 세트에서 사용자 ID가 10보다 작은 ID로 구성된 서브세트를 sn10으로 저장
sn10 <- subset(sn, sn$V1<10 & sn$V2<10)

# sn10 데이터 세트를 igraph 형태의 그래프로 변환
sn10.graph <- graph.data.frame(sn10, directed=FALSE)
# 0~ 9번 사용자간 최소 경로에 대한 행렬
# Ex) 3번 사용자는 1번 사용자와 2단계로 연결됨
shortest.paths(sn10.graph)

# 5번 사용자와 연결된 최단 경로를 보여 줌
get.shortest.paths(sn10.graph, "5")


# p.355
# 5번과 9번 사용자간 최단 경로
get.shortest.paths(sn10.graph, "5", "9")

# 5번 사용자가 8번, 9번 사용자들과 연결된 각각 최단경로 
get.all.shortest.paths(sn10.graph, "5", c("8", "9"))
     
###############################

