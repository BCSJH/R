# p.315
# Chap 12- 네트워크 만들기
#install.packages("igraph")
library(igraph)

g_star <- graph(edges=NULL,n=NULL,directed=FALSE)
plot(g_star)

g_star <- g_star + vertex("A", shape="circle", size=40, color="yellow")
plot(g_star)

g_star <- g_star + vertices("B", "C", "D", "E", "F", shape="circle", size=40)
plot(g_star)

g_star <- g_star + edge("A", "B")
plot(g_star)

g_star <- g_star + edges("A", "C",  "A", "D", "A", "E", "A", "F")
plot(g_star)

##################################################
for (i in 1:3){
  g_star <- g_star + edge("B", "D")
  g_star <- g_star + edge("B", "E")
  g_star <- g_star + edge("F", "E")
  g_star <- g_star + edge("F", "C")
  g_star <- g_star + edge("D", "C")
}

g_star <- g_star + edges("A", "C",  "A", "D", "A", "E", "A", "F") 
+ edges("A", "C",  "A", "D", "A", "E", "A", "F")
plot(g_star)
########################################################################################

##################################################
# p.316 no. of Nodes and no. of edges in totoal
vcount(g_star) 
ecount(g_star)  

g_Y <- graph(edges=NULL,n=NULL,directed=FALSE)
g_Y <- g_Y + vertices("A", "B", "C", "D", "E", "F", shape="circle", size=30)
#g_Y <- g_Y + edge("A", "B", "A", "C", "A", "D", "D", "E", "E", "F")
g_Y <- g_Y + edge("A", "B", "C", "A", "B", "C","D", "E", "F", "D", "E", "F")

#g_Y <- g_Y + edge("D", "E", "F", "D", "E", "F")
plot(g_Y)

g_ring <- graph(edges=NULL,n=NULL,directed=FALSE)
g_ring <- g_ring + vertices("A", "B", "C", "D", "E", "F", shape="circle", size=30)
g_ring <- g_ring + edge("A", "B", "B", "C", "C", "D", "D", "E", "E", "F", "F", "A")
plot(g_ring)

# p.322
# 연결 정도(g_star 그래프)
degree(g_star, normalized=FALSE)
degree(g_star, normalized=TRUE)

tmax <- centr_degree_tmax(g_star)
centralization.degree(g_star, normalized=FALSE)$centralization / tmax

# p.324
# 연결 정도(g_Y 그래프)
degree(g_Y, normalized=FALSE)
degree(g_Y, normalized=TRUE)

tmax <- centr_degree_tmax(g_Y)
centralization.degree(g_Y, normalized=FALSE)$centralization / tmax

# 연결 정도 (g_ring 그래프)
degree(g_ring, normalized=FALSE)
degree(g_ring, normalized=TRUE)

tmax <- centr_degree_tmax(g_ring)
centralization.degree(g_ring, normalized=FALSE)$centralization / tmax

# p.327
# 근접 (g_star 그래프)
closeness(g_star, normalized=FALSE)

# p.328
closeness(g_star, normalized=TRUE)

tmax <- centralization.closeness.tmax(g_star)
centralization.closeness(g_star, normalized=FALSE)$centralization / tmax 

# p.329
# 근접 (g_Y 그래프)
closeness(g_Y)

# p.330
closeness(g_Y, normalized=TRUE)

tmax <- centralization.closeness.tmax(g_Y)
centralization.closeness(g_Y, normalized=FALSE)$centralization / tmax

# 근접 (g_ring 그래프)
closeness(g_ring)

closeness(g_ring, normalized=TRUE)

tmax <- centralization.closeness.tmax(g_ring)
centralization.closeness(g_ring, normalized=FALSE)$centralization / tmax

# p.334
# 중개(g_star 그래프)
betweenness(g_star, normalized=FALSE)

betweenness(g_star, normalized=TRUE)

tmax <- centralization.betweenness.tmax(g_star)
centralization.betweenness(g_star, normalized=FALSE)$centralization / tmax

# p.336
# 중개 (g_Y 그래프)
betweenness(g_Y, normalized=FALSE)
betweenness(g_Y, normalized=TRUE)

tmax <- centralization.betweenness.tmax(g_Y)
centralization.betweenness(g_Y, normalized=FALSE)$centralization / tmax

# 중개 (g_ring 그래프)
betweenness(g_ring, normalized=FALSE)
betweenness(g_ring, normalized=TRUE)

tmax <- centralization.betweenness.tmax(g_ring)
centralization.betweenness(g_ring, normalized=FALSE)$centralization / tmax

# p.337
# 네트워크 밀도
graph.density(g_star)

# p.338
# 최단 경로
shortest.paths(g_ring)

get.shortest.paths(g_ring, "A")

# p.339
get.shortest.paths(g_ring, "A", "C")

get.shortest.paths(g_ring, "A", c("C", "E"))

# p.340
average.path.length(g_ring)





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
# 모든 노드를 포함하는 노드들의 연속을 만듦
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

# vmax(107)에 해당하는 노드의 연결정도
degree(sn.df, vmax)


# p.351
summary(degree(sn.df))

# 사용자별 연결정도 그래프
plot(degree(sn.df), xlab="사용자 번호", ylab="연결 정도", type='h')

# 연결정도 분포 및 출력
sn.df.dist <- degree.distribution(sn.df)

# 출력결과:
# 연결정도가 작은 노드들이 많고, 연결정도가 큰 노드들은 적은 분포를 나타냄.
# 이러한 모양의 분포를 멱함수 분포라고 하며,
# 네트워크의 많은 분포는 정규분포와는 다르게 멱함수 분포를 띠고 있음
plot(sn.df.dist, xlab="연결 정도", ylab="확률")

# p.352
# 근접 
# 정규화된 근접 중심성
closeness(sn.df, normalized=TRUE)

# 이론적인 최대 근접 중심성
tmax <- centralization.closeness.tmax(sn.df)
# 정규화된 근접 중심화
centralization.closeness(sn.df, normalized=FALSE)$centralization / tmax

# 정규화된 중개 중심ㅅ
betweenness(sn.df, normalized=TRUE)

# p.353
# 이론적인 최대 중개 중심성 및 정규화된 중개 중심성
tmax <- centralization.betweenness.tmax(sn.df)
centralization.betweenness(sn.df, normalized=FALSE)$centralization / tmax

# sn.df에 대한 네트워크 밀도
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

