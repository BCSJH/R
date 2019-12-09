install.packages("igraph")
library(igraph)
g_star <- graph(edges = NULL, n= NULL, directed = FALSE)
#방향성이 없고, 에지와 노드가 없는 igraph tㅐㅇ성 그래프 초기화

g_star <- g_star + vertex("A", shape="circle", size=30, color="yellow")
#그래프에 A 노드 추가

g_star <- g_star + vertices("B","C","D","E","F",shape="circle",size=30)
#그래프에 B, C, D, E, F 노드 추가

g_star <- g_star + edge("A","B")
#그래프에 A와 B간 에지 추가

g_star <- g_star + edge("A","C","A","D","A","E","A","F")
plot(g_star)

vcount(g_star) # 네트워크 내의 노드 수
ecount(g_star) # 노드 간 연결된 에지의 총 수

degree(g_star, normalized = FALSE)
#각 노드의 연결 정도 중심성 A는 5개와 연결되어 있음

degree(g_star, normalized = TRUE)# 정규화된 연결 중심성 
tmax <- centr_degree_tmax(g_star)
centralization.degree(g_star,normalized = FALSE)$centralization/tmax

####################################################################

closeness(g_star,normalized = FALSE)
#각노드의 근접 중심성

closeness(g_star,normalized = TRUE) 
#각노드의 정규화된 근접 중심성 -> A는 모든 노드와 1단계로 연결되어 있기 떄문에 1의 값을 가짐
tmax <- centralization.closeness.tmax(g_star) # 이론적인 근접 중심화의 최댓값
centralization.closeness(g_star,normalized = FALSE)$centralization/tmax # 정규화된 근접 중심화

###################################################################

betweenness(g_star,normalized = FALSE) # 각 노드의 비정규화된 중개 중심성
betweenness(g_star,normalized = TRUE) # 이론적인 중개 중심화 최댓값
tmax <- centralization.closeness.tmax(g_star)
centralization.closeness(g_star,normalized = FALSE)$centralization/tmax

#밀도
graph.density(g_star)# g_start 그래프에 대한 네트워크 밀도

#최단경로
shortest.paths(g_star)
get.shortest.paths(g_star,"A") #A에서 갈 수 있는 모든 최단 경로

