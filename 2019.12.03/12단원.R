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
tmax <- centralization.closeness.tmax(g_star)
centralization.closeness(g_star,normalized = FALSE)$centralization/tmax

###################################################################

betweenness(g_star,normalized = FALSE) # 각 노드의 비정규화된 중개 중심성
betweenness(g_star,normalized = TRUE) # 이론적인 중개 중심화 최댓값
tmax <- centralization.closeness.tmax(g_star)
centralization.closeness(g_star,normalized = FALSE)$centralization/tmax

