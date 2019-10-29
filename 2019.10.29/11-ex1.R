##########################
# 방법1 (한글 질의 포함)
##########################

library(XML)
library(RCurl)

# 검색 키워드 인코딩 
# 영문 질의의 예
query <- URLencode(iconv("I dreamed a dream","euc-kr","UTF-8"))
# 한글 질의의 예
query <- URLencode(iconv("가고파","euc-kr","UTF-8"))
# 검색 URL
url <- paste("https://www.youtube.com/results?q=", query,sep="")

# 검색 페이지 다운로드
doc <- getURL(url)
# html문서로 변환 
html <- htmlParse(doc, encoding="utf-8")
html

# '제목' 추출(<h3> 태그 내의 <a> 태그 데이터
title <- xpathSApply(html, "//h3[@class='yt-lockup-title ']/a", xmlValue) 
title


##########################
# 방법2
##########################

library(XML)
library(RCurl)
# 영문 질의 
url <- "https://www.youtube.com/results?sp=SADqAwA%253D&q=I+dreamed+a+dream"
doc <- getURL(url)
html <- htmlParse(doc, encoding="UTF-8")
title <- xpathSApply(html, "//h3[@class='yt-lockup-title ']/a", xmlValue) 
title



          