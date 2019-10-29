library(XML)
library(RCurl)

# 검색 첫 페이지 다운로드
url <- "https://www.youtube.com/results?sp=SADqAwA%253D&q=I+dreamed+a+dream"
doc <- getURL(url)
html <- htmlParse(doc, encoding="UTF-8")

# 1 페이지 URL
link <- xpathSApply(html, "//div[@class='branded-page-box search-pager  spf-link ']//button",  xmlGetAttr, 'data-redirect-url')
url1 <- paste("https://www.youtube.com", link, sep="")  
# 2 ~ 5 페이지 URLs
link <- xpathSApply(html, "//div[@class='branded-page-box search-pager  spf-link ']//a",  xmlGetAttr, 'href')
url2 <- paste("https://www.youtube.com", link[1:4], sep="") 
urls <- c(url1, url2)
urls

titles <- NULL
for(url in urls) {
  # 페이지별 추출 
  doc <- getURL(url)
  html <- htmlParse(doc, encoding="UTF-8")
  title <- xpathSApply(html, "//h3[@class='yt-lockup-title ']/a", xmlValue) 
  # 추출결과 통합 
  titles <- c(titles, title)
}
titles

###################






          