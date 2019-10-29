library(XML)
library(RCurl)

# �˻� ù ������ �ٿ�ε�
url <- "https://www.youtube.com/results?sp=SADqAwA%253D&q=I+dreamed+a+dream"
doc <- getURL(url)
html <- htmlParse(doc, encoding="UTF-8")

# 1 ������ URL
link <- xpathSApply(html, "//div[@class='branded-page-box search-pager  spf-link ']//button",  xmlGetAttr, 'data-redirect-url')
url1 <- paste("https://www.youtube.com", link, sep="")  
# 2 ~ 5 ������ URLs
link <- xpathSApply(html, "//div[@class='branded-page-box search-pager  spf-link ']//a",  xmlGetAttr, 'href')
url2 <- paste("https://www.youtube.com", link[1:4], sep="") 
urls <- c(url1, url2)
urls

titles <- NULL
for(url in urls) {
  # �������� ���� 
  doc <- getURL(url)
  html <- htmlParse(doc, encoding="UTF-8")
  title <- xpathSApply(html, "//h3[@class='yt-lockup-title ']/a", xmlValue) 
  # ������ ���� 
  titles <- c(titles, title)
}
titles

###################






          