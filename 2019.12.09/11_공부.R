install.packages("XML")
install.packages("RCurl")
library(XML)
library(RCurl)

query <- URLencode(iconv("I dreamed a dream", "euc-kr","UTF-8"))
url <- paste("https://www.youtube.com/results?q=",query,sep="")

doc <- getURL(url)
html <- htmlParse(doc, encoding="UTF-8")