library(XML)
library(rvest) # Easily Harvest (Scrape) Web Pages
library(RCurl)


url <- "https://www.coupang.com/np/search?q=%EC%A0%84%EB%8F%99%ED%82%A5%EB%B3%B4%EB%93%9C&channel=user&component=&eventCategory=SRP&trcid=&traid=&sorter=scoreDesc&minPrice=&maxPrice=&priceRange=&filterType=&listSize=36&filter=&isPriceRange=false&brand=&offerCondition=&rating=0&page="

df.products <- NULL

for (page in 1:10) {
  url2 <- paste(url, page, sep="")
  
  # doc <- htmlParse(url2, encoding="utf-8")
  # prod_name <- xpathSApply(doc, "//ul[@id='productList']//div[@class='name']", xmlValue) 
  doc <- read_html(url2, encoding="utf-8")
  prod_name <- doc %>% html_nodes(".name") %>% html_text() 
  
  
  prod_name <- gsub('\n', '', prod_name)
  prod_name <- gsub('\t', '', prod_name)
  prod_name <- gsub(' ', '', prod_name)
  
  
  price     <- xpathSApply(doc, "//ul[@id='productList']//strong[@class='price-value']", xmlValue)
  #price <- doc %>% html_nodes(".price-value") %>% html_text() 
  
  
  df <- data.frame(상품명= prod_name, 가격=price) 
  df.products <- rbind(df.products, df) 
}


df.products

#df.products$상품명 <- sort(format(df.products$상품명, justify = "left"))
#df.products

#df.products$가격 <- sort(format(df.products$가격, justify = "left"))
#df.products

df.products$가격 <- format(df.products$가격, justify = "left")
df.products
##########################################################
##########################################################


############################
library(XML)
library(rvest) # Easily Harvest (Scrape) Web Pages
library(RCurl)


url <- "https://www.coupang.com/np/search?q=%EC%A0%84%EB%8F%99%ED%82%A5%EB%B3%B4%EB%93%9C&channel=user&component=&eventCategory=SRP&trcid=&traid=&sorter=scoreDesc&minPrice=&maxPrice=&priceRange=&filterType=&listSize=36&filter=&isPriceRange=false&brand=&offerCondition=&rating=0&page="

df.products <- NULL

for (page in 1:10) {
  url2 <- paste(url, page, sep="")
  
  # doc <- htmlParse(url2, encoding="utf-8")
  # prod_name <- xpathSApply(doc, "//ul[@id='productList']//div[@class='name']", xmlValue) 
  doc <- read_html(url2, encoding="utf-8")
  prod_name <- doc %>% html_nodes(".name") %>% html_text() 
  
  
  prod_name <- gsub('\n', '', prod_name)
  prod_name <- gsub('\t', '', prod_name)
  prod_name <- gsub(' ', '', prod_name)
  
  
  price     <- xpathSApply(doc, "//ul[@id='productList']//strong[@class='price-value']", xmlValue)
  #price <- doc %>% html_nodes(".price-value") %>% html_text() 
  
  
  df <- data.frame(상품명= prod_name,가격=price) 
  df.products <- rbind(df.products, df) 
}


df.products

#df.products$상품명 <- sort(format(df.products$상품명, justify = "left"))
#df.products

df.products$가격 <- sort(format(df.products$가격, justify = "left"))
df.products

##########################################################
##########################################################
