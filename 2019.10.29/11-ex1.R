##########################
# ���1 (�ѱ� ���� ����)
##########################

library(XML)
library(RCurl)

# �˻� Ű���� ���ڵ� 
# ���� ������ ��
query <- URLencode(iconv("I dreamed a dream","euc-kr","UTF-8"))
# �ѱ� ������ ��
query <- URLencode(iconv("������","euc-kr","UTF-8"))
# �˻� URL
url <- paste("https://www.youtube.com/results?q=", query,sep="")

# �˻� ������ �ٿ�ε�
doc <- getURL(url)
# html������ ��ȯ 
html <- htmlParse(doc, encoding="utf-8")
html

# '����' ����(<h3> �±� ���� <a> �±� ������
title <- xpathSApply(html, "//h3[@class='yt-lockup-title ']/a", xmlValue) 
title


##########################
# ���2
##########################

library(XML)
library(RCurl)
# ���� ���� 
url <- "https://www.youtube.com/results?sp=SADqAwA%253D&q=I+dreamed+a+dream"
doc <- getURL(url)
html <- htmlParse(doc, encoding="UTF-8")
title <- xpathSApply(html, "//h3[@class='yt-lockup-title ']/a", xmlValue) 
title



          