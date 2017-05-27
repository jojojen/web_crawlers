# R version 3.3.2 (2016-10-31)
# Platform: x86_64-apple-darwin13.4.0 (64-bit)
# Running under: macOS Sierra 10.12.2
# 
# locale:
#   [1] zh_TW.UTF-8/zh_TW.UTF-8/zh_TW.UTF-8/C/zh_TW.UTF-8/zh_TW.UTF-8

# install packages
pkgs.needs <- c("rvest", "dplyr", "stringi", "RCurl", "XML")
pkgs.installed <- installed.packages()[,"Package"] 
new.pkgs <- pkgs.needs[!(pkgs.needs %in% pkgs.installed)]
if(length(new.packages)) install.packages(new.pkgs)                         
library(rvest)
library(dplyr)   # data manipulation & pipe line
library(RCurl)   # getURLContent
library(stringi)
library(XML)     # readHTMLTable

# define function
getStockList <- function(){
  url <- "http://isin.twse.com.tw/isin/C_public.jsp?strMode=2"
  html_file <- getURLContent(url)
  html_EncodingUTF8 = stri_encode(html_file,
                                  attr(html_file, "Content-Type")[2], "utf8") 
  tb = readHTMLTable(html_EncodingUTF8, encoding = "UTF-8") %>% .[[2]]
  tb <- tb[complete.cases(tb), ]
  tb$有價證券代號及名稱 <- as.character(tb$有價證券代號及名稱)
  pattern <- "\\s+" # create regex pattern
  list <- strsplit(tb$有價證券代號及名稱, pattern)
  scode <- sapply(list, function(x){as.character(x[1])})
  sname <- sapply(list, function(x){as.character(x[2])})
  tb = cbind(scode, sname, tb) %>% .[,-3]
  tb = data.frame(lapply(tb, as.character), stringsAsFactors=FALSE)
  return(tb)
}

# get news title and info
surl = "http://www.appledaily.com.tw/realtimenews/section/finance/"
apple = read_html(surl,encoding="UTF-8")
rddt = apple %>% html_nodes('.rtddt')
time = rddt %>% html_nodes('time') %>% html_text()
title = rddt %>% html_nodes('h1') %>% html_text() %>% iconv(from = 'UTF-8', to = 'UTF-8')
domain = "http://www.appledaily.com.tw"
url = rddt %>% html_nodes('a') %>% html_attr('href')
url = paste0(domain, url)
news = data.frame(time=time, title=title, url=url)
news <- data.frame(lapply(news, as.character), stringsAsFactors=FALSE)
news_title <- news$title
nid <- rownames(news)
news <- cbind(nid=nid, news)

# get stock list
pattern <- "^[0-9]{4}$" # create regex pattern
codes   <- getStockList()
stock_codes = subset(codes, grepl(pattern, codes$scode)) %>% .[, 1:2]

# create dtm/tdm
df <- news
snum <- length(stock_codes[, 1])
for (i in c(1:snum)){
  stock_name = stock_codes[i, 2] %>% as.character
  stock_code = stock_codes[i, 1]
  mention <- grepl(stock_name, title) %>% as.data.frame
  colnames(mention) <- stock_code
  df <- cbind(df, mention)
}
## dtm
dtm = df[, -c(2:5)]
## tdm
tdm <- df[, -c(1:5)]
tdm = df[, -c(1:5)] %>% t
colnames(tdm) <- nid
scode <- rownames(tdm)
tdm <- cbind(scode, tdm)
# clean data stocks/news matrix
df_stocks <- tdm[apply(tdm, MARGIN = 1, function(x) any(x == TRUE)), ] %>% 
              as.data.frame(stringsAsFactors=FALSE)
df_news <- dtm[apply(dtm[, -1], MARGIN = 1, function(x) any(x == TRUE)), ]

# check data
View(news)
View(df_stocks)
View(df_news)

# analyze stocks
df_stocks[, 2:31] = df_stocks[, 2:31] %>% as.matrix %>% as.logical %>% as.integer
df_stocks_at = df_stocks %>% cbind(stock = .[, 1], atValue = rowSums(.[, -1])) 
df_stocks_at = cbind(scode = df_stocks[, 1], 
                     atValue = rowSums(df_stocks[, -1]) %>% as.numeric) %>% 
                as.data.frame
df_stocks_at$scode = df_stocks_at$scode %>% as.character
df_stocks_at = merge(stock_codes, df_stocks_at, by = "scode") %>% .[,-1]
df_stocks_at$atValue = df_stocks_at$atValue %>% as.numeric 

## use barplot to show media coverage of stocks
barplot(df_stocks_at$atValue, names.arg = df_stocks_at$sname, 
        family = "STHeiti")  # choose a font  