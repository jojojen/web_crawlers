# R version 3.3.2 (2016-10-31)
# Platform: x86_64-apple-darwin13.4.0 (64-bit)
# Running under: macOS Sierra 10.12.2
# 
# locale:
#   [1] zh_TW.UTF-8/zh_TW.UTF-8/zh_TW.UTF-8/C/zh_TW.UTF-8/zh_TW.UTF-8

# install packages
pkgs.needs <- c("rvest", "pipeR", "stringr")
pkgs.installed <- installed.packages()[,"Package"] 
new.pkgs <- pkgs.needs[!(pkgs.needs %in% pkgs.installed)]
if(length(new.packages)) install.packages(new.pkgs)                         
library(rvest)    ## to get content
library(pipeR)    ## pipe line
library(stringr)  ## str_c

# define function
getUdnNewsCont <- function(url) {
  udn = read_html(url,encoding="UTF-8")
  artic = udn %>% html_nodes('p') %>% html_text() %>% 
    iconv(from = 'UTF-8',to = 'UTF-8') %>% str_c(collapse='.')
  return(artic)
}  

# set progressBar (60 news in one page)
pb_news <- txtProgressBar(1, 60, style=3) 

## get news list
surl <- "http://money.udn.com/money/breaknews"
outTbl = read_html(surl, encoding="UTF-8") %>% 
          html_node("#ranking_table") %>% html_table
ranking_table = read_html(surl, encoding="UTF-8") %>% html_node("#ranking_table")

## get news content
url = ranking_table %>% html_nodes('a') %>% html_attr('href') 
content <- character(60)
for (i in c(1:length(url))) {
  content[i] <- getUdnNewsCont(url[i])
  setTxtProgressBar(pb_news, i)
}

## combine data
news <- data.frame(outTbl, url=url, content=content)
View(news) 
