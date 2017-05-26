# R version 3.3.2 (2016-10-31)
# Platform: x86_64-apple-darwin13.4.0 (64-bit)
# Running under: macOS Sierra 10.12.2
# locale:
#   [1] zh_TW.UTF-8/zh_TW.UTF-8/zh_TW.UTF-8/C/zh_TW.UTF-8/zh_TW.UTF-8

# install packages
pkgs.needs <- c("XML", "dplyr", "stringr")
pkgs.installed <- installed.packages()[,"Package"] 
new.pkgs <- pkgs.needs[!(pkgs.needs %in% pkgs.installed)]
if(length(new.packages)) install.packages(new.pkgs)                         
library(XML)      # readHTMLTable
library(dplyr)    # data manipulation & pipe line
library(stringr)  # str_pad

# set working directory
mainDir <- "/Users/jen/Documents/web_crawler"
ifelse(!dir.exists(file.path(mainDir)), dir.create(file.path(mainDir)), "folder exists")
setwd(mainDir)

# define functions
## get Taiwan stocks monthly data (Stock Exchange Market only)
getStockData <- function(yyyy, mm, scode){
  mm <- mm %>% str_pad(2, pad = "0") %>% as.character
  url <- paste0("http://www.twse.com.tw/exchangeReport/STOCK_DAY?response=html&date=",yyyy,mm,"01","&stockNo=",scode)
  tb = url %>% readHTMLTable %>% .[[1]] 
  return(tb)
}
## get Taiwan market monthly data (Stock Exchange Market only)
getMartData <- function(yyyy, mm){
  mm <- mm %>% str_pad(2, pad = "0") %>% as.character
  url <- paste0("http://www.twse.com.tw/exchangeReport/FMTQIK?response=html&date=",yyyy,mm,"01")
  tb = url %>% readHTMLTable %>% .[[1]] 
  return(tb)
}

## examples
df <- getMartData(2017,5)
View(df)
df2 <- getStockData(2017,5,"0050") 
View(df2)
