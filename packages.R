# packages-----

library(stringr)
library(XML)
library(rvest)
library(dplyr)
library(doBy)

library(rjson)
library(jiebaR)

options(stringsAsFactors = F)

#library(devtools)
#install_github("qinwf/cidian")
#library(cidian)

read_Url <- function(url) {
  #读取网页信息，避免由于暂时链接失效或者断网造成功亏一篑
  out <- tryCatch({
    url %>% as.character() %>% read_html() 
  },
  error=function(cond) {
    message(paste("URL does not seem to exist:", url))
    message("Here's the original error message:")
    message(cond)
    return(NA)
  })    
  return(out)
}


range.std<-function(x,alpha=0.05){
  lower<-quantile(x,alpha)
  upper<-quantile(x,1-alpha)
  mysd<-x[x>lower & x<upper] %>% sd
  mymean<-x[x>lower & x<upper] %>% mean
  lower2<-mymean-3*mysd
  upper2<-mymean+3*mysd
  return(c(lower2,upper2))
}

# 计算小数部分
decimal<-function(x){
  x<-x-floor(x)
  return(x)
}
