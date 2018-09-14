#获取《工作细胞》信息 ---


# 总体信息 --------
url<-"https://bangumi.bilibili.com/media/web_api/search/result/?season_month=7&pub_date=2018&season_type=1&page_size=30"
web<-read_html(url)


info.list<-(web %>% html_text() %>% 
  str_extract("\\[.+\\]") %>% rjson::fromJSON())[[1]]

url<-paste('https://bangumi.bilibili.com/jsonp/seasoninfo/',
           info.list$season_id,
           '.ver?callback=seasonListCallback&jsonp=jsonp',sep="")
web<-read_html(url)

ins.list<-(web %>% html_text() %>% 
  str_remove("^seasonListCallback\\(") %>% 
  str_remove("\\);$") %>%
  str_c("[",.,"]",sep="") %>% rjson::fromJSON())
info.list<-c(info.list,ins.list[[1]]$result)
rm(ins.list)

# 分集信息 -----
ep.df<-info.list$episodes %>% plyr::ldply(function(x){as.data.frame(t(unlist(x)))})


