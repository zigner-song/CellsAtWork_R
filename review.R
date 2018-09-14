#爬取评论

review.df<-data.frame()
for(aid in ep.df$av_id){
  for(page in 1:20000){
    if(T
      #!(aid=="26833564" & page <= 124)
      ){
      #Sys.sleep(0.2)
      url<-paste("https://api.bilibili.com/x/v2/reply?jsonp=jsonp&type=1&sort=1&nohot=1&oid=",aid,"&pn=",page,sep="")
      web<-read_Url(url)
      ins<-web %>% html_text() %>% rjson::fromJSON()
      ins<-ins$data$replies
      print(c(aid,page))
      if(length(ins)==0){break()}
      for(i in 1:length(ins)){
        #取出回复的评论
        if(length(ins[[i]]$replies)>0){
          ins<-c(ins,ins[[i]]$replies)
          ins[[i]]$replies<-NULL
        }
      }
      ins.df<-ins %>% plyr::ldply(function(x){as.data.frame(t(unlist(x)))})
      ins.df$aid<-aid
      review.df<-bind_rows(review.df,ins.df)
    }
  }
}

save(review.df,file="review_raw")
review_raw<-review.df # 备份

#review.df <- review_raw
