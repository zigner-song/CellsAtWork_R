# 分词

#seg <- worker()
words_wangluo<-c(readLines('ciku/erciyuan.txt'),
                 readLines('ciku/wangluo.txt'),
                 readLines('ciku/cv.txt'),
                 (info.list$actor %>% plyr::ldply(function(x){as.data.frame(t(unlist(x)))}))$actor
                 ) %>% unique
words_medicine<-c(readLines('ciku/jiepou.txt'),
                  readLines('ciku/yixue.txt'),
                  readLines('ciku/cell.txt'),
                  readLines('ciku/cell2.txt'),
                  readLines('ciku/doc.txt'),
                  readLines('ciku/doc2.txt'),
                  readLines('ciku/intern_doctor.txt'),
                  #readLines('ciku/bio.txt'),
                  readLines('ciku/bio_dict.txt'),
                  (info.list$actor %>% plyr::ldply(function(x){as.data.frame(t(unlist(x)))}))$role,
                  "T细胞","献血"
                  ) %>% unique
words_medicine<-setdiff(words_medicine,c(words_wangluo,"屏蔽","过分","多的","生物","本质","细胞","第一","血小板止血"))
words_adj<-readLines('ciku/adj_people.txt',encoding='UTF-8')

mydict<-c(words_wangluo,
          words_medicine
          ) %>% unique()
writeLines(mydict,"ciku/mydict.txt")

my_stopwords<-c(readLines('ciku/stop_words.txt'),
                readLines('ciku/stop_words2.txt'),
                readLines(STOPPATH),
                "感觉","这番","更新","哈哈哈","终于","举报","弹幕","这集","一集","哈哈哈哈","这部") %>% unique()
writeLines(my_stopwords,"ciku/my_stopwords.txt")


seg <- worker(stop_word='ciku/my_stopwords.txt',
              user = 'ciku/mydict.txt')

wd.list<-seg[review.df$content.message]




library(wordcloud2)

freq.df<-freq(wd.list)

freq.df<-freq.df[freq.df$freq>=20,]
freq.df<-freq.df[str_length(freq.df$char)>1,]
freq.df<-arrange(freq.df,desc(freq))

wordcloud2(freq.df,
           fontFamily = "冬青黑体简体中文",
           backgroundColor = 'black',
           color = 'random-light')
#names(freq.df)[1]<-"word"


seg.fun<-function(x){
  return((seg[x]))
}









#血小板------
review_platelet.df<-subset(review.df,str_detect(review.df$content.message,"血小板"))
wd.platelet<-seg[review_platelet.df$content.message]

tagger <- worker("tag")
wd.platelet<-(tagger <= wd.platelet)
data.frame(char=wd.platelet,tag=names(wd.platelet)) %>%distinct()

freq.platelet.df<-freq(wd.platelet)
freq.platelet.df<-merge(freq.platelet.df,
                        data.frame(char=wd.platelet,tag=names(wd.platelet)) %>%distinct(),
                        by="char")

freq.platelet.df<-freq.platelet.df[freq.platelet.df$freq>=20,]
freq.platelet.df<-freq.platelet.df[str_length(freq.platelet.df$char)>1,]
freq.platelet.df<-freq.platelet.df[!(freq.platelet.df$tag %in% c('m','z','o','e','q','p','t','d')),]
freq.platelet.df<-arrange(freq.platelet.df,desc(freq))
wordcloud2(freq.platelet.df[-1,],
           fontFamily = "冬青黑体简体中文",
           backgroundColor = 'black',
           color = 'random-light'
           )


word_platelet.list<-apply(review_platelet.df["content.message"],1,seg.fun)
length(word_platelet.list)


f_adj<-function(x){
  if(length(x)>0){
    #x<-c("白细胞","可爱")
    x<-setdiff(x,"血小板")
    x<-unique(x)
    xx<-intersect(x,words_adj)
    xx<-as.numeric(length(xx)>0)
    return(xx)
  } else {
    return(0)
  }
}
f_medicine<-function(x){
  xx<-0
  if(length(x)>0){
    #x<-c("白细胞","可爱")
    x<-unique(x)
    x<-setdiff(x,
               c((info.list$actor %>% plyr::ldply(function(x){as.data.frame(t(unlist(x)))}))$role,
                 "白细胞","T细胞","血小板")
               )
    if(length(x)>0){
      for(i in x){
        if(str_length(i)>1 & i %in% words_medicine){
          xx<-1
          break()
        }
      }
    }
    
  } else {
    xx<-0
  }
  return(xx)
}




cleaning_list_char<-function(x){
  x<-x[str_length(x)>1 & !str_detect(x,"av\\d{1,}")]
  return(x)
}
word_platelet.list<-lapply(word_platelet.list,cleaning_list_char)
med<-lapply(word_platelet.list,f_medicine) %>% unlist
med_key<-lapply(keyword_platelet.list,f_medicine) %>% unlist

review_platelet.df$med<-med


charact<-lapply(word_platelet.list,f_adj) %>% unlist
review_platelet.df$charact<-charact
xtabs(~med+charact)
table(med)
table(med_key)
review_platelet.df$content.message[med_key==1]
review_platelet.df$content.message[review_platelet.df$med==1]

review_platelet.df$content.message[review_platelet.df$content.message %>% str_detect('会员')]


length(charact)





###


review_platelet.df$like<-as.numeric(review_platelet.df$like)

summaryBy(like~med,review_platelet.df[review_platelet.df$like<=15,],FUN = function(x){mean(x,na.rm=T)})

glm(like~med,data = review_platelet.df,family = poisson()) %>% summary()

t.test(like~med,data = review_platelet.df[review_platelet.df$like<=15,])


review_platelet.df$like_rank<-ifelse(review_platelet.df$like==0,1,
                              ifelse(review_platelet.df$like<=2,2,
                              ifelse(review_platelet.df$like<=15,3,4))) %>% 
                          factor(
                                levels = 1:4,
                                labels = c('0点赞','1-2点赞','3-15点赞','15+点赞')
                              )

ins<-xtabs(~like_rank+med,review_platelet.df) %>% prop.table(2)%>% data.frame()

ins$yy<-1-decimal(cumsum(ins$Freq)-ins$Freq/2)

ggplot(ins)+
  geom_col(aes(x=med,fill=like_rank,y=Freq))+
  geom_text(aes(x=med,y=yy,label=scales::percent(Freq)),color='white')+
  #coord_flip()+
  #scale_y_reverse()+
  scale_x_discrete(label = c("医疗无关评论","医疗相关评论"))+
  theme(text = element_text(family = 'STHeiti'),
        panel.background = element_blank(),
        axis.title  = element_blank(),
        axis.text.y= element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        #legend.position = "bottom",
        plot.margin=unit(c(0,0,0,0),'lines'),
        plot.background = element_blank())


library(ggplot2)
ggplot(review_platelet.df[review_platelet.df$like<50000,],aes(x=med,y=like))+geom_jitter()
