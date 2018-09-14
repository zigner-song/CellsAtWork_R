# TF-IDF
get_keywords <- worker("keywords", topn = 5,
                  stop_word='ciku/my_stopwords.txt',
                  user = 'ciku/mydict.txt')
seg["我上上上周打了个喷嚏我上上周不小心擦破了点皮我上周差点得有流行性病毒引起的感冒我这周差点食物中毒对，没错，目前为止你们看到的都是我体内发生的不多说了，我去见我的血小板老婆了"]

get_vec.keywords<-function(x){
  vector_keywords(x, get_keywords)
}



keyword_platelet.list<-lapply(word_platelet.list,get_vec.keywords)

freq.platelet.keywords.df<-freq(keyword_platelet.list%>% unlist)                                                
freq.platelet.keywords.df<-freq.platelet.keywords.df[freq.platelet.keywords.df$freq>10,]
freq.platelet.keywords.df<-arrange(freq.platelet.keywords.df,desc(freq))

wordcloud2(freq.platelet.keywords.df[-1,],
           fontFamily = "冬青黑体简体中文",
           backgroundColor = 'black',
           color = 'random-light')





# 全量

#word.list<-lapply(review.df$content.message,seg.fun)
word.list<-apply(review.df["content.message"],1,seg.fun)
#word.list<-seg.fun(review.df$content.message)  

keywords.list<-lapply(word.list,get_vec.keywords)

freq.keywords.df<-freq(keywords.list %>% unlist)                                                
freq.keywords.df<-freq.keywords.df[freq.keywords.df$freq>10 & str_length(freq.keywords.df$char)>1,]
freq.keywords.df<-arrange(freq.keywords.df,desc(freq))

wordcloud2(freq.keywords.df,
           fontFamily = "冬青黑体简体中文",
           backgroundColor = 'black',
           color = 'random-light')
