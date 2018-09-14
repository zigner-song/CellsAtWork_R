# review_cleaning


#删除回复
review.df$content.message<-str_remove_all(review.df$content.message,"^回复.+:")
 
# 删除表情
review.df$content.message<-str_remove_all(review.df$content.message,"\\[.+\\]")

#删除颜文字
review.df$content.message<-str_remove_all(review.df$content.message,"\\s{1,}")

#删除评论字数小于15字的评论
review.df<-review.df[review.df$content.message %>% str_length() >=7 ,]


