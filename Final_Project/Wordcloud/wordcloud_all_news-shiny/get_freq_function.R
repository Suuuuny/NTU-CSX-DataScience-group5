library(NLP)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(memoise)
library(tibble)
library(data.table)
library(NLP)
library(tm)
library(jiebaRD)
library(jiebaR)
library(magrittr)
library(RColorBrewer)
library(wordcloud)

news_data = read.csv("all_news.csv", sep = "")


# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(Media,Candi,Month) {
  # Careful not to let just any name slip in here;
  news <- news_data[(news_data$media == Media)&(news_data$candi == Candi)&(news_data$month == Month),]
  
  
  myCorpus = Corpus(VectorSource(news$content))
  
  toSpace <- content_transformer(function(x,pattern){
    return(gsub(pattern," ",x))
  })
  clean_doc <- function(docs){
    clean_words <- c("[A-Za-z0-9]","、","《","『","』","【","】","／","，","。","！","「","（","」","）","\n","；",">","<","＜","＞","分享","記者","攝影","提及","表示","報導","我們","他們","的","也","都","就","與","但","是","在","和","及","為","或","且","有","含","為達最佳瀏覽效果，建議使用 Chrome、Firefox 或 Internet Explorer 10","新聞送上來！快加入自由電子報APP、LINE好友  2018年6月13日‧星期三‧戊戌年四月卅 注目新聞 1 中國卡車罷工延燒 司機高喊「打倒共產黨」！ 2 新版身分證票選出爐 設計師魯少綸奪首獎！ 3 洋腸又來！夜店外帶18歲妹 網站取名「台女很容易」... 4 侯友宜妻1塊土地99個門牌 蘇貞昌：太荒唐 5 連俞涵超正弟媳曝光…根本巨乳林志玲,","以上版本的瀏覽器。 爆")
    for(i in 1:length(clean_words)){
      docs <- tm_map(docs,toSpace, clean_words[i])
    }
    return(docs)
  }
  myCorpus <- clean_doc(myCorpus)
  
  mixseg = worker()
  segment <- c("柯文哲","姚文智","丁守中","台北市長","選舉","候選人","台灣","選票","柯市長","民進黨","國民黨","台北市民","市民")
  new_user_word(mixseg,segment)
  
  # 有詞頻之後就可以去畫文字雲
  jieba_tokenizer=function(d){
    unlist(segment(d[[1]],mixseg))
  }
  seg = lapply(myCorpus, jieba_tokenizer)
  freqFrame = as.data.frame(table(unlist(seg)))
  # 清除單字
  for(i in c(1:length(freqFrame$Var1))){
    if((freqFrame$Var1[i] %>% as.character %>% nchar) == 1){
      freqFrame[i,] <- NA
    }
  }
  freqFrame <- na.omit(freqFrame)
  write.table(freqFrame, paste(Media,"_",Candi,"_",Month,".csv"), sep = ",", row.names = FALSE)

})
# apple
getTermMatrix("apple","Ko",1)
getTermMatrix("apple","Ko",2)
getTermMatrix("apple","Ko",3)
getTermMatrix("apple","Ko",4)
getTermMatrix("apple","Ko",5)
getTermMatrix("apple","Di",1)
getTermMatrix("apple","Di",2)
getTermMatrix("apple","Di",3)
getTermMatrix("apple","Di",4)
getTermMatrix("apple","Di",5)
getTermMatrix("apple","Yao",1)
getTermMatrix("apple","Yao",2)
getTermMatrix("apple","Yao",3)
getTermMatrix("apple","Yao",4)
getTermMatrix("apple","Yao",5)

# ct
getTermMatrix("ct","Ko",1)
getTermMatrix("ct","Ko",2)
getTermMatrix("ct","Ko",3)
getTermMatrix("ct","Ko",4)
getTermMatrix("ct","Ko",5)
getTermMatrix("ct","Di",1)
getTermMatrix("ct","Di",2)
getTermMatrix("ct","Di",3)
getTermMatrix("ct","Di",4)
getTermMatrix("ct","Di",5)
getTermMatrix("ct","Yao",1)
getTermMatrix("ct","Yao",2)
getTermMatrix("ct","Yao",3)
getTermMatrix("ct","Yao",4)
getTermMatrix("ct","Yao",5)

# ltn
getTermMatrix("ltn","Ko",1)
getTermMatrix("ltn","Ko",2)
getTermMatrix("ltn","Ko",3)
getTermMatrix("ltn","Ko",4)
getTermMatrix("ltn","Ko",5)
getTermMatrix("ltn","Di",1)
getTermMatrix("ltn","Di",2)
getTermMatrix("ltn","Di",3)
getTermMatrix("ltn","Di",4)
getTermMatrix("ltn","Di",5)
getTermMatrix("ltn","Yao",1)
getTermMatrix("ltn","Yao",2)
getTermMatrix("ltn","Yao",3)
getTermMatrix("ltn","Yao",4)
getTermMatrix("ltn","Yao",5)


# udn
getTermMatrix("udn","Ko",1)
getTermMatrix("udn","Ko",2)
getTermMatrix("udn","Ko",3)
getTermMatrix("udn","Ko",4)
getTermMatrix("udn","Ko",5)
getTermMatrix("udn","Di",1)
getTermMatrix("udn","Di",2)
getTermMatrix("udn","Di",3)
getTermMatrix("udn","Di",4)
getTermMatrix("udn","Di",5)
getTermMatrix("udn","Yao",1)
getTermMatrix("udn","Yao",2)
getTermMatrix("udn","Yao",3)
getTermMatrix("udn","Yao",4)
getTermMatrix("udn","Yao",5)

