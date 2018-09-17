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



# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(Candi,Month) {
  # Careful not to let just any name slip in here; a
  par(family=("Heiti TC Light"))
  text <- read.csv("Apple_news_cleaning.csv",encoding = "Big5")
  text <- text[text$new==Candi,]
  text <- text[text$month==Month,]
  
  
  myCorpus = Corpus(VectorSource(text$content))
  
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
  return(freqFrame)
  
  
})

