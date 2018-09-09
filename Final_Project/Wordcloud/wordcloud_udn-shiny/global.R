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
  text <- read.csv("Udn_news_cleaning.csv",encoding = "Big5")
  text <- text[text$new==Candi,]
  text <- text[text$month==Month,]
  
  
  myCorpus = Corpus(VectorSource(text$V3))
  
  toSpace <- content_transformer(function(x,pattern){
    return(gsub(pattern," ",x))
  })
  
  clean_doc <- function(docs){
    clean_words <- c("[A-Za-z0-9]","、","《","『","』","【","】","／","，","。","！","「","（","」","）","\n","；",">","<","＜","＞","以上版本的瀏覽器。 爆","報導","記者")
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

