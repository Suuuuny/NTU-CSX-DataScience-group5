library(NLP)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(memoise)



# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(Candi) {
  # Careful not to let just any name slip in here; a

  text <- read.csv("FaceBookAPI-Taipei_new.csv")
  text <- text[text$name==Candi,]

  
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})

