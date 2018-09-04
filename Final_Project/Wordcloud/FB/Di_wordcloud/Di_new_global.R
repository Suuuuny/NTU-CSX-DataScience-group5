# load library
library(tibble)
library(dplyr)
library(tidyr)
library(data.table)
library(NLP)
library(tm)
library(jiebaRD)
library(jiebaR)
library(magrittr)
library(RColorBrewer)
library(wordcloud)
library(memoise)

# The list of txt file
di_month <<- list("一月" = "Di 1",
                  "二月" = "Di 2",
                  "三月" = "Di 3",
                  "四月" = "Di 4",
                  "五月" = "Di 5",
                  "六月" = "Di 6"
                  )
ko_month <<- list("一月" = "Ko 1",
                  "二月" = "Ko 2",
                  "三月" = "Ko 3",
                  "四月" = "Ko 4",
                  "五月" = "Ko 5",
                  "六月" = "Ko 6"
                  )
yao_month <<- list("一月" = "Yao 1",
                  "二月" = "Yao 2",
                  "三月" = "Yao 3",
                  "四月" = "Yao 4",
                  "五月" = "Yao 5",
                  "六月" = "Yao 6"
)
# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(book) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  if (!(book %in% books))
    stop("Unknown book")
  
  text <- readLines(sprintf("./%s.txt", book),
                    encoding="UTF-8")
  
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