# === this is a script for constructing a dtm file
# load package
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
# set dir
setwd("~/GitHub/NTU-CSX-DataScience-group5/Final_Project/Wordcloud/all")
# load data
data = read.csv("all.csv")

nrow(data)-sum(is.na(data))
