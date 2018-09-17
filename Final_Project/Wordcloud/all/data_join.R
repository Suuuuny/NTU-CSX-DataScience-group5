# ==== 
# this script is aim to return a well-cut DTM file for those txt file

# load the required package
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

# setwd()
setwd("~/GitHub/NTU-CSX-DataScience-group5/Final_Project/Wordcloud/all")

# == load dataset
df_apple = read.csv('Apple_news_cleaning.csv',sep = ',')
df_ct = read.csv('CT_news_cleaning.csv',sep = ',')
df_udn = read.csv('Udn_news_cleaning.csv',sep = ',')
df_ltn = read.csv('ltn_news_cleaning',sep = ',')
df_fb = read.csv('FaceBookAPI-Taipei_new_edit.csv',sep = ',')

# == merge dataset together
# I would like to create a new frame for all data,
# with the media, candi, month, title and content as columns contained
name = c('media', 'candi', 'month', 'title', 'content')

# apple
df_apl = subset(df_apple, select = c('Media','new','month','title','content'))
names(df_apl) <- name

# ct
df_ct = subset(df_ct, select = c('media','new','month','title','content'))
names(df_ct) <- name

# udn
media = rep('udn',nrow(df_udn))
df_udn = cbind(df_udn, media)
df_udn = subset(df_udn, select = c('media','new','month','V2','V3'))
names(df_udn) <- name

# ltn
df_ltn = subset(df_ltn, select = c('Media','new','news_month','V1','bindtext'))
names(df_ltn) <- name

# fb
fb = rep('fb', nrow(df_fb))
df_fb = cbind(df_fb, fb)
df_fb = subset(df_fb, select = c('fb','name','month','post'))
names(df_fb) <- c('media', 'candi', 'month',  'content')

sum(is.na(df_fb))
df_fb[duplicated(df_fb),]
# join all together
# one problem is that df_fb doesn't get a title column 
# so we decide to drop all title var and then join
df_all = data.frame()
df_all = rbind(df_apl, df_ct, df_ltn, df_udn)
# see if there is any prob
str(df_all)
# unique(df_all$media)
# lower case
for(i in nrow(df_all)){
  df_all$media =  df_all$media %>% tolower()
  }
# clean the data
# if there any na-value
sum(is.na(df_all))
# dup-value
df_all[duplicated(df_all), ]

# save file
write.table(df_all, file = "C:/Users/Weber/Documents/GitHub/NTU-CSX-DataScience-group5/Final_Project/Wordcloud/all/all_news.csv")
write.table(df_fb, file = "C:/Users/Weber/Documents/GitHub/NTU-CSX-DataScience-group5/Final_Project/Wordcloud/all/all_fb.csv")





