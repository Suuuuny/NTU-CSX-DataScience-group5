library(magrittr)
# load data
Yao <- read.csv("Yao_Appnews.csv")
Ko <- read.csv("Ko_Appnews.csv")
Di <- read.csv("Ding_Appnews.csv")

# assign new id to 3 candi
new_id_var <- function(Data){
  new <- rep(deparse(substitute(Data)),nrow(Data))
  Data <- cbind(Data, new)
}
Ko<-new_id_var(Ko)
Yao<-new_id_var(Yao)
Di<-new_id_var(Di)

# bind all data together 
all <- rbind(Di, Ko, Yao)
# delete Na data
all <- all %>% na.omit()
# remove duplicated data
all <- all[!duplicated(all$content), ]
# sort data by candi and time
all <- all[with(all, order(new, year ,month, day)), ]
# re-assign new rowname
row.names(all) = c(1:nrow(all))

# save the new data
write.table(all, file = "C:/Users/Weber/Documents/GitHub/NTU-CSX-DataScience-group5/Final_Project/Wordcloud/wordcloud_ap-shiny/Apple_news_cleaning.csv", sep = ",")

