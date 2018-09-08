library(magrittr)
# load data
di <- read.csv("Di")
ko <- read.csv("Ko")
yao <- read.csv("Yao")
# bind all data together 
all <- rbind(di, ko, yao)
# delete Na data
all <- all %>% na.omit()
# remove duplicated data
all <- all[!duplicated(all$bindtext), ]
# sort data by candi and time
all <- all[with(all, order(new, news_month)), ]
# re-assign new rowname
row.names(all) = c(1:nrow(all))

# save the new data
write.table(all, file = "C:/Users/Weber/Documents/GitHub/NTU-CSX-DataScience-group5/Final_Project/Wordcloud/wordcloud_ltn-shiny/ltn_news_cleaning", sep = ",")

