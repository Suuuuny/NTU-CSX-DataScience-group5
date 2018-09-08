library(magrittr)
library(tidyr)

# load data
yao <- read.csv("Yao.csv")
ko <- read.csv("Ko.csv")
di <- read.csv("Di.csv")

# subset
yao <- subset(yao, select = c(month, date, V2, V3, new))
ko <- subset(ko, select = c(month, date, V1, bindtext, new))
di <- subset(di, select = c(month ,date, V2, V3, new))

names(ko) <- c('month' ,'date', 'V2', 'V3', 'new')


# bind all data together 
all <- rbind(di, ko, yao)
# delete Na data
all <- all %>% na.omit()
# remove duplicated data
# all <- all[!duplicated(all$V2), ]
# sort data by candi and time
all <- all[with(all, order(new,month, date)), ]
# re-assign new rowname
row.names(all) = c(1:nrow(all))

# save the new data
write.table(all, file = "C:/Users/Weber/Documents/GitHub/NTU-CSX-DataScience-group5/Final_Project/Wordcloud/wordcloud_udn-shiny/Udn_news_cleaning.csv", sep = ",")

