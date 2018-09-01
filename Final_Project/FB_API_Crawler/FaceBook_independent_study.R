library(dplyr)
library(NLP)
library(tidyr)
library(ggplot2)

# 丁守中 2017-12-17 - 2018-6-16
Di_report <- read.csv("Di_report.csv")
# 切開time變數
Di_data <- Di_report %>% separate(time, c("date","time"), "T")
Di_data <- Di_data %>% separate(date, c("year","month","day"), "-") 
# 去除2017資料
Di_data <- Di_data[Di_data$year == "2018", ]
# 轉會為數值資料
Di_data$month <- Di_data$month %>% as.numeric()
Di_data$day <- Di_data$day %>% as.numeric()
# 建立半月變數
halfmonth <- c()
for(i in Di_data$day>15) {
  if (i==TRUE) {
    halfmonth <- c(halfmonth,0.5)
  }else{
    halfmonth <- c(halfmonth,0)
  }
}
# 半月變數加上原始月份，指定回資料表
halfmonth <- Di_data$month + halfmonth
halfmonth <- halfmonth %>% as.factor()
summary(halfmonth)
Di_data <- data.frame(Di_data,halfmonth)

share_count = c()
for (i in levels(Di_data$halfmonth)) {
  share_count <- c(share_count,(Di_data[Di_data$halfmonth==i,7] %>% sum()))
}
### Share / 發文量 = CP值
share_count_cp <- share_count / (summary(Di_data$halfmonth) %>% as.numeric())
summary(Di_data$halfmonth) %>% as.numeric()
# 視覺化
month <- c("一月上","一月下","二月上","二月下","三月上","三月下","四月上","四月下","五月上","五月下","六月上","六月下")

names(share_count_cp) <- levels(Di_data$halfmonth)
share_count_cp <- share_count_cp %>% as.data.frame()
colnames(share_count_cp) <- "share" 

share_count_cp <- cbind(month, share_count_cp)
share_count_cp$month <- factor(share_count_cp$month, levels = c("一月上","一月下","二月上","二月下","三月上","三月下","四月上","四月下","五月上","五月下","六月上","六月下")) 
Di_plot <- ggplot(share_count_cp, aes( month, share )) + 
  geom_bar(stat = "identity", fill = "green3") +
  ggtitle("丁守中發文分被享比例(每半月)") 
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.05))
  


 # 姚文智 2017-11-15 - 2018-6-16
Yao_report <- read.csv("Yao_report.csv")
# 切開time變數
Yao_data <- Yao_report %>% separate(time, c("date","time"), "T")
Yao_data <- Yao_data %>% separate(date, c("year","month","day"), "-") 
# 去除2017資料
Yao_data <- Yao_data[Yao_data$year == "2018", ]
# 轉會為數值資料
Yao_data$month <- Yao_data$month %>% as.numeric()
Yao_data$day <- Yao_data$day %>% as.numeric()
# 建立半月變數
halfmonth <- c()
for(i in Yao_data$day>15) {
  if (i==TRUE) {
    halfmonth <- c(halfmonth,0.5)
  }else{
    halfmonth <- c(halfmonth,0)
  }
}
# 半月變數加上原始月份，指定回資料表
halfmonth <- Yao_data$month + halfmonth
halfmonth <- halfmonth %>% as.factor()
summary(halfmonth)
Yao_data <- data.frame(Yao_data,halfmonth)

share_count = c()
for (i in levels(Yao_data$halfmonth)) {
  share_count <- c(share_count,(Yao_data[Yao_data$halfmonth==i,7] %>% sum()))
}
### Share / 發文量 = CP值
share_count_cp <- share_count / (summary(Yao_data$halfmonth) %>% as.numeric())
summary(Yao_data$halfmonth) %>% as.numeric()




# 柯文哲 2017-10-22 - 2018-6-15
Ko_report <- read.csv("Ko_report.csv")
# 切開time變數
Ko_data <- Ko_report %>% separate(time, c("date","time"), "T")
Ko_data <- Ko_data %>% separate(date, c("year","month","day"), "-") 
# 去除2017資料
Ko_data <- Ko_data[Ko_data$year == "2018", ]
# 轉會為數值資料
Ko_data$month <- Ko_data$month %>% as.numeric()
Ko_data$day <- Ko_data$day %>% as.numeric()
# 建立半月變數
halfmonth <- c()
for(i in Ko_data$day>15) {
  if (i==TRUE) {
    halfmonth <- c(halfmonth,0.5)
  }else{
    halfmonth <- c(halfmonth,0)
  }
}
# 半月變數加上原始月份，指定回資料表
halfmonth <- Ko_data$month + halfmonth
halfmonth <- halfmonth %>% as.factor()
summary(halfmonth)
Ko_data <- data.frame(Ko_data,halfmonth)

share_count = c()
for (i in levels(Ko_data$halfmonth)) {
  share_count <- c(share_count,(Ko_data[Ko_data$halfmonth==i,7] %>% sum()))
}
### Share / 發文量 = CP值
share_count_cp <- share_count / (summary(Ko_data$halfmonth) %>% as.numeric())
summary(Ko_data$halfmonth) %>% as.numeric()

# 視覺化
names(share_count_cp) <- levels(Di_data$halfmonth)

