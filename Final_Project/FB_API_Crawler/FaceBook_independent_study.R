library(dplyr)
library(NLP)
library(tidyr)
library(ggplot2)
# 丁守中 2017-12-17 - 2018-6-16
Di_report <- read.csv("Di_report.csv")


Di_data <- Di_report %>% separate(time, c("date","time"), "T")
Di_data <- Di_data %>% separate(date, c("year","month","day"), "-") 
Di_data <- Di_data[Di_data$year == "2018", ]
Di_data$month <- Di_data$month %>% as.numeric()
Di_data$day <- Di_data$day %>% as.numeric()


halfmonth <- c()
for (i in Di_data$day>15) {
  if (i==TRUE) {
    halfmonth <- c(halfmonth,0.5)
  }else{
    halfmonth <- c(halfmonth,0)
  }
}
halfmonth <- Di_data$month + halfmonth
halfmonth <- halfmonth %>% as.factor()
summary(halfmonth)
Di_data <- data.frame(Di_data,halfmonth)

Like_count = c()
for (i in levels(Di_data$halfmonth)) {
  Like_count <- c(Like_count,(Di_data[Di_data$halfmonth==i,7] %>% sum()))
}

### Share / 發文量 = CP值
share_count_cp <- Like_count / (summary(Di_data$halfmonth) %>% as.numeric())
summary(Di_data$halfmonth) %>% as.numeric()

names(Like_count_cp) <- levels(Di_data$halfmonth)
qplot(carat, price, data = Like_count_cp)

 # 姚文智 2017-11-15 - 2018-6-16
Yao_report <- read.csv("Yao_report.csv")

# 柯文哲 2017-10-22 - 2018-6-15
Ko_report <- read.csv("Ko_report.csv")

