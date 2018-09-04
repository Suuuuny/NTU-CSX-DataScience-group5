library(dplyr)
library(magrittr)
library(NLP)
library(tidyr)
library(ggplot2)

# 丁守中 2017-12-17 - 2018-6-16
Di_report <- read.csv("Di_report.csv")
# 姚文智　
Yao_report <- read.csv("Yao_report.csv")
# 柯文哲
Kao_report <- read.csv("Ko_report.csv")

# 建立 function 進行資料清理
DataClean <- function(DATA){
  # please input data.frame and this function will output data.frame

  # Cut time variable 
  DATA <- DATA %>% separate(time, c("date","time"), "T")
  DATA <- DATA %>% separate(date, c("year","month","day"), "-") 
  # Drop 2017 data
  DATA <- DATA[DATA$year == "2018", ]
  # 轉換為數值資料
  DATA$month <- DATA$month %>% as.numeric()
  DATA$day <- DATA$day %>% as.numeric()
  # Exchage month and day to halfmonth (以15天為間距)
  halfmonth <- c()
  for(i in DATA$day>15) {
    if (i==TRUE) {
      halfmonth <- c(halfmonth,0.5)
    }else{
      halfmonth <- c(halfmonth,0)
    }
  }
  # 半月變數加上原始月份，指定回資料表
  halfmonth <- DATA$month + halfmonth
  halfmonth <- halfmonth %>% as.factor()
  # 把半月變數資料放回去data frame
  DATA <- cbind(DATA,halfmonth)
  # 清除6月半資料
  DATA <- DATA[DATA$halfmonth!=6.5,]
 
  return(DATA)
}

Di_report <- DataClean(Di_report)
Yao_report <- DataClean(Yao_report)
Kao_report <- DataClean(Kao_report)




CountCP <- function(DATA,index){
  # 功能：使用每半個月為間距，計算喜好數量／發文數量＝ＣＰ值
  # index = 7 : share, 8 : like 
  count = c()
  for (i in levels(DATA$halfmonth)[1:11]) {
    count <- c(count,(DATA[DATA$halfmonth==i,index] %>% sum()))
  }
  count_cp <- count / (summary(DATA$halfmonth)[1:11] %>% as.numeric())
  month <- c("一月上","一月下","二月上","二月下","三月上","三月下","四月上","四月下","五月上","五月下","六月上") 
  count_cp <- count_cp %>% as.data.frame()
  count_cp <- cbind(month,count_cp)
  count_cp$month <- count_cp$month %>% factor(.,levels = c("一月上","一月下","二月上","二月下","三月上","三月下","四月上","四月下","五月上","五月下","六月上"))
  colnames(count_cp) <- c("month","CP")
  return(count_cp)
}

Di_count_share_cp <- CountCP(Di_report,7)
Di_count_like_cp <- CountCP(Di_report,8)

Di_share_plot <- ggplot(Di_count_share_cp, aes(x=month, y=CP)) + 
  geom_bar(stat = "identity", fill = "green3") +
  ggtitle("丁守中發文被分享比例(每半月)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.05))

Di_like_plot <- ggplot(Di_count_like_cp, aes(x=month, y=CP)) + 
  geom_bar(stat = "identity", fill = "green3") +
  ggtitle("丁守中發文被按讚比例(每半月)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.05))

########################################################

Yao_count_share_cp <- CountCP(Yao_report,7)
Yao_count_like_cp <- CountCP(Yao_report,8)

Yao_share_plot <- ggplot(Yao_count_share_cp, aes(x=month, y=CP)) + 
  geom_bar(stat = "identity", fill = "green3") +
  ggtitle("姚文智發文被分享比例(每半月)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.05))

Yao_like_plot <- ggplot(Yao_count_like_cp, aes(x=month, y=CP)) + 
  geom_bar(stat = "identity", fill = "green3") +
  ggtitle("姚文智發文被按讚比例(每半月)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.05))
##########################################################

Kao_count_share_cp <- CountCP(Kao_report,7)
Kao_count_like_cp <- CountCP(Kao_report,8)

Kao_share_plot <- ggplot(Kao_count_share_cp, aes(x=month, y=CP)) + 
  geom_bar(stat = "identity", fill = "green3") +
  ggtitle("丁守中發文被分享比例(每半月)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.05))

Kao_like_plot <- ggplot(Kao_count_like_cp, aes(x=month, y=CP)) + 
  geom_bar(stat = "identity", fill = "green3") +
  ggtitle("丁守中發文被按讚比例(每半月)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.05))

