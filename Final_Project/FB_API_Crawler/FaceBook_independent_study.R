library(dplyr)
library(NLP)
library(tidyr)
library(ggplot2)

# 丁守中 2017-12-17 - 2018-6-16
Di_report <- read.csv("Di_report.csv")

<<<<<<< HEAD
share_count_cp <- cbind(month, share_count_cp)
share_count_cp$month <- factor(share_count_cp$month, levels = c("一月上","一月下","二月上","二月下","三月上","三月下","四月上","四月下","五月上","五月下","六月上","六月下")) 
Di_plot <- ggplot(share_count_cp, aes( month, share )) + 
  geom_bar(stat = "identity", fill = "green3") +
  ggtitle("丁守中發文分被享比例(每半月)") 
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.05))
=======
# 建立 function 進行資料清理
DataClean <- function(DATA){
  # please input data frame
>>>>>>> dfb7de260c6da84cc23a8a787e8cdc7c0bdd774d
  
  # Cut time variable 
  DATA <- DATA %>% separate(time, c("date","time"), "T")
  DATA <- DATA %>% separate(date, c("year","month","day"), "-") 
  # Drop 2017 data
  DATA <- DATA[DATA$year == "2018", ]
  # 轉換為數值資料
  DATA$month <- DATA$month %>% as.numeric()
  DATA$day <- DATA$day %>% as.numeric()
  # Exchage month and day to halfmonth
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

CountCP <- function(DATA,index){
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

