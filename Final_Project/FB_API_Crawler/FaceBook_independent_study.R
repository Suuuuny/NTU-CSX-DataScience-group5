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
Ko_report <- read.csv("Ko_report.csv")

# 建立 function 進行資料清理
DataClean <- function(DATA){
  # please input data.frame and this function will output data.frame

  # Cut time variable 
  time2 <- DATA$time
  DATA <- cbind(DATA,time2)


  
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
Ko_report <- DataClean(Ko_report)

############## 整理成 Final table
name <- c("x","year","month","day","time","post","share","like","love","haha","sad","wow","angry","time2","halfmonth","name","Candidate")
Di_report <- cbind(Di_report,rep("Di",times=265),rep("丁守中",times=265))
colnames(Di_report) <- name
Yao_report <- cbind(Yao_report,rep("Yao",times=243),rep("姚文智",times=243))
colnames(Yao_report) <- name
Ko_report <- cbind(Ko_report,rep("Ko",times=138),rep("柯文哲",times=138))
colnames(Ko_report) <- name

Facebook_report <- rbind(Di_report,Yao_report,Ko_report)


############### Facebook 爬文結果!!!!!!!
write.csv(Facebook_report,"FaceBook_report.csv")






## 畫圖 丁守中 柯文哲 姚文智 每半個月發文量
Di = summary(Di_report$halfmonth) %>% as.numeric()
Yao = summary(Yao_report$halfmonth) %>% as.numeric()
Ko = summary(Ko_report$halfmonth) %>% as.numeric()

report_sum = data.frame()

halfmonth = c("一月上","一月下","二月上","二月下","三月上","三月下","四月上","四月下","五月上","五月下","六月上")
report_Di = cbind(Di[1:11],rep("丁守中",time(11)))
report_Ko = cbind(Ko[1:11],rep("柯文哲",time(11)))
report_Yao = cbind(Yao[1:11],rep("姚文智",time(11)))
report_sum <- rbind(report_Di,report_Ko,report_Yao) %>% as.data.frame()
report_sum <- report_sum %>% cbind(.,c(halfmonth,halfmonth,halfmonth))


colnames(report_sum) <- c("post","name","halfmonth")
report_sum$post <- as.numeric(as.character(report_sum$post))
report_sum$halfmonth <- report_sum$halfmonth %>% factor(.,c("一月上","一月下","二月上","二月下","三月上","三月下","四月上","四月下","五月上","五月下","六月上"))


ggplot(report_sum, aes(x = halfmonth, y = post, colour=name,group=name))+ geom_point(size = 4)+ geom_line() +  scale_color_manual(values=c("blue", "green", "black"))+xlab("time") +scale_fill_discrete(labels=c("丁守中","V","C"))






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

Ko_count_share_cp <- CountCP(Ko_report,7)
Ko_count_like_cp <- CountCP(Ko_report,8)

Ko_share_plot <- ggplot(Ko_count_share_cp, aes(x=month, y=CP)) + 
  geom_bar(stat = "identity", fill = "green3") +
  ggtitle("丁守中發文被分享比例(每半月)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.05))

Ko_like_plot <- ggplot(Ko_count_like_cp, aes(x=month, y=CP)) + 
  geom_bar(stat = "identity", fill = "green3") +
  ggtitle("丁守中發文被按讚比例(每半月)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.05))

