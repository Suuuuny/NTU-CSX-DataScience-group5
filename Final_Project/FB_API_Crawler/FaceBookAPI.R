#### 引用套件 ####
library(jsonlite)
library(httr)
library(magrittr)
library(dplyr)

####六都候選人臉書粉絲專頁####

##台北市##
#柯文哲https://www.facebook.com/DoctorKoWJ/
#姚文智https://www.facebook.com/YaoTurningTaipei/
#丁守中https://www.facebook.com/tingshouchung/

##測試使用##
#enc.teia

##########FacebookAPI##########

token = "EAACEdEose0cBAJ1hM3pZBh4rZB3uuGThmv3dnmhu5ZAUrDE1D9ECA0kV2tOIy73PIu82toszGIq0yXAwVZAbs3ErXLZCmzeSCf7w89sqZCSehXH2cOgpqyaL0kodS6ZCHUje0s3Pq7fSZAv5vVEd1D46ikhIvTJy9ktwD8yOz4WZAwiLccfNyZBSyldQWGs3U5bWoZD"
FacebookID = "YaoTurningTaipei"
## 注意 : limit請設定25的倍數
limit <- 300


##################################################################
###### Crawl posts data <posts內容、分享、按讚> ##################
##################################################################

# Crawl Posts data from facebookAPI 已完成開發
# 程式碼說明: 此Function可以輸入粉絲專頁面帳號、要爬幾篇、token。取得posts
# 包成Getpost function

GetPost <- function(FacebookID,limit,token){
  url_1 = "https://graph.facebook.com/v3.0/"
  url_2 = "?fields=posts.limit("
  url_3 = ")&access_token="
  url = paste0(url_1,FacebookID,url_2,limit,url_3,token)
  response = GET(url)
  Posts  = content(response)
  
  # Get posts/message from post <List>
  Posts <- Posts$posts$data
  
  # Get post data in data.frame -> post_data
  post_data <- data.frame()
  time <- sapply(Posts,function(data){
    return(data$created_time)
  })
  message <- sapply(Posts,function(data){
    return(data$message %>% as.character())
  }) %>% unlist
  post_data <- cbind(time,message)
  return(post_data)
}

# 執行程式碼
Post_data <- GetPost(FacebookID,limit,token)



##################################################################

# get shares from every post 已完成開發
# 程式碼說明: 取得每篇文章shares數
# 包成function GetShare

GetShare <- function(FacebookID,limit,token){
  url_1 = "https://graph.facebook.com/v3.0/"
  url_2 = "?fields=posts.limit("
  url_3 = "){shares}&access_token="
  url = paste0(url_1,FacebookID,url_2,limit,url_3,token)
  response = GET(url)
  shares  = content(response)
  shares <- shares$posts$data
  
  shareCT <- c()
  for(i in c(1:limit)){
    shareCT <- c(shareCT,shares[[i]]$shares[[1]])
  }
  return(shareCT)
}

# 執行程式碼
Share_data <- GetShare(FacebookID,limit,token)

###################################################################

# get 讚!!like,love,wow,haha,sad,angry,thankful 已完成開發
# 程式碼說明: 取得每篇文章情緒符號

Getmood <- function(FacebookID,limit,token){
  url_1 = "https://graph.facebook.com/v3.0/"
  url_2 = "?fields=%20%20%20posts.as(like)%7Breactions.type(LIKE).limit(0).summary(true)%7D%2C%20%20%20posts.as(love)%7Breactions.type(LOVE).limit(0).summary(true)%7D%2C%20%20%20posts.as(wow)%7Breactions.type(WOW).limit(0).summary(true)%7D%2C%20%20%20posts.as(haha)%7Breactions.type(HAHA).limit(0).summary(true)%7D%2C%20%20%20posts.as(sad)%7Breactions.type(SAD).limit(0).summary(true)%7D%2C%20%20%20posts.as(angry)%7Breactions.type(ANGRY).limit(0).summary(true)%7D%2C%20%20%20posts.as(thankful)%7Breactions.type(THANKFUL).limit(0).summary(true)%7D&access_token="
  url = paste0(url_1,FacebookID,url_2,token)
  
  retext <- fromJSON(content(GET(url), "text"))
  
  # Get first page mood count
  like_temp <- (retext$like$data$reactions$summary %>% data.frame())$total_count
  love_temp <- (retext$love$data$reactions$summary %>% data.frame())$total_count
  haha_temp <- (retext$haha$data$reactions$summary %>% data.frame())$total_count
  sad_temp <- (retext$sad$data$reactions$summary %>% data.frame())$total_count
  wow_temp <- (retext$wow$data$reactions$summary %>% data.frame())$total_count
  angry_temp <- (retext$angry$data$reactions$summary %>% data.frame())$total_count
  
  mood_res <- cbind(like_temp,love_temp,haha_temp,sad_temp,wow_temp,angry_temp) %>% data.frame()
  
  
  # Jump to nxt page
  
  next_likeurl <- retext$like$paging$"next"
  next_loveurl <- retext$love$paging$"next"
  next_wowurl  <- retext$wow$paging$"next"
  next_hahaurl <- retext$haha$paging$"next"
  next_sadurl <- retext$sad$paging$"next"
  next_angrurl <- retext$angry$paging$"next"
  
  temp_limit <- limit
  limit <- (limit-25)/25
  library(tcltk) # 進度條
  u <- 1:limit
  pb <- tkProgressBar("進度","已完成 %", 0, 100) 
  
  for( i in 1:limit) {
    info<- sprintf("已完成 %d%%", round(i*100/length(u)))  
    setTkProgressBar(pb, i*100/length(u), sprintf("進度 (%s)", info),info)
    liketext <- fromJSON(content(GET(next_likeurl), "text"))
    lovetext <- fromJSON(content(GET(next_loveurl), "text"))
    wowtext <- fromJSON(content(GET(next_wowurl), "text"))
    hahatext <- fromJSON(content(GET(next_hahaurl), "text"))
    sadtext <- fromJSON(content(GET(next_sadurl), "text"))
    angrtext <- fromJSON(content(GET(next_angrurl), "text"))
      
    like_data <- (liketext$data$reactions$summary %>% data.frame())$total_count
    love_data <- (lovetext$data$reactions$summary %>% data.frame())$total_count
    wow_data  <- (wowtext$data$reactions$summary %>% data.frame())$total_count
    haha_data <- (hahatext$data$reactions$summary %>% data.frame())$total_count
    sad_data  <- (sadtext$data$reactions$summary %>% data.frame())$total_count
    angr_data <- (angrtext$data$reactions$summary %>% data.frame())$total_count
    
    like_temp <- c(like_temp,like_data)
    love_temp <- c(love_temp,love_data)
    wow_temp <- c(wow_temp,wow_data)
    haha_temp <- c(haha_temp,haha_data)
    sad_temp <- c(sad_temp,sad_data)
    angry_temp <- c(angry_temp,angr_data)
    
    
    
    next_likeurl <- liketext$paging$"next"
    next_loveurl <- lovetext$paging$"next"
    next_wowurl  <- wowtext$paging$"next"
    next_hahaurl <- hahatext$paging$"next"
    next_sadurl <-  sadtext$paging$"next"
    next_angrurl <- angrtext$paging$"next"
      
    
  }
  close(pb)  
  mood_res <- cbind(like_temp,love_temp,haha_temp,sad_temp,wow_temp,angry_temp) %>% data.frame()
  return(mood_res[1:temp_limit,])
}

# 執行程式碼
Mood_data <- Getmood(FacebookID,limit,token)


###################################################################

## 結果整合到 Report <data.frame>並且輸出

 Report <- cbind(Post_data,Share_data,Mood_data)
 colnames(Report) <- c("time","post","share","like","love","haha","sad","wow","angry")
 write.csv(Report,file="report.csv")

 data <- read.csv("report.csv")
 
###################################################################

#### 結果 ####
# 丁守中 爬文結果
 Di_report <- read.csv("Di_report.csv")
# 姚文智 爬文結果
 Yao_report <- read.csv("Yao_report.csv")
# 柯文哲 爬文結果
 Kao_report <- read.csv("Ko_report.csv")

#####################################################################
 
## 後續資料清理 在Facebook_independent_study.R