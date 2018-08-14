# NTU-CSX-DataScience-group5
## 期末專題 : 選戰懶人包 - 以2018台北市長候選人為分析對象

1. [摘要](#摘要)    
2. [作品連結](#作品連結)  
3. [作品介紹](#作品介紹)   
    1 [選題動機](#選題動機)   
    2 [預期得到結果](#預期得到結果)   
    3 [資料蒐集](#資料蒐集)    
    4 [資料處理與預期呈現](#資料處理與預期呈現)   
    5 [資料呈現方式](#資料呈現方式)   
    6 [專題流程](#專題流程)   
4. [結論](#結論)   

### 一、 摘要 :<a name="摘要"></a>

依據2018年1-6月四大報新聞媒體文章、Facebook社群做文本分析與情緒分析，來探究台北市三大候選人(柯文哲、丁守中、姚文智)的關注議題、受眾(社群)有感議題、以及媒體對於這三位候選人的關注議題，與報導是否有偏頗傾向。

### 二、作品連結 :<a name="作品連結"></a>
寫作情況 : 小組完成   
1. [R shiny](https://dppss90008.shinyapps.io/news_shiny) 
2. [PPT簡報](https://docs.google.com/presentation/d/1p2vua3FNWMaGRWYAarCJ8WTwRFNiI9LmvhUbOLukDQY/edit?usp=sharing)

### 三、作品介紹 :<a name="作品介紹"></a>

#### 1. 選題動機<a name="選題動機"></a>

1. 協助公眾認識候選人所關心的議題  
2. 比較四大報如何報導台北市三位候選人  
3. 公眾在社群平台對於三位候選人的討論度  

#### 2. 預期得到結果<a name="預期得到結果"></a>

1. 從情緒分析結果來看，四大報對於各個候選人的報導是否有所偏頗？  
2. 報紙報導的候選人關注議題與候選人本身關注議題是否相關？  
3. 公眾偏好以及強烈有感的議題為何？  

#### 3. 資料蒐集<a name="資料蒐集"></a>

1. 蒐集蘋果日報、自由時報、聯合報網站 1月至6月 有關於柯文哲、丁守中、姚文智的新聞  
2. 蒐集中國時報報系1月至6月有關於柯文哲、丁守中、姚文智的平面報導  
3. 蒐集柯文哲、丁守中、姚文智從今年1月至今的臉書發文與每則發文的分享量、按讚量等資訊  

#### 4. 資料處理與預期呈現<a name="資料處理與預期呈現"></a>

1. 四大報文本 :  
﹒將1月至5月的新聞文本  
﹒做情緒分析與LDA  
﹒逐月做新聞文本的文字雲  
2. 臉書粉專文章 :  
﹒情緒分析  
﹒臉書文章LDA  
﹒逐月做文章內容文字雲  
3. 公眾想法 :  
﹒臉書文章按讚量  
﹒與網民情緒量的趨勢呈現  

#### 5. 資料呈現方式<a name="資料呈現方式"></a>

1. R shiny :   
﹒趨勢圖  
﹒盒狀圖  
﹒文字雲  
﹒長條圖  

#### 6. 專題流程<a name="專題流程"></a>

 議題設定 --> 資料收集 : 媒體與粉專資料爬蟲--> 資料清理 --> 資料分析 --> 資料呈現 : 文字雲、LDA、情緒分析

### 四、結論<a name="結論"></a>

分析完所有數值資料，用一句話形容候選人 :  
1. 柯文哲 : 媒體寵兒，有做事然後會希望讓大家看到的人，務實的事務官形象。  
2. 丁守中：存在感比較低，沒太多亮點，新聞幾乎都跟選舉連在一起，臉書發文的字中規中矩。  
3. 姚文智：力求突破，高飛理念型，希望爭取注意和支持。
