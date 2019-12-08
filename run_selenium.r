library(RSelenium)
library(tidyverse)

# ran_tweet_collectで取得したデータの読み込み
load(file = "ut_df.RData")
# 待機時間
sleep_time <- 10


# sleep関数の指定
sleep <- function(sec){
  start <- proc.time()
  end <- proc.time()
  while((end-start)[3] < sec){ end <- proc.time() }
}


# 大島てるのサイトへのリンクだけに絞り込む
ut_df <- ut_df %>% filter(stringr::str_detect(url,"oshimaland.co.jp"))

# 重複削除
ut_df <- ut_df %>% distinct(id,.keep_all=TRUE)
ut_df <- ut_df %>% distinct(url,.keep_all=TRUE)

#ブラウザを立ち上げる
rem <- remoteDriver(port=4444L,browserName = "chrome")
rem$open()

accident_df <- data.frame(url=NA,
                          date=NA,
                          address=NA,
                          detail=NA,
                          upload_date=NA)

for (i in 1:nrow(ut_df)) {
  url_str <- paste0("http://www.", ut_df$url[i])
  
  #ブラウザで目的のページに移動
  rem$navigate(url_str)
  sleep(sleep_time)
  
  
  tryCatch({
    # エラーや警告が発生したときに例外処理を行いたいコード
    # 要素の取得
    element_1 <- rem$findElement(using = "xpath", '//*[@id="property-info"]/li[1]')
    element_2 <- rem$findElement(using = "xpath", '//*[@id="property-info"]/li[2]')
    element_3 <- rem$findElement(using = "xpath", '//*[@id="property-info"]/li[3]')
    element_4 <- rem$findElement(using = "xpath", '//*[@id="property-info"]/li[4]')
    
    df <- data.frame(url=url_str,
                     date=as.character(element_1$getElementText()),
                     address=as.character(element_2$getElementText()),
                     detail=as.character(element_3$getElementText()),
                     upload_date=as.character(element_4$getElementText())
    )
    
    accident_df <- rbind(accident_df, df)
    }, 
    error = function(e) {
      #エラーが発生した時の処理
      print("error")
      },
    warning = function(e) {
      #警告が発生した時の処理
    },
    finnaly = {
    #ここに記載したコードは必ず実行される
      print(url_str)
    }, silent = TRUE)
}

save(accident_df, file = "accident_df.RData")
