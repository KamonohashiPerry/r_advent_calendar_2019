library(RSelenium)
library(tidyverse)

# 待機時間
sleep_time <- 2

# sleep関数の指定
sleep <- function(sec){
  start <- proc.time()
  end <- proc.time()
  while((end-start)[3] < sec){ end <- proc.time() }
}

load(file = "tokyo_city_list.RData")

#ブラウザを立ち上げる
rem <- remoteDriver(port=4444L,browserName = "chrome")
rem$open()

url <- "http://www.oshimaland.co.jp"

# accident_df <- data.frame(url=NA,
#                           date=NA,
#                           address=NA,
#                           detail=NA,
#                           upload_date=NA)


for (i in 67:length(tokyo_city_list)) {
  #ブラウザで目的のページに移動
  rem$navigate(url)
  
  for (k in 1:5) {
    tryCatch({
      # 検索窓にキーワードを入力する
      element_serachbar <- rem$findElement(using = "xpath", '//*[@id="map-canvas"]/div[6]/div[1]/input[1]')
      element_serachbar$clearElement()
      element_serachbar$sendKeysToElement(list(tokyo_city_list[i]))
      sleep(sleep_time)
      
      # 検索ボタンを押す
      element_serachbar_bottom <- rem$findElement(using = "xpath", '//*[@id="map-canvas"]/div[6]/div[1]/input[2]')
      element_serachbar_bottom$clickElement()
      sleep(sleep_time)
      print("search")
      
      # 検索結果を押す
      xpath_str <- paste0('//*[@id="map-canvas"]/div[6]/div[2]/div/dl[1]/dd/table/tbody/tr[',k+1,']/td/a')
      element_serac_result_one <- rem$findElement(using = "xpath", xpath_str)
      element_serac_result_one$clickElement()
      sleep(sleep_time)
      print("click")
      
      # 地図を若干引く
      element_zoom <- rem$findElement(using = "xpath", '//*[@id="zobtn"]')
      element_zoom$clickElement()
      
      # 事故物件の星マークを取得する
      element_fires <- rem$findElements(using = "class", 'fire-image')
      sleep(sleep_time)
      print("fire")
      
      # rem$executeScript("window.scrollTo(0,100);")
      
      for (j in 1:length(element_fires)) {
        tryCatch({
          # エラーや警告が発生したときに例外処理を行いたいコード
          element_fires_each <- element_fires[[j]]
          element_fires_each$clickElement()
          print("fire click")
          sleep(sleep_time)
          # 要素の取得
          element_1 <- rem$findElement(using = "xpath", '//*[@id="property-info"]/li[1]')
          element_2 <- rem$findElement(using = "xpath", '//*[@id="property-info"]/li[2]')
          element_3 <- rem$findElement(using = "xpath", '//*[@id="property-info"]/li[3]')
          element_4 <- rem$findElement(using = "xpath", '//*[@id="property-info"]/li[4]')
          df <- data.frame(url=tokyo_city_list[i],
                           date=as.character(element_1$getElementText()),
                           address=as.character(element_2$getElementText()),
                           detail=as.character(element_3$getElementText()),
                           upload_date=as.character(element_4$getElementText())
          )
          accident_df <- rbind(accident_df, df)
          save(accident_df, file = "accident_df_from_map.RData")
          print(as.character(element_3$getElementText()))
          print(length(unique(accident_df$address)))
        }, 
        error = function(e) {print("error")},
        warning = function(e) {},
        finnaly = {print(j)
          rem$executeScript("window.scrollTo(0,150);")
          sleep(sleep_time)
          sleep(sleep_time)
          }, silent = TRUE)
      } 
    }, 
    error = function(e) {print("error")},
    warning = function(e) {},
    finnaly = {print(k)}, silent = TRUE)
    }
}




