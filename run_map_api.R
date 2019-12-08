library(config)
library(RCurl)
library(RJSONIO)

# ローカルのconfigファイルからオブジェクトの取得
conf <- config::get(config = "yahoo_map_api")

load(file = "accident_df.RData")

# 待機時間
sleep_time <- 1


# sleep関数の指定
sleep <- function(sec){
  start <- proc.time()
  end <- proc.time()
  while((end-start)[3] < sec){ end <- proc.time() }
}


# 重複削除
accident_df <- accident_df %>% distinct(url,.keep_all=TRUE)
accident_df <- accident_df %>% distinct(address,.keep_all=TRUE)

# 経度と緯度のカラム追加
accident_df <- accident_df %>% mutate(latitute="",
                                      longitude="",
                                      location_type="",
                                      formatted_address="")


# yahoo!mapのapiを使うためのURL
root <- "http://geo.search.olp.yahooapis.jp/OpenLocalPlatform/V1/geoCoder?"
root <- paste0(root,"appid=",conf$api_key ,"&output=json")


for (i in 2:nrow(accident_df)) {
  address_str <- accident_df$address[i]
  url   <- paste0(root, "&query=", address_str, sep="")
  url.u <- URLencode(iconv(url, "", "UTF-8")) # UTF-8に変換
  doc <- getURL(url.u)
  x <- fromJSON(doc, simplify = FALSE)
  
  tryCatch({
    # エラーや警告が発生したときに例外処理を行いたいコード
    accident_df$latitute[i] <- unlist(strsplit(x$Feature[[1]]$Geometry$Coordinates, ","))[2]
    accident_df$longitude[i] <- unlist(strsplit(x$Feature[[1]]$Geometry$Coordinates, ","))[1]
    accident_df$location_type[i] <- x$Feature[[1]]$Property$AddressType
    accident_df$formatted_address[i] <- x$Feature[[1]]$Property$Address
    
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
  
  sleep(sleep_time)
}

save(accident_df, file = "accident_df_with_coordinate.RData")
