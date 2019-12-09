# install.packages("jpmesh")
library(jpmesh)
library(sf)
library(tidyverse)
library(kokudosuuchi)

# load dataset
load(file = "accident_df_with_coordinate.RData")

# メッシュデータの追加
accident_df <- accident_df %>% mutate(k_mesh="")
accident_df <- accident_df %>% filter(latitude != "", !is.na(upload_date))

# 緯度、軽度の情報から任意のメッシュのデータを得る
for (i in 1:nrow(accident_df)) {
  accident_df$k_mesh[i] <- jpmesh::coords_to_mesh(
                                        as.numeric(accident_df$longitude[i]),
                                        as.numeric(accident_df$latitude[i]),
                                        mesh_size = "10km")
}

# メッシュを整数にする
accident_df <- accident_df %>% mutate(k_mesh=as.integer(k_mesh))

# 一都三県に絞る
accident_df <- accident_df %>% filter(stringr::str_detect(formatted_address,"東京|神奈川|埼玉|千葉"))
accident_df %>% group_by(k_mesh) %>% count() %>% arrange(desc(n))


# 国土地理院のデータを取得する
KSJlist <- getKSJSummary()
# View(getKSJURL("m1000"))

{
# 公示地価
published_land_price <- getKSJData("http://nlftp.mlit.go.jp/ksj/gml/data/L01/L01-15/L01-15_GML.zip",
                                    cache_dir = "cached_zip")

published_land_price_df <- data.frame(price=as.integer(published_land_price$`L01-15`$L01_006),
                                      address=as.character(published_land_price$`L01-15`$L01_019),
                                      area_code=as.integer(published_land_price$`L01-15`$L01_017))
published_land_price_df <- published_land_price_df %>% filter( 15000 > area_code ,area_code > 11000)

published_land_price_df$address <- gsub(published_land_price_df$address, pattern = "　",replacement =  "")
published_land_price_df$address <- gsub(published_land_price_df$address, pattern = "番",replacement =  "-")
published_land_price_df$address <- gsub(published_land_price_df$address, pattern = "外$",replacement =  "")

published_land_price_df <- published_land_price_df %>%  mutate(latitude="",
                                                               longitude="",
                                                               location_type="",
                                                               formatted_address="")


# 住所から緯度経度を求める
library(config)
library(RCurl)
library(RJSONIO)

# ローカルのconfigファイルからオブジェクトの取得
conf <- config::get(config = "yahoo_map_api")

# 待機時間
sleep_time <- 1

# sleep関数の指定
sleep <- function(sec){
  start <- proc.time()
  end <- proc.time()
  while((end-start)[3] < sec){ end <- proc.time() }
}

# yahoo!mapのapiを使うためのURL
root <- "http://geo.search.olp.yahooapis.jp/OpenLocalPlatform/V1/geoCoder?"
root <- paste0(root,"appid=",conf$api_key ,"&output=json")


for (i in 1:nrow(published_land_price_df)) {
  address_str <- published_land_price_df$address[i]
  url   <- paste0(root, "&query=", address_str, sep="")
  url.u <- URLencode(iconv(url, "", "UTF-8")) # UTF-8に変換
  doc <- getURL(url.u)
  x <- fromJSON(doc, simplify = FALSE)
  
  tryCatch({
    # エラーや警告が発生したときに例外処理を行いたいコード
    published_land_price_df$latitude[i] <- unlist(strsplit(x$Feature[[1]]$Geometry$Coordinates, ","))[2]
    published_land_price_df$longitude[i] <- unlist(strsplit(x$Feature[[1]]$Geometry$Coordinates, ","))[1]
    published_land_price_df$location_type[i] <- x$Feature[[1]]$Property$AddressType
    published_land_price_df$formatted_address[i] <- x$Feature[[1]]$Property$Address
    print(i)
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
    
  }, silent = TRUE)
  
  sleep(sleep_time)
}
}


# 推計人口
estimated_population <- getKSJData("http://nlftp.mlit.go.jp/ksj/gml/data/m1000/m1000-17/m1000-17_GML.zip",
                                   cache_dir = "cached_zip")

estimated_population_df <- data.frame(one_k_mesh=estimated_population$Mesh3_POP_00$MESH_ID,
                                       city_code=estimated_population$Mesh3_POP_00$CITY_CODE,
                                       population=estimated_population$Mesh3_POP_00$POP2010)


estimated_population_df <- estimated_population_df %>% mutate(lng_center="",
                                                              lat_center="")

# 1kmメッシュを緯度経度に戻す
for (i in 1:nrow(estimated_population_df)) {
  estimated_population_df$lng_center[i] <- jpmesh::mesh_to_coords(estimated_population_df$one_k_mesh[i])$lng_center
  estimated_population_df$lat_center[i] <- jpmesh::mesh_to_coords(estimated_population_df$one_k_mesh[i])$lat_center
}


estimated_population_df <- estimated_population_df %>% mutate(five_k_mesh="")

# 緯度経度を5kmメッシュにする
for (i in 1:nrow(estimated_population_df)) {
  estimated_population_df$five_k_mesh[i] <- jpmesh::coords_to_mesh(
    as.numeric(estimated_population_df$lng_center[i]),
    as.numeric(estimated_population_df$lat_center[i]),
    mesh_size = "10km")
}

estimated_population_df <- estimated_population_df %>% 
                              mutate(five_k_mesh=as.integer(five_k_mesh))

estimated_population_df_distinct <- estimated_population_df %>% distinct(five_k_mesh,.keep_all=TRUE)


accident_summary <- accident_df %>% group_by(k_mesh) %>% summarise(accident_count=n()) %>% arrange(desc(accident_count))

estimated_population_df_distinct <- estimated_population_df_distinct %>% 
                                        left_join(accident_summary,
                                                  by = c("five_k_mesh"="k_mesh"))
estimated_population_df_distinct$accident_count <- replace_na(estimated_population_df_distinct$accident_count, 0)

estimated_population_df_distinct <- estimated_population_df_distinct %>% mutate(accident_percapita=accident_count/population)


