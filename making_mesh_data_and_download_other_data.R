# install.packages("jpmesh")
library(jpmesh)
library(sf)
library(tidyverse)
library(kokudosuuchi)
library(jpndistrict)
library(mapview)

# load dataset
# load(file = "accident_df_with_coordinate.RData")
load(file = "accident_df_with_coordinate_from_map.RData")

# メッシュデータの追加
accident_df <- accident_df %>% mutate(k_mesh="")
accident_df <- accident_df %>% filter(latitude != "", !is.na(upload_date))

# 緯度、軽度の情報から任意のメッシュのデータを得る
for (i in 1:nrow(accident_df)) {
  accident_df$k_mesh[i] <- jpmesh::coords_to_mesh(
                                        as.numeric(accident_df$longitude[i]),
                                        as.numeric(accident_df$latitude[i]),
                                        mesh_size = "1km")
}

# メッシュを整数にする
accident_df <- accident_df %>% mutate(k_mesh=as.integer(k_mesh))

# 東京都に絞る
accident_df <- accident_df %>% filter(stringr::str_detect(formatted_address,"東京"))
accident_df %>% group_by(k_mesh) %>% count() %>% arrange(desc(n))

# accident_df <- rbind(accident_df,accident_df_1)


# 国土地理院のデータを取得する
KSJlist <- getKSJSummary()
# View(getKSJURL("m1000"))

# 公示地価
published_land_price <- getKSJData("http://nlftp.mlit.go.jp/ksj/gml/data/L01/L01-15/L01-15_GML.zip",
                                    cache_dir = "cached_zip")

published_land_price_df <- data.frame(price=as.integer(published_land_price$`L01-15`$L01_006),
                                      address=as.character(published_land_price$`L01-15`$L01_019),
                                      area_code=as.integer(published_land_price$`L01-15`$L01_017))

for (i in 1:nrow(published_land_price_df)) {
  published_land_price_df$latitude[i] <- as.character(published_land_price$`L01-15`$geometry[[i]][2])
  published_land_price_df$longitude[i] <- as.character(published_land_price$`L01-15`$geometry[[i]][1])
}

# 東京の地価に絞り込む
published_land_price_df_tokyo <- published_land_price_df %>% filter(stringr::str_detect(address,"東京"),
                                                                    stringr::str_detect(address,"区"))
published_land_price_df_tokyo$address_fixed <- gsub(pattern = "　", replacement = "", published_land_price_df_tokyo$address)
published_land_price_df_tokyo$address_fixed <- stringi::stri_trans_nfkc(published_land_price_df_tokyo$address_fixed)

for (i in 1:nrow(published_land_price_df_tokyo)) {
  published_land_price_df_tokyo$address_fixed[i] <- strsplit(published_land_price_df_tokyo$address_fixed,split="[0-9]")[[i]][1]
}

tokyo_city_list <- unique(published_land_price_df_tokyo$address_fixed)

save(tokyo_city_list, file = "tokyo_city_list.RData")

# published_land_price_df <- published_land_price_df %>% filter( 15000 > area_code ,area_code > 11000)


# 緯度経度を1kmメッシュにする
for (i in 1:nrow(published_land_price_df)) {
  published_land_price_df$mesh[i] <- jpmesh::coords_to_mesh(
    as.numeric(published_land_price_df$longitude[i]),
    as.numeric(published_land_price_df$latitude[i]),
    mesh_size = "1km")
}

published_land_price_df <- published_land_price_df %>% mutate(mesh=as.integer(mesh))

published_land_price_df_summary <- published_land_price_df %>% group_by(mesh) %>% summarise(mean_price=mean(price))


# 推計人口
estimated_population <- getKSJData("http://nlftp.mlit.go.jp/ksj/gml/data/m1000/m1000-17/m1000-17_GML.zip",
                                   cache_dir = "cached_zip")

estimated_population_df <- data.frame(one_k_mesh=estimated_population$Mesh3_POP_00$MESH_ID,
                                       city_code=estimated_population$Mesh3_POP_00$CITY_CODE,
                                       population=estimated_population$Mesh3_POP_00$POP2010)


# estimated_population_df <- estimated_population_df %>% mutate(lng_center="",
#                                                               lat_center="")
# 
# # 1kmメッシュを緯度経度に戻す
# for (i in 1:nrow(estimated_population_df)) {
#   estimated_population_df$lng_center[i] <- jpmesh::mesh_to_coords(estimated_population_df$one_k_mesh[i])$lng_center
#   estimated_population_df$lat_center[i] <- jpmesh::mesh_to_coords(estimated_population_df$one_k_mesh[i])$lat_center
# }
# 
# 
# estimated_population_df <- estimated_population_df %>% mutate(five_k_mesh="")

# # 緯度経度を10kmメッシュにする
# for (i in 1:nrow(estimated_population_df)) {
#   estimated_population_df$five_k_mesh[i] <- jpmesh::coords_to_mesh(
#     as.numeric(estimated_population_df$lng_center[i]),
#     as.numeric(estimated_population_df$lat_center[i]),
#     mesh_size = "10km")
# }


# estimated_population_df <- estimated_population_df %>% 
#                               mutate(five_k_mesh=as.integer(five_k_mesh))
estimated_population_df <- estimated_population_df %>% 
                                mutate(one_k_mesh=as.integer(one_k_mesh))

# estimated_population_df_distinct <- estimated_population_df %>% distinct(five_k_mesh,.keep_all=TRUE)


accident_summary <- accident_df %>%
                        group_by(k_mesh) %>%
                        summarise(accident_count=n()) %>% 
                        arrange(desc(accident_count))

# estimated_population_df_distinct <- estimated_population_df_distinct %>% 
#                                         left_join(accident_summary,
#                                                   by = c("five_k_mesh"="k_mesh"))
estimated_population_df <- estimated_population_df %>% 
                                        left_join(accident_summary,
                                                  by = c("one_k_mesh"="k_mesh"))

estimated_population_df$accident_count <- replace_na(estimated_population_df$accident_count, 0)

estimated_population_df <- estimated_population_df %>% mutate(accident_percapita=accident_count/population)

estimated_population_df <- estimated_population_df %>% 
                              left_join(published_land_price_df_summary,
                                        by=c("one_k_mesh"="mesh"))

# save(estimated_population_df, file = "estimated_population_df_from_map_2.RData")

load(file = "estimated_population_df_from_map_2.RData")

estimated_population_df <- estimated_population_df %>% 
                                mutate(accident_ratio_class=if_else(estimated_population_df$accident_percapita > 0.05, "0.05以上",
                                                                    if_else(estimated_population_df$accident_percapita > 0.01, "0.01以上","0.01未満")))
estimated_population_df <- estimated_population_df %>% mutate(accident_ratio = round(estimated_population_df$accident_percapita*100,2)  )


# df <- estimated_population_df %>% group_by(city_code) %>% summarise(accident_count=sum(accident_count),
#                                                                              population=sum(population))


library(jpndistrict)

# sf_pref13 <- jpn_pref(pref_code = 13, district = TRUE) %>% 
#                 mutate(city_code=as.integer(city_code))
# 
# a <- sf_pref13 %>% 
#       left_join(df, by = "city_code") %>% 
#       filter(city_code >= 13000, city_code < 13300)
# 
# a$accident_count <- replace_na(data = a$accident_count,0)
# a$population <- replace_na(data = a$population,100)
# 
# a <- a %>% mutate(percapita_accident=round(accident_count/population*100,2))
# 
# ggplot(a) +
#   geom_sf(data = a, aes(fill=percapita_accident))
# 
# 
# ggplot(a) +
#   geom_sf(data = a, aes(fill=accident_count))


# estimated_population_df <- estimated_population_df %>% mutate(lng_center=as.numeric(lng_center),
#                                                               lat_center=as.numeric(lat_center))

# 
# estimated_population_df_distinct_sf <- st_as_sf(estimated_population_df_distinct,
#                                                 coords = c("lng_center", "lat_center"))


library(dplyr)
library(jpndistrict)
library(mapview)


estimated_population_df_tokyo <- estimated_population_df %>% filter(city_code>=13000, city_code<13300)
estimated_population_df_tokyo <- estimated_population_df_tokyo %>% mutate(poligon="")

for (i in 1:nrow(estimated_population_df_tokyo)) {
  estimated_population_df_tokyo$poligon[i] <- export_mesh(estimated_population_df_tokyo$one_k_mesh[i])
}
# sfオブジェクトに変換
estimated_population_df_tokyo <- sf::st_as_sf(estimated_population_df_tokyo)

city_code <- read_csv(file = "city_code.csv")
estimated_population_df_tokyo <- estimated_population_df_tokyo %>% left_join(city_code, by = c("city_code"="市町村コード"))


estimated_population_df_tokyo <- estimated_population_df_tokyo %>% filter(accident_percapita_100 < 1)

# マップでの可視化
estimated_population_df_tokyo %>% mapview::mapview(zcol = "accident_ratio", aplha = 0,at = seq(0.0001, 1, 0.09))

# マップでの可視化
estimated_population_df_tokyo %>% mapview::mapview(zcol = "accident_count", aplha = 0,at = seq(1, 50, 5))


estimated_population_df_tokyo <- estimated_population_df_tokyo %>% mutate(bin = ntile(mean_price, 10))
vis_df <- estimated_population_df_tokyo %>% filter(accident_count>0) %>% group_by(bin) %>% summarize(mean_accident_percapita=mean(accident_percapita),
                                                                                                     count=n(),
                                                                                                     mean_price=mean(mean_price))

g <- ggplot(data = vis_df, aes(x =  bin, y =mean_accident_percapita)) + theme_set(theme_bw(base_size = 14,base_family="HiraKakuProN-W3"))
g <- g +geom_point() + stat_smooth(se=T,fullrange = T,level = 0.95)
g <- g + ggtitle("地価の階級値と平均人口あたりのAP発生件数")
g

plot(vis_df$bin, vis_df$`mean(accident_percapita)`)



estimated_population_df_tokyo %>% group_by(市区町村) %>% summarize(sum_accident=sum(accident_count), population=sum(population), mean_accident_percapita=mean(accident_percapita_100)) %>% arrange(desc(sum_accident))



g <- ggplot(data = estimated_population_df_tokyo %>% filter(accident_count>0),
            aes(accident_count)) + theme_set(theme_bw(base_size = 14,base_family="HiraKakuProN-W3"))
g <- g +geom_histogram(bins = 50)
g <- g + ggtitle("1kmメッシュごとのAP件数のヒストグラム")
g


g <- ggplot(data = estimated_population_df_tokyo %>% filter(population > 0),
            aes(population)) + theme_set(theme_bw(base_size = 14,base_family="HiraKakuProN-W3"))
g <- g +geom_histogram(bins = 50)
g <- g + ggtitle("1kmメッシュごとの人口のヒストグラム")
g


g <- ggplot(data = estimated_population_df_tokyo %>% filter(mean_price > 0),
            aes(mean_price)) + theme_set(theme_bw(base_size = 14,base_family="HiraKakuProN-W3"))
g <- g +geom_histogram(bins = 50)
g <- g + ggtitle("1kmメッシュごとの平均公示地価のヒストグラム")
g


g <- ggplot(data = estimated_population_df_tokyo %>% filter(accident_ratio > 0),
            aes(accident_ratio)) + theme_set(theme_bw(base_size = 14,base_family="HiraKakuProN-W3"))
g <- g +geom_histogram(bins = 50)
g <- g + ggtitle("1kmメッシュごとの人口あたり事故物件発生件数のヒストグラム")
g

