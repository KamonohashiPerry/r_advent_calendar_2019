# devtools::install_github("geoffjentry/twitteR")
# devtools::install_github("rstudio/config")
# devtools::install_github("lsilvest/bit64")

library(twitteR)
library(config)
library(bit64)

n <- 3000
user_name <- "Oshimaland"

# sleep関数の指定
sleep <- function(sec){
  start <- proc.time()
  end <- proc.time()
  while((end-start)[3] < sec){ end <- proc.time() }
}

# ローカルのconfigファイルからオブジェクトの取得
conf <- config::get(config = "twitter")

# twitterAPIの認証
setup_twitter_oauth(consumer_key = conf$api_key,
                    consumer_secret =  conf$api_secret,
                    access_token = conf$access_token,
                    access_secret = conf$access_token_secret)

# twitterから取得する情報のdata.frame
ut_df <- data.frame(created_at=NA,
                    id=bit64::as.integer64(NA),
                    url=NA)

# プログレスバー
pb <- txtProgressBar(min = 1, max = n, style = 3)

# 初回にid指定なしで取得し、それ以降は直前の取得結果のIDを参照しながら
# さかのぼってtweetを取得する。
for (j in 201:n) {
  if (j == 1){
    ut <- userTimeline(user=user_name)
  }
  else {
    ut <- userTimeline(user=user_name,
                       maxID = min(ut_df$id[2:nrow(ut_df)]))
  }
  ut_length <- length(ut)
  
  if (ut_length > 0){
    empty_vec <- rep(NA, ut_length)
    
    df <- data.frame(created_at=empty_vec,
                     id=empty_vec,
                     url=empty_vec)
    
    for (i in 1:ut_length) {
      df$created_at[i] <- ut[[i]]$created
      df$id[i] <- ut[[i]]$id
      if (!is.null(ut[[i]]$urls$display_url)) {
        df$url[i] <- ut[[i]]$urls$display_url 
      }
    }
    df$id <- bit64::as.integer64(df$id)
    ut_df <- rbind(ut_df, df)
    
    setTxtProgressBar(pb, j) 
    sleep(60)
    
    save(ut_df, file = "ut_df_201912.RData")
    
  }
 print(nrow(df))
}

load(file = "ut_df_201912.RData")
ut_df_1 <- ut_df

load(file = "ut_df.RData")
ut_df_2 <- ut_df


ut_df_all <- rbind(ut_df_1, ut_df_2)

length(unique(ut_df_all$url))

# 保存
# save(ut_df, file = "ut_df.RData")
