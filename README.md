# r_advent_calendar_2019

## 手順
### Twitterに出てきたものだけを取得（直近3ヶ月〜）
+ run_tweet_collect.RでTweetを収集
+ run_selenium.RでAPの情報をスクレイピング
+ run_map_api.Rで住所から緯度経度の取得
+ making_mesh_data_and_download_other_data.Rで緯度経度からメッシュデータへの変換、その他の人口データや地価データと接続

### 直近3ヶ月以前のものを取得
+ run_selenium_from_map.Rで地図上から直接APの情報を取得する
+ making_mesh_data_and_download_other_data.Rで緯度経度からメッシュデータへの変換、その他の人口データや地価データと接続
