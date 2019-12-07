library(RSelenium)

#立ち上げたいページのurl
url <- "https://www.google.co.jp/"

#ブラウザを立ち上げる
rem <- remoteDriver(port=4444L,browserName = "chrome")
rem$open()

#ブラウザで目的のページに移動する場合
rem$navigate(url)

