library(LDATS)
library(RMeCab)
library(tidyverse)

data(rodents)


load(file = "accident_df_with_coordinate_from_map.RData")


# 年のデータを作成
# 年ごとにテキストを作成
# 形態素解析
# 不要語の除去


accident_df <- accident_df %>%  filter(!is.na(address))
accident_df$date_fixed <- ""

for (i in 1:nrow(accident_df)) {
  accident_df$date_fixed[i] <- strsplit(accident_df$date, "[年]|[〜]|[/]")[[i]][1]
}

table(accident_df$date_fixed)

accident_df$date_fixed_heisei <- if_else(grepl(pattern = "平成",x = accident_df$date_fixed), 1988,
                                  if_else(grepl(pattern = "令和元", x = accident_df$date_fixed), 2019,
                                   if_else(grepl(pattern = "昭和", x = accident_df$date_fixed), 1925, 0)))

accident_df$date_fixed <- gsub(pattern = "平成|令和元|昭和", replacement = "", x = accident_df$date_fixed)

accident_df$date_fixed <- as.integer(accident_df$date_fixed) + as.integer(accident_df$date_fixed_heisei)


accident_df <- accident_df %>% filter(!is.na(date_fixed), date_fixed > 1900, date_fixed < 2021)

accident_df$detail_fixed <- gsub(x = accident_df$detail,
                                 pattern = "\\(|\\)|（|）|/|\\.|:|_|-|[0-9]|[０-９]",
                                 replacement = " ")

accident_df_recent <- accident_df %>% filter(date_fixed > 2010)


# 年ごとにテキストを作成
text_data <- accident_df_recent %>% 
                  group_by(date_fixed) %>% 
                  mutate(text_by_year = paste0(detail, collapse = "")) 

text_data <- text_data %>% distinct(date_fixed, .keep_all = TRUE)
text_data <- text_data %>% select(date_fixed, text_by_year)

text_data$text_by_year <- gsub(x = text_data$text_by_year,
                                 pattern = "\\(|\\)|（|）|/|\\.|:|_|-|[0-9]|[０-９][a-z][A-Z]|&|=|１|２|３|４|０|b|https|www|suumo|chintai|jp|の|二|こと|ヶ月|一",
                                 replacement = " ")

text_data$text_by_year <- gsub(x = text_data$text_by_year,
                               pattern = "首つり",replacement = "首吊り")
text_data$text_by_year <- gsub(x = text_data$text_by_year,
                               pattern = "腐敗",replacement = "腐乱")
text_data$text_by_year <- gsub(x = text_data$text_by_year,
                               pattern = "落下",replacement = "転落")
text_data$text_by_year <- gsub(x = text_data$text_by_year,
                               pattern = "事故死",replacement = "事故")
text_data$text_by_year <- gsub(x = text_data$text_by_year,
                               pattern = "風呂",replacement = "浴室")


# Bag of wordsの生成
res <- docMatrixDF(text_data$text_by_year,minFreq=10)

word_list <- c("首吊り","遺棄","刺殺","変死","孤独","殴殺","殺人","火災","瑕疵","異臭","練炭","絞殺","白骨","腐乱","自殺","転落","事件","事故","アパート","室内","屋上","トイレ","ビル","ベランダ","浴室","マンション")

res <- data.frame(res)
res <- res[row.names(res) %in% word_list, ]

word_vector <- row.names(res)

colnames(res) <- text_data$date_fixed

res <- res %>% select(`2011`,`2012`,`2013`,`2014`,`2015`,`2016`,`2017`,`2018`,`2019`)

for (i in 1:nrow(res)) {
  nam <- paste("w_", i, sep = "")
  assign(nam, as.integer(res[1, ]))
}

# こんな感じで作っていけばいい。
document_term_table <- data.frame(list(aaa=w_1, bbb=w_2))


document_covariate_table <- list(year=as.integer(colnames(res)),
                                            year2=as.integer(colnames(res)))

test_set <- list(document_term_table, document_covariate_table)

r_LDATS <- LDA_TS(rodents,
                  topics = 2:5, 
                  nseeds = 2,
                  formulas = ~1,  
                  nchangepoints = 0:1,
                  timename = "newmoon")


print(r_LDATS)

plot(r_LDATS)



