# install.packages("RMeCab", repos = "http://rmecab.jp/R", type = "source") 
library(RMeCab)
library(topicmodels)

load(file = "accident_df_with_coordinate_from_map.RData")


accident_df$detail_fixed <- gsub(x = accident_df$detail,
                                 pattern = "\\(|\\)|（|）|/|\\.|:|_|-|[0-9]|[０-９]",
                                 replacement = " ")

# Bag of wordsの生成
res <- docMatrixDF(accident_df$detail_fixed,minFreq=4)
colnames(res) <- accident_df$k_mesh
res <- t(res)
res <- res[rowSums(res) >=1, ]

# トピック数
k <- 10
# 出力単語数
i <- 5
# 出力トピック数
j <- 5

# LDAの推定
LDA_estimate <- LDA(res, k,method="Gibbs",control=list(verbose=1))

# トピックごとの単語の出力
terms_each_topics <- data.frame(terms(LDA_estimate,i))

# ドキュメントごとの上位トピックの出力
topics_each_document <- data.frame(topics(LDA_estimate,j))


# トピックの確率の出力
topic_prob <- posterior(LDA_estimate)$topics
k_mesh_list <- rownames(topic_prob)
topic_prob <- data.frame(topic_prob)

# メッシュごとの平均的なトピックを計算する
topic_prob$k_mesh <- k_mesh_list


topic_prob %>% group_by(k_mesh) %>% dplyr::summarise_at(vars(dplyr::starts_with("X")), funs(mean))

topic_prob_summary <- topic_prob %>% group_by(k_mesh) %>% dplyr::summarise_at(vars(dplyr::starts_with("X")), funs(mean))


View(rowSums(address_bow))



library('wordcloud2')
library(RMeCab)

load(file = "accident_df_with_coordinate__from_map.RData")

address_bow <- docMatrixDF(accident_df$address,minFreq=1)
word_list <- data.frame(word=rownames(address_bow), count=rowSums(address_bow))
word_list %>% filter(count<200, !stringr::str_detect(word, "[0-9]")) %>% wordcloud2()



accident_bow <- docMatrixDF(accident_df$detail,minFreq=1)
word_list <- data.frame(word=rownames(accident_bow), count=rowSums(accident_bow))
word_list %>% filter(count<500, !stringr::str_detect(word, "[0-9]")) %>% wordcloud2()


date_bow <- docMatrixDF(accident_df$date,minFreq=1)
word_list <- data.frame(word=rownames(date_bow), count=rowSums(date_bow))
word_list %>% filter(stringr::str_detect(word, "[0-9][0-9][0-9][0-9]")) %>% wordcloud2()
