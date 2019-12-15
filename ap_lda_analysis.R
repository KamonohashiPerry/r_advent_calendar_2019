# install.packages("RMeCab", repos = "http://rmecab.jp/R", type = "source") 
library(RMeCab)
library(topicmodels)

load(file = "accident_df_with_coordinate__from_map.RData")

# Bag of wordsの生成
res <- docMatrixDF(accident_df$detail,minFreq=4)
colnames(res) <- accident_df$address
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



address_bow <- docMatrixDF(accident_df$address,minFreq=1)

View(rowSums(address_bow))
