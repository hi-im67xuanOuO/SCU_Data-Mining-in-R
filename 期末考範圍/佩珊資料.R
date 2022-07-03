rm(list = ls())       #清除右邊視窗變數
cat('\014')           #清除console視窗內容

#setwd("C:/data/ptt_NTUST")   #設定R執行路徑
#myDir = "c:/data/ptt_NTUST"

setwd("/Users/liuqingxuan/Downloads/大三下課程/資料探勘導論/coso_305_orginal")   #設定R執行路徑
myDir = "/Users/liuqingxuan/Downloads/大三下課程/資料探勘導論/coso_305_orginal"

#setwd("C:/data/hotel")   #設定R執行路徑
#myDir = "c:/data/hotel"

#setwd("C:/data/3KR")   #設定R執行路徑
#myDir = "c:/data/3KR"


# 取得讀入 c:\data\in 資料夾中所有.txt的文章
allFiles = list.files(path = myDir, pattern = "*.txt")
#allFiles = list.files("c:/data/in",".txt",full.names = T)
myText = NULL

#Sys.setlocale(category = "LC_ALL", locale = "zh_cn.utf-8")
#Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")

#個別讀第一個檔案
  # fpath = paste(myDir,"2801-104-1.txt",sep="/")
# t0 = readLines(fpath)
# t0<-iconv(readLines(fpath), from = "Big5", to = "UTF8")
# t1 = paste(t0,collapse=" ")
# myText = c(myText,t1)
# 
#
# 個別讀第二個檔案
# fpath = paste(myDir,"002.txt",sep="/")
# t0 = readLines(fpath)
# t1 = paste(t0,collapse=" ")
# myText = c(myText,t1)
# 
# 
#個別讀第三個檔案
# fpath = paste(myDir,"003.txt",sep="/")
# t0 = readLines(fpath)
# t1 = paste(t0,collapse=" ")
# myText = c(myText,t1)


#使用for迴圈讀N的檔案，myText是一個文字向量，
#myText文字向量中每一個元素(Entries)儲存一篇文章內容
for (i in 1 : length(allFiles)) 
{
  t0 = readLines(allFiles[i])
  t0<-iconv(readLines(allFiles[i]), from = "Big5", to = "UTF8") #爽轉檔
  t1 = paste(t0,collapse=" ")
  myText = c(myText,t1)
}
#myWant = myText[grepl("赤壁", myText)]

# for (f in allFiles) {
#   fpath = paste(xdir,f,sep="/")
#   t0 = readLines(fpath)
#   t1 = paste(t0,collapse=" ")
#   myText = c(myText,t1)
# }

# 使用 jiebaR 套件做中文斷詞
#install.packages("jiebaR")
library(jiebaR)

mySeg = worker() # 建立一個jiebaR斷詞環境變數

# 使用new_user_word函數加入jiebaR無法判定或者新出現的名詞
#new_user_word(mySeg, c("金融監督管理委員會"),"n")



#使用for迴圈讀N的檔案，myText是一個文字向量，
#myText文字向量中每一個元素(Entries)儲存一篇文章內容
myText2 = NULL


for (i in 1:length(myText)){
  t0 = myText[i]
  #t1 = mySeg <= t0
  t1 = segment(t0,mySeg)
  #t1 <- t1[nchar(t1)>1 & nchar(t1)<7] #去除字符長度小於2的詞語,&<7
  t1 <- t1[nchar(t1)>1] #去除字符長度小於2的詞語
  #t1 <- t1[nchar(t1)>0] #去除字符長度小於1的詞語
  t1=gsub(pattern='[a-zA-Z[:punct:][:digit:]]',replacement='',t1)
  myText2 = c(myText2,paste0(t1,collapse=" "))
}

#myText2也是文字向量儲存149篇已經經過jiebaR斷詞過的文章
#接下來要條列出每一篇文章中的詞，將表示成兩個欄位的平面表格方式
#利用dplyr套件的data_frame函數先將文字向量轉成data.frame格式
#install.packages("dplyr")
library(dplyr)
#將文章轉成data.frame格式
text_df = data_frame(doc_id = 1:length(myText2), text = myText2)

#myWant1 = myText2[grepl("行員", myText2)]
#install.packages("tidytext")
library(tidytext)
# 轉成 tidytext 格式
# unnest_tokens_(data, output_col, input_col, token = "words")
# 參數：
# output_col: output column, 新變數名稱是 word
# input_col: input column, 舊變數名稱是 text_df 中的 text
# token 預設是 “words”, 但也可以是 “ngrams”, “sentences”, “lines”,
# “paragraphs”,…

# unnest_tokens 函數遇到中文會不正常，偶而會把已經斷好的詞
# 再切開成單字
# 自己準備一個 tokenizer 函數 tok99
tok99 = function(t) strsplit(t,"[ ]{1,}")
#strsplit()函數分割字串
td1 = unnest_tokens(text_df, word, text, token=tok99)
td1

#直接在源頭td1去掉空白字元
td1=td1 %>% filter(word != "")
td1

td_count = dplyr::count(td1, word, sort = TRUE)
td_count

#td_count=td_count[-1,]

############################################
# 詞彙頻率圖
############################################
#dplyr()套件中融入很多概念與結構化查詢語言(SQL)相仿的函數。
#搭配 %>% 運算子(%>% 運算子稱作 Pipeline)一起使用，能夠讓我們整理資料的能力獲得一個檔次的提升！
#filter()	篩選符合條件的觀測值
#select()	選擇變數
#mutate()	新增變數
#arrange()	依照變數排序觀測值
#summarise()	聚合變數
#group_by()	依照類別變數分組，搭配


library(ggplot2)
#windows()
td_count %>%
  filter(n > 50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


############################################
# 文字雲
############################################

library(wordcloud)
#windows()
td_count %>%
  with(wordcloud(word, n, min.freq = 20, scale=c(4,1),
                 random.order = T, ordered.colors = T, colors=1:length(word)))

############################################
# 計算 TF-IDF
############################################

# 先計算個別文章各詞的 TF-IDF
td2 = td1 %>%
  count(doc_id,word,sort=T) %>%
  ungroup() %>%
  bind_tf_idf(word,doc_id, n)

td2

td_tfidf = arrange(td2,desc(tf_idf))

td_tfidf


#####################################################
# 各篇文章的 TF-IDF 比較圖
#####################################################
#mutate()以現有的column資料做運算，形成新的column
plot_305 = mutate(td_tfidf,word = factor(word, levels = rev(unique(word))))


#install.packages("showtext")
library(showtext)
showtext.auto(enable = TRUE)
par(family="STKaiti") 
par(family = "STHeiti")
#windows()
plot_305 %>%
  top_n(30) %>%
  ggplot(aes(word, tf_idf, fill = doc_id)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()

############################################################
# 轉成 tm 套件的  dtm 矩陣
#
# cast_dtm(data, document_col, term_col, value_col,
# weighting = tm::weightTf, …)
############################################################
#install.packages("tm")
library(tm)

# 轉成一般 TF 為主的 dtm 矩陣
dtm_tf = cast_dtm(td_tfidf, doc_id, word, n)
#class(dtm_tf)
kk = as.matrix(dtm_tf)
gg = t(kk)
dim(dtm_tf)
#[1] 305 1971

inspect(dtm_tf[1:10,1:10])

# 轉成 TF-IDF dtm 矩陣
dtm_tfidf = cast_dtm(td_tfidf, doc_id, word, tf_idf)
kk2 = as.matrix(dtm_tfidf)

dim(dtm_tfidf)
#[1] 149 1815

inspect(dtm_tfidf[1:10,1:10])

############################################################
# 轉成 Matrix 套件的稀疏矩陣 (sparse matrix)
############################################################
#install.packages("Matrix")
library(Matrix)

# cast into a Matrix object
m = cast_sparse(td_tfidf, doc_id, word, tf_idf)

dim(m)
#[1] 149 1815

DTM=as.matrix(m)
TDM=t(DTM)
write.table(gg, file = "149_gg.CSV", sep = ",")
write.table(TDM, file = "149_TDM.CSV", sep = ",")

#==================================================================
#此時DTM或者TDM已經是下一階段進行主題模型(topic modeling)工作
#所認同的矩陣型態(通常就是指從文件資料進到Corpus後再轉出的矩陣格式)
#==================================================================

#install.packages('topicmodels')
library(topicmodels)

#DTM2 = removeSparseTerms(DTM, 0.9) 

#利用LDA函數建立一個擁有五個主題的topic model，稱為DTM_lda
DTM_lda_VEM <- LDA(dtm_tf, k = 5, method="VEM", control = list(seed = 1234321))
DTM_lda_VEM

DTM_lda_Gibbs <- LDA(dtm_tf, k = 5, method="Gibbs", control = list(seed = 1234321))
DTM_lda_Gibbs

library(tidytext)

ap_topics_VEM <- tidy(DTM_lda_VEM, matrix = "beta")
ap_topics_VEM
#write.table(ap_topics_VEM, file = "coso_ap_topics_VEM_20171225.CSV", sep = ",")

ap_topics_Gibbs <- tidy(DTM_lda_Gibbs, matrix = "beta")
ap_topics_Gibbs
#write.table(ap_topics_Gibbs, file = "coso_ap_topics_Gibbs_20171225.CSV", sep = ",")


library(ggplot2)
library(dplyr)

ap_top_terms_VEM <- ap_topics_VEM %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

windows()
ap_top_terms_VEM %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

ap_top_terms_Gibbs <- ap_topics_Gibbs %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

windows()
ap_top_terms_Gibbs %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#首先，先看term歸屬哪一個topic，顯示出每一個主題最重要的前50個詞
terms(DTM_lda_VEM,50)
terms(DTM_lda_Gibbs,50)

#當然也可以全顯示並且輸出為一個CSV檔案
terms(DTM_lda_VEM,1971)
term_topic_VEM=as.matrix(terms(DTM_lda_VEM,1971))
#write.table(term_topic_VEM, file = "coso_term_topic_VEM_20171225.CSV", sep = ",")


terms(DTM_lda_Gibbs,1971)
term_topic_Gibbs=as.matrix(terms(DTM_lda_Gibbs,1971))
#write.table(term_topic_Gibbs, file = "coso_term_topic_Gibbs_20171225.CSV", sep = ",")



#接著看doc歸屬哪一個topic，
#即看每一份文件被模型DTM_lda分配到哪一個主題topic中
topics(DTM_lda_VEM)
doc_topic_VEM=as.matrix(topics(DTM_lda_VEM))
table(doc_topic_VEM)
#write.table(doc_topic_VEM, file = "coso_doc_topic_VEM_20171225.CSV", sep = ",")


topics(DTM_lda_Gibbs)
doc_topic_Gibbs=as.matrix(topics(DTM_lda_Gibbs))
table(doc_topic_Gibbs)
#write.table(doc_topic_Gibbs, file = "coso_doc_topic_Gibbs_20171225.CSV", sep = ",")


#再來，可以查看前面所有term以及每一份文件doc是歸屬於某一主題topic的機率值Pr
postpr_all_VEM=posterior(DTM_lda_VEM)
postpr_all_Gibbs=posterior(DTM_lda_Gibbs)

postpr_all_VEM
postpr_all_VEM_term = postpr_all_VEM$terms[1:5,1:1971]
#write.table(postpr_all_VEM_term, file = "coso_postpr_all_VEM_term_20171225.CSV", sep = ",")
postpr_all_VEM_topic = postpr_all_VEM$topics[1:305,1:5]
#write.table(postpr_all_VEM_topic, file = "coso_postpr_all_VEM_topic_20171225.CSV", sep = ",")

postpr_all_Gibbs
postpr_all_Gibbs_term = postpr_all_Gibbs$terms[1:5,1:1971]
#write.table(postpr_all_Gibbs_term, file = "coso_postpr_all_Gibbs_term_20171225.CSV", sep = ",")
postpr_all_Gibbs_topic = postpr_all_Gibbs$topics[1:305,1:5]

#write.table(postpr_all_Gibbs_topic, file = "coso_postpr_all_Gibbs_topic_20171225.CSV", sep = ",")


#看前10份文件歸屬於每一個主題Topic的機率值，
#使用posterior函數的用意為事後機率值的觀念可以幫助預測(歸屬到哪一個主題)
#doc_topic_pr_VEM = posterior(DTM_lda_VEM,DTM[1:10,])$topics
#write.table(doc_topic_pr_VEM, file = "coso_doc_topic_pr_VEM_20171225.CSV", sep = ",")

#doc_topic_pr_Gibbs = posterior(DTM_lda_Gibbs,DTM[1:10,])$topics
#write.table(doc_topic_pr_Gibbs, file = "coso_doc_topic_pr_Gibbs_20171225.CSV", sep = ",")



#另外一個重要議題是，R語言另外又提供一個套件lda，資料格式完全
#不甩tm套件所規定的矩陣資料格式，所以lda套件有自己的矩陣資料格式，
#剛好發明topicmodels套件的作者心量大，寫了一個從topicmodels套件格式
#轉換到lda資料格式的函數dtm2ldaformat()方便資料處裡，兩個套件差別在哪呢?
#萃取主題的方法不同，後者lda套件是採用Gibbs抽樣方法求出，
#而且後者lda套件呈現圖形工具較為豐富
#install.packages('lda')
library(lda)

xx=dtm2ldaformat(dtm_tf)
names(xx)  
#查看xx有哪些內容，有兩個，其一為document，另一個為vocab
xdoc=xx$documents #文件document
xvocab=xx$vocab   #詞彙vocabulary

#install.packages('ggplot2')
library(ggplot2)
#install.packages('reshape2')
library(reshape2)


theme_set(theme_bw())  
set.seed(8675309)


#因為金融內控缺失有定義五個定義，所以
#此處K=5設定五個主題Number of topics
K = 5 
result = lda.collapsed.gibbs.sampler(xdoc,K,xvocab,
                                     25,  ## Num iterations
                                     0.1,0.1,compute.log.likelihood=TRUE) 

#Get the top words in the topic
#此函數top.topic.words()可以獲得每個主題的熱門詞彙和文檔
#此函數top.topic.documents()可以獲得每個主題的熱門詞彙和文檔
top.words = top.topic.words(result$topics, 5, by.score=TRUE)
#top.documents =top.topic.documents(result$document_sums, num.documents = 100, alpha = 0.1)

a=c() 
b=c() 
for(i in 1:5) 
{ 
  a=c(a,top.words[,i]) 
  b=c(b,rep(paste("主題",i,sep=" "),5)) 
}

a = table(a, b) 
a = as.matrix(a) 
library(wordcloud)
windows()
comparison.cloud(a, scale = c(1, 1.5), rot.per = 0.5, colors = brewer.pal(ncol(a), 
                                                                          "Dark2"))


#######################
# Plot畫圖
#######################

# 取20份文件顯示，Number of documents to display
N = 20

topic.proportions = t(result$document_sums) / colSums(result$document_sums)

topic.proportions = 
  topic.proportions[sample(1:dim(topic.proportions)[1], N),]

topic.proportions[is.na(topic.proportions)] =  1 / K

colnames(topic.proportions) = apply(top.words, 2, paste, collapse=" ")

topic.proportions.df = melt(cbind(data.frame(topic.proportions),
                                  document=factor(1:N)),
                            variable.name="topic",
                            id.vars = "document")  

#qplot(value, fill=document, xlab="topic", ylab="proportion",
#      data=topic.proportions.df, geom="bar") +
#  theme(axis.text.x = element_text(angle=90, hjust=1)) +  
#  coord_flip() + 改變座標
#  facet_wrap(~ document, ncol=5) 

windows()
ggplot(data=topic.proportions.df, aes(x=topic, y=value, fill=document)) +
  geom_bar(colour="black", stat="identity") +
  labs(x = "topic", y = "proportion") + ggtitle("documenttopicPr")+ coord_flip()+  facet_wrap(~ document, ncol=5) 


#R語言中有預設的tfidf計算公式，此為加權過的值，
#與一般Data Mining學理上所介紹的tfidf定義的結果有些差異
#inspect(DTM_tfidf_R)  #查看tfidf值與教材中內容是否有差異?
#TDM_tfidf_R=t(DTM_tfidf_R)

#視覺化文字雲
#install.packages("wordcloud")  #安裝wordcloud套件
library(wordcloud)             #載入wordcloud套件

#計算目前DTM矩陣中每一個英文字在五份文件中的總次數
myterm_tfidf_R = sort(colSums(as.matrix(DTM_tfidf_R)),decreasing = TRUE)
head(myterm_tfidf_R,10)
#在R中繪製文字雲
windows()#另外開啟一個視窗顯示文字雲
wordcloud(names(myterm_tfidf_R),myterm_tfidf_R,min.freq = 1,random.order = F,colors=1:length(names(myterm_tfidf_R)))


# 詞彙集群分析
# tdm: 橫列是詞彙t(terms)，直行是文件d(documents)
# 階層式集群分析
library(tm)
tdm2 = removeSparseTerms(TDM_tfidf_R, sparse=0.65)
m2 = as.matrix(tdm2)
distMatrix = dist(scale(m2))
fit = hclust(distMatrix, method="ward.D")
windows()
plot(fit)

# cut tree into 5 clusters
rect.hclust(fit, k=5)

#==============================================
#標準參考書中TFIDF計算方法
#==============================================
#另外，計算Data Mining學術上常用的tfidf權重值
#先將語料庫中所建立的TDM轉成可以加減乘除等運算的一般矩陣myDTM3
TDM3 = removeSparseTerms(TDM, sparse=0.65)
myTDM3 = as.matrix(TDM3)
myDTM3=t(myTDM3)

#先計算每一份文件所包含的字詞數量，即針對每一列加總rowSums
myrs=rowSums(myDTM3)

#先計算每一個字詞在所有文件中出現的數量，即針對每一行加總colSums
# myDTM3/myDTM3
# colSums(myDTM3/myDTM3)
# is.nan(myDTM3/myDTM3)
# replace(myDTM3/myDTM3,is.nan(myDTM3/myDTM3),0)

mycs=colSums(replace(myDTM3/myDTM3,is.nan(myDTM3/myDTM3),0))


#計算myDTM3中每一個字詞的idf值
myidf=log10(nrow(myDTM3)/mycs)


#為myDTM3中每一個字詞在每一份文件中的tfidf值
for (i in 1:nrow(myDTM3))
{
  myDTM3[i,]=(myDTM3[i,]/myrs[i])*myidf
}
#轉置為TDM格式
myTDM3tfidf=t(myDTM3)


m3 = as.matrix(myTDM3tfidf)
distMatrix3 = dist(scale(m3))
fit3 = hclust(distMatrix3, method="ward.D")
windows()
plot(fit3)

# cut tree into 5 clusters
rect.hclust(fit3, k=5)