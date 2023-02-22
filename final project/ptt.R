#=============================================
#(1)抓ptt資料
#=============================================
install.packages(c("rJava","tm","jiebaR","httr","RCurl","xml2",
                   "Rfacebook","wordcloud","ggplot2","igraph",
                   "topicmodels","NbClust","proxy","e1071"))
install.packages("tmcn")
install.packages("Rwordseg", repos="http://r-forge.r-project.org")
library(httr)
library(RCurl)
library(xml2)

data = NULL

  url = paste0('https://www.ptt.cc/man/fastfood/DA67/index.html')
  tmp1 = content(GET(url))  
  tmp2 = xml_find_all(tmp1, "//div[@class='title']/a")  
  tmp3 = xml_attr(tmp2,"href")	
  data = c(data,tmp3)


num=0
data2=NULL
for( i in data ) {
  url2 = paste0('https://www.ptt.cc/',i)
  tmp4 = content(GET(url2))  
  tmp5 = xml_find_all(tmp4, "//div[@class='title']/a")  
  tmp6 = xml_attr(tmp5,"href")	
  data2 = c(data,tmp6)
  
  data2 = Filter(function(x) x != "www.ptt.cc",data2)
  length(data2)
  num = num+length(data2)
  
  getdoc = function(url2)  {
    outputDir = "/Users/liuqingxuan/Downloads/大三下課程/資料探勘導論/ptt_摩斯漢堡/"
    url3 = paste0("https://www.ptt.cc",url2)
    tmp1 = content(GET(url3))  
    tmp2 = xml_find_all(tmp1, "//div[@id='main-content']")  
    content0 = xml_text(tmp2)   
    name = strsplit(url2, '/')[[1]][6]   
    write(content0, paste0(outputDir, gsub('html', 'txt', name)) )
  }
  sapply(data2, getdoc)
}

num
  
class(data2)
length(data2)
head(data2)
tail(data2)


#*********************************************************
#(2)jiebaR中文斷詞
#*********************************************************

setwd("/Users/liuqingxuan/Downloads/大三下課程/資料探勘導論/ptt_摩斯漢堡/")   
myDir = "/Users/liuqingxuan/Downloads/大三下課程/資料探勘導論/ptt_摩斯漢堡/"

allFiles = list.files(path = myDir, pattern = "*.txt")

myText = NULL



for (i in 1 : length(allFiles)) 
{
  t0 = readLines(allFiles[i])
  t1 = paste(t0,collapse=" ")
  myText = c(myText,t1)
}

library(jiebaR)

mySeg = worker()

myText2 = NULL

for (i in 1:length(myText)){
  t0 = myText[i]
  t1 = segment(t0,mySeg)
  t1 <- t1[nchar(t1)>1] 
  t1=gsub(pattern='[a-zA-Z[:punct:][:digit:]]',replacement='',t1)
  myText2 = c(myText2,paste0(t1,collapse=" "))
  
}
for (i in 1:132){
  myText2[i] = gsub("作者","", myText2[i])
  myText2[i] = gsub("看板","", myText2[i])
  myText2[i] = gsub("標題","", myText2[i])
  myText2[i] = gsub("閒聊","", myText2[i])
  myText2[i] = gsub("問題","", myText2[i])
}

View(myText2)

install.packages("dplyr")
library(dplyr)

text_df = data_frame(doc_id = 1:length(myText2), text = myText2)
View(text_df)
text_df = text_df[2:133,]

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#(3)非監督式SentimentAnalysis  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
myDoc1 = text_df$text 

negtxt2 = readLines("/Users/liuqingxuan/Downloads/大三下課程/資料探勘導論/ntu_negative2.txt", encoding = "UTF-8")
postxt2 = readLines("/Users/liuqingxuan/Downloads/大三下課程/資料探勘導論/ntu_positive2.txt", encoding = "UTF-8")

n = length(myDoc1)
n
score = numeric(n)

for (i in 1:n) {  
  words = unlist(strsplit(myDoc1[i]," "))
  s1 = match(words, postxt2)
  s1 = sum(!is.na(s1))   
  s2 = match(words, negtxt2)
  s2 = sum(!is.na(s2))  
  score[i] = s1 - s2
}

length(score)

score1 = factor(
  ifelse(score > 0, 1,
         ifelse(score < 0, -1, 0)
  )
)

score1_int=as.numeric(as.vector(score1))
head(score1_int)

EmotionalAnalysis = data.frame(text_df,score1_int)
#EmotionalAnalysis$doc_id<-as.numeric(EmotionalAnalysis$doc_id)
#EmotionalAnalysis$score1_int<-as.numeric(EmotionalAnalysis$score1_int)

View(EmotionalAnalysis)

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#(4)BPN與決策樹建模與預測  
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

#決策樹
require(rpart)
library(rpart)
set.seed(22)
train.index <- sample(x=1:nrow(EmotionalAnalysis), size=ceiling(0.8*nrow(EmotionalAnalysis)))
train <- text_df[train.index, ]
test <- text_df[-train.index, ]

#attach(EmotionalAnalysis)

cart.model<- rpart(EmotionalAnalysis$score1_int ~.,
                   data=train,
                   method = 'class')

require(rpart.plot)
#library("rpart.plot")
dev.new()
prp(cart.model,   
    faclen=0,         
    fallen.leaves=TRUE, 
    shadow.col="gray",  
    extra=2)  

#BPN
require(neuralnet)
require(nnet)
require(caret)
data <- EmotionalAnalysis

head(class.ind(data$score1_int))

data <- cbind(data, class.ind(data$score1_int))

head(data)
formula.bpn <- setosa + versicolor + virginica ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width

bpn <- neuralnet(formula = formula.bpn, 
                 data = data,
                 hidden = c(2),   
                 learningrate = 0.01, 
                 threshold = 0.01,  
                 stepmax = 5e5   
)

plot(bpn)


#--------------------------------------------------------
#(5)Topic Model分析說明 
#--------------------------------------------------------
install.packages("tidytext")
library(tidytext)

tok99 = function(t) strsplit(t,"[ ]{1,}")

td1 = unnest_tokens(text_df, word, text, token=tok99)
td1

td1=td1 %>% filter(word != "")
td1

td_count = dplyr::count(td1, word, sort = TRUE)
td_count

library(ggplot2)
windows()
td_count %>%
  filter(n > 50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

library(wordcloud)
windows()
td_count %>%
  with(wordcloud(word, n, min.freq = 20, scale=c(4,1),
                 random.order = T, ordered.colors = T, colors=1:length(word)))


td2 = td1 %>%
  count(doc_id,word,sort=T) %>%
  ungroup() %>%
  bind_tf_idf(word,doc_id, n)

td2

td_tfidf = arrange(td2,desc(tf_idf))

td_tfidf

plot_305 = mutate(td_tfidf,word = factor(word, levels = rev(unique(word))))

windows()
plot_305 %>%
  top_n(30) %>%
  ggplot(aes(word, tf_idf, fill = doc_id)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()

library(tm)

dtm_tf = cast_dtm(td_tfidf, doc_id, word, n)

kk = as.matrix(dtm_tf)
gg = t(kk)
dim(dtm_tf)

inspect(dtm_tf[1:10,1:10])

dtm_tfidf = cast_dtm(td_tfidf, doc_id, word, tf_idf)

dim(dtm_tfidf)

inspect(dtm_tfidf[1:10,1:10])

library(Matrix)

m = cast_sparse(td_tfidf, doc_id, word, tf_idf)

dim(m)

DTM=as.matrix(m)
TDM=t(DTM)
write.table(gg, file = "200_gg.CSV", sep = ",")
write.table(TDM, file = "200_TDM.CSV", sep = ",")

library(topicmodels)

DTM_lda_VEM <- LDA(dtm_tf, k = 5, method="VEM", control = list(seed = 1234321))
DTM_lda_VEM

DTM_lda_Gibbs <- LDA(dtm_tf, k = 5, method="Gibbs", control = list(seed = 1234321))
DTM_lda_Gibbs

ap_topics_VEM <- tidy(DTM_lda_VEM, matrix = "beta")
ap_topics_VEM

ap_topics_Gibbs <- tidy(DTM_lda_Gibbs, matrix = "beta")
ap_topics_Gibbs

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

terms(DTM_lda_VEM,50)
terms(DTM_lda_Gibbs,50)

write.table(terms(DTM_lda_Gibbs,50), file = "200_DTM_lda_Gibbs.CSV", sep = ",")






