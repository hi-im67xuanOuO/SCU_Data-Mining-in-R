rm(list = ls())       #清除右邊視窗變數
cat('\014')           #清除console視窗內容

df <- read.csv("/Users/liuqingxuan/Downloads/資料探勘期末報告/kfc.csv",stringsAsFactors=FALSE)
View(df)
myDoc = c(df$text)

#R中顯示向量內容的基本方法
myDoc
myDoc[1]
myDoc[3:5]
myDoc[c(2,4)]

#安裝tm套件
#install.packages("tm")
#載入tm套件
library(tm)            

getSources() #告訴您R的語料庫來源形式可以是哪些哪些
getReaders() #告訴您R可以讀的格式有哪些哪些

#利用Corpus()函數建立語料庫cp_vdocs

cp_myDoc=Corpus(VectorSource(myDoc)) 

#在R中語料庫Corpus的是屬於List資料型態所以無法直接點選看內容
#可以使用inspect()函數查看內容
inspect(cp_myDoc)
#直接點選只會顯示此Corpus的概述
cp_myDoc
#看一下右邊視窗中這一個Corpus變數的組成
#可以使用$符號接content屬性看內容
cp_myDoc$content
cp_myDoc$content[1]  #看任意筆資料

#或者可以使用[[]]兩個中括號來查看List資料
cp_myDoc[[1]]
cp_myDoc[[1]][1]  #就是content真實內容
cp_myDoc[[1]][2]  #meta內容
cp_myDoc[[1]][3]  #沒有任何東西可以顯示
#同理可以查看第二句話
cp_myDoc[[2]]
cp_myDoc[[2]][1]  #就是content真實內容
cp_myDoc[[2]][2]  #meta內容
cp_myDoc[[2]][3]  #沒有任何東西可以顯示


#試著先將目前語料庫的所有英文字透過DocumentTermMatrix函數
#產生一個文件(檔)-詞條矩陣通常以DTM or dtm命名
DTM=DocumentTermMatrix(cp_myDoc)
#該矩陣依然為List資料型態，只能使用inspect()函數查看內容
inspect(DTM)

testDTM=as.matrix(DTM)
View(testDTM)

bk<-cbind(testDTM,df$score1_int)
View(bk)
#write.table(bk,file="/Users/liuqingxuan/Downloads/mos_test_newnew_model.csv",sep=",",row.names=F, na = "NA")


BK<-as.data.frame(bk)
View(BK)
class(BK)
dim(BK)
str(BK)
summary(BK)
head(BK)
table(BK$V8755) #情緒分析統計數量

n<-length(BK$V8755)
for (i in 1:132) {
  if(BK$V8755[i]==1){
    BK$V8755[i]="+"
  }else if(BK$V8755[i]==0){
    BK$V8755[i]="="
  }else{
    BK$V8755[i]="-"
  }
}
View(BK)

write.table(BK,file="/Users/liuqingxuan/Downloads/kfc_BK.csv",sep=",",row.names=F, na = "NA")

#****************
#CART建模rpart()
#****************

#install.packages("rpart")
library(rpart)

bankModel_1 = rpart(BK$V8755 ~., data = BK)
class(bankModel_1)
#summary(bankModel_1)
bankModel_1
plot(bankModel_1) ; text(bankModel_1)
names(bankModel_1)  #看一下有哪些屬性?
bankModel_1$variable.importance
#View(bankModel_1$variable.importance)

#****************
#CART預測rpart()
#****************

bankPred_1 = predict(bankModel_1, type = "class")
class(bankPred_1) 
bankPred_1
table(bankPred_1)
#summary(bankPred_1)


#*********************************
#評估CART模型
#使用混淆矩陣(Confusion Matrix)
#*********************************

CM1 = table(BK$V8755, bankPred_1, dnn=c("實際值","預測值"))  
accuracy_1 = sum(diag(CM1))/sum(CM1)*100
print(CM1);cat("\n\n全體的預測正確率 = ", accuracy_1, "% \n")







#***************************************************
#R語言中有預設的tfidf計算公式，但是此為加權過的值，
#與一般Text Mining學理上所介紹的tfidf定義的公式結果
#有些差異，可以先行使用
#***************************************************
DTM_R_tfidf = DocumentTermMatrix(cp_myDoc,control = list(weighting=weightTfIdf))
inspect(DTM_R_tfidf)  #查看tfidf值與教材中內容是否有差異?
testDTM_R_tfidf=as.matrix(DTM_R_tfidf)
write.table(testDTM_R_tfidf,file="/Users/liuqingxuan/Downloads/kfc_testDTM_R_tfidf.csv",sep=",",row.names=F, na = "NA")


#**************
#視覺化文字雲
#**************
#install.packages("wordcloud")  #安裝wordcloud套件
library(wordcloud)             #載入wordcloud套件

#計算目前DTM矩陣中每一個英文字在五份文件中的總次數
myterm = sort(colSums(as.matrix(DTM)),decreasing = TRUE)
head(myterm,3)
#在R中繪製文字雲
wordcloud(names(myterm),myterm,min.freq = 1,random.order = F)

#在R中繪製文字雲的另一個方法
windows()  #另外開啟一個視窗顯示文字雲
wordcloud(names(myterm),myterm,min.freq = 1,colors = 1:length(names(myterm)),random.order = T)


#計算目前DTM_R_tfidf矩陣中每一個英文字
#在五份文件中的tfidf值的加總
myterm_R_tfidf = sort(colSums(as.matrix(DTM_R_tfidf)),decreasing = TRUE)
head(myterm_R_tfidf,3)
#在R中繪製文字雲
wordcloud(names(myterm_R_tfidf),myterm_R_tfidf,min.freq = 0,random.order = F)

#在R中繪製文字雲的另一個方法
windows()  #另外開啟一個視窗顯示文字雲
wordcloud(names(myterm_R_tfidf),myterm_R_tfidf,min.freq = 0,colors = 1:length(names(myterm_R_tfidf)),random.order = T)


#***************************************
#計算Text Mining學術上常用的tfidf權重值
#先將語料庫中所建立的DTM轉成可以加減乘除
#等運算的一般矩陣myDTM
#***************************************
myDTM = as.matrix(DTM)
myDTM_TM_tfidf = myDTM 
#先計算每一份文件所包含的字詞數量，
#即針對每一列加總rowSums
myrs=rowSums(myDTM_TM_tfidf)

#先計算每一個字詞在所有文件中出現的數量，
#即針對每一行加總colSums，拆解步驟如下：
# myDTM/myDTM
# colSums(myDTM/myDTM)
# is.nan(myDTM/myDTM)
# replace(myDTM/myDTM,is.nan(myDTM/myDTM),0)
mycs=colSums(replace(myDTM_TM_tfidf/myDTM_TM_tfidf,is.nan(myDTM_TM_tfidf/myDTM_TM_tfidf),0))


#計算每一個字詞的idf值
myidf=log10(nrow(myDTM_TM_tfidf)/mycs)


#為每一個字詞在每一份文件中的tfidf值
for (i in 1:nrow(myDTM_TM_tfidf))
{
  myDTM_TM_tfidf[i,]=(myDTM_TM_tfidf[i,]/myrs[i])*myidf
}

myDTM_TM_tfidf  #此內容值為一般Text Mining中的tfidf



#************************************************************
#簡單的集群分析Clustering 
#可以根據文件距離做集群Clustering
#又或者可以根據字詞做集群Clustering
#在集群分析中，主要可以分成兩種類型：
#(1)階層式集群(Hierarchical Clustering)：
#   不需指定集群數目，讓資料自動由上往下/由下往上結合起來。
#(2)分割式集群(Partitional Clustering)：
#   需事先指定集群數目，經過不斷的迭代，直到群內的變異最小。
#************************************************************

#首先利用dist()函數先產生文件之間「距離矩陣(Distance Matrix)」，
#可以初判資料之間的距離的遠或近，距離的計算常用的有兩種：
#(1)歐式距離euclidean 
#(2)曼哈頓距離manhattan
#的另一個觀念即相似度(Similarity)
mydist=dist(myDTM_TM_tfidf)
#mydist_e=dist(myDTM_TM_tfidf,method="euclidean")
#mydist_m=dist(myDTM_TM_tfidf,method="manhattan")


#接著就可以根據資料間的距離，進行階層式集群，可以使用hclust()函數
#一般來說，要如何把某些資料結合起來為一群，不同的集群方式，
#也會產生不同的集群結果，R語言中常用有五種方法，而我們可以
#在hclust()函數的method參數中修改，語法如下：
#hclust(mydist, method="single")   # 最近法(Single Linkage)
#hclust(mydist, method="complete") # 最遠法(Complete Linkage)
#hclust(mydist, method="average")  # 平均法(Average Linkage)
#hclust(mydist, method="centroid") # 中心法(Centroid Method)
#hclust(mydist, method="ward.D2")  # 華德法(Wards Method)
myhc=hclust(mydist,method = "single")  #階層式集群法

#視覺化繪圖
windows()
plot(myhc)  #繪製集群圖
#abline(h=0.06, col="blue")
#標示區塊框出三群資料的範圍
rect.hclust(myhc, k=3)


mydist_sc=dist(scale(myDTM_TM_tfidf))
myhc_sc=hclust(mydist_sc,method = "single")  #階層式集群法
windows()
plot(myhc_sc)  #繪製集群圖
#abline(h=0.06, col="red")
rect.hclust(myhc_sc, k=3)

#計算目前myDTM矩陣中每一個英文字在五份文件中的總次數
myterm_TM_tfidf = sort(colSums(myDTM_TM_tfidf),decreasing = TRUE)
head(myterm_TM_tfidf,3)
wordcloud(names(myterm_TM_tfidf),myterm_TM_tfidf,min.freq = 0,random.order = F)  #R中顯示文字雲


windows()  #另外開啟一個視窗顯示文字雲
wordcloud(names(myterm_TM_tfidf),myterm_TM_tfidf,min.freq = 0,colors = 1:length(names(myterm_TM_tfidf)),random.order = F)


#因此我們可以利用cutree()，讓整個階層的結構縮減，
#變成分成三群的狀態：
cut.h.cluster <- cutree(myhc, k=3)  # 分成三群
cut.h.cluster                       # 集群結果



#使用函式是kmeans()：
# 分成三群
kmeans.cluster <- kmeans(myDTM_TM_tfidf, centers=3) 

# 查看群內的變異數(look up the variation within the group)
kmeans.cluster$withinss

# 查看集群結果
kmeans.cluster$cluster

