#如果是使用RStudio軟體使用者可以執行
#下列兩行指令快速達成視窗清空的工作
rm(list = ls()) #清除RStudio軟體右邊環境視窗變數內容
cat("\014")     #清除RStudio軟體下方Console Window視窗內容

#首先，先以大數據分析中非常有名的鳶尾花(iris)資料集(dataset)
#作為說明，iris資料集是由150個橫列(row)與5個直欄(column)所組
#成的資料集，在分類技術中建立分類模型所需要的資料分成兩部分，
#分別為訓練資料集(Training dataset)與測試資料集(Testing dataset)
#，以下介紹在R語言中的處理方式與語法
data()        #確認R提供的免費的資料集(dataset)是否有iris
data("iris")  #載入iris資料集
iris          #查看iris資料集詳細資料
#欄位有5個
#Sepal.Length、Sepal.Width、Petal.Length、Petal.Width、Species
#Sepal為花萼，Petal為花瓣，Species種類

class(iris)   #是data.frame資料型態，中文稱為資料框，就是一個二維表格
nrow(iris)    #計算總共幾筆資料
ncol(iris)    #計算總共幾個欄位
dim(iris)     #顯示此表格有幾列與幾行

names(iris)   #顯示此表格有那些欄位名稱
str(iris)     #str()就是結構(structure)的意思，顯示出
              #關於此表格中資料值的型態與資料
summary(iris) #關於此表格每一個欄位中的基本統計資料

#接著查看資料框中的任一資料
iris[2,3]  #表格中查看資料[列row,行column]，iris[2,3]即第2列第3行
iris[2,]   #即第2列所有行資料
iris[,3]   #即所有列第3行資料
iris[,5]   #即所有列第5行資料

#iris總共有150筆資料，
#假設3/4(即75%)資料為訓練資料集，剩下1/4(即25%)資料就是測試資料集
#則R該如何處理?
#ANS:利用抽樣函數sample()按照比例，3/4訓練用，1/4測試用，
#抽取後不放回方式抽樣就可以達成
set.seed(101)   #設定亂數種子，讓抽樣結果的編號每一次都相同

#統計抽樣sample()協助切割iris資料集，3/4訓練用，1/4測試用
#round()是能夠指定保留幾個小數位數的函數，以四捨五入的規則決定，
#使用的時候可以多給一個參數digits，預設是digits = 0，就是取為整數
index = sample(nrow(iris), round((3/4) * nrow(iris)))
index                          #查看一下有哪些抽樣到的編號

training_data = iris[index,]   #即第index列所有行資料
dim(training_data)             #接著查看訓練資料集維度
training_data

testing_data  = iris[-index,]  #R語言中"-"號就是扣除的意思，
                               #即非第index列所有行資料 
dim(testing_data)              #接著查看測試資料集維度
testing_data

#**********************************************************
#練習題：
# 假設9/10(即90%)資料為訓練資料集，
# 剩下1/10(即10%)資料為測試資料集，R又該如何處理呢?
#ANS:
#index = sample(nrow(iris),round((9/10)*nrow(iris)))
#training_data = iris[index,]   #即第index列所有行資料
#testing_data  = iris[-index,]  #即非第index列所有行資料
#dim(training_data)             #訓練資料集維度
#dim(testing_data)              #測試資料集維度
#**********************************************************

#重要觀念說明
#訓練資料集 vs.測試資料集
#訓練資料集(即建模資料):可以驗證模型的「解釋能力」
#測試資料集:可以驗證模型的「預測能力」

#商業智慧與大數據分析(滄海書局) p.432
#先介紹K最鄰近法(K-Nearest Neighbors)，簡稱KNN法
#KNN是一種不需要建立模型的分類方法，
#由學者Cover和Hart於1968年提出，其概念非常的簡單，
#判斷每筆訓練資料與待分類資料的距離(Distance)，
#再利用和待分類資料最近的K個訓練資料類別當作待分類
#資料的類別。類似人類開會活動中的多數決投票方式確認其類別

#先安裝與下載含有knn()函數的套件class
#KNN原理很簡單沒有繁複計算的原則，假如要預測一個人是什麼分類要怎麼預測呢?
#只要在舊資料中找到跟這一個人最相似(最靠近)的K個人，然後看看跟這個人最靠近
#的鄰居他們各自是什麼分類，用多數決投票，分類的票數比較多的就是那一個人預測
#值的分類，從頭到尾都在計算相似度(或稱靠近的程度)，所以會先將就的資料彼此之
#間靠近的程度都算出來，然後新的人進來，就將這一位新客戶的資料與舊資料之間的距
#離算出來，K=3就會找最靠近的3個人，然後看這3個人倒底他們是什麼分類，然後使用
#多數決，K一旦改變的話則預測效果會不同，K越小計算時間會越長，K越大則計算時間
#會越快。

#install.packages("class")
library(class)

#當資料經過分類技術的歸類後所建立的模型其功能是預測其他尚未分類
#資料的類別值。其模型形式與回歸分析(Regression Analysis)類似
#即Y(應變數) = X1(自變數) + X2 + ... + Xk
#R語法一：模型名稱 = 函數名稱(Y ~ X1 + X2 + … + Xk, 其他參數)
#R語法二：模型名稱 = 函數名稱(Y ~. , 其他參數)

#R語言中knn()函數語法：
#knn(訓練資料集的X, 預測所需的X, 訓練資料集的Y, 最近鄰居數量k)

X.train = training_data[,1:4]
Y.train = training_data[,5]
X.test =  testing_data[,1:4]
Y.test =  testing_data[,5]  

#最後，會以產生一個混淆矩陣(Confusion Matrix)，
#其目的為展現所建構的分類模型之績效的評估結果
# k = 3的意思就是鄰居數量為3
Pred1_KNN = knn(X.train, X.train, Y.train, k = 3)

#接下來就是以訓練dataset(資料的75%)所建構的分類模型
#之預測的混淆矩陣Confusion Matrix的產生 

#以列聯表(又稱交叉表)方式呈現
#table()函數會傳回一個列聯表(或稱交叉表)，
#dnn = 此參數功能可以在列聯表兩邊註明維度的名稱，
#通常是在顯示應變數資料是"實際"的還是"預測"的
mytable1 = table(Y.train, Pred1_KNN, dnn = c("實際", "預測"))  
p_accuracy = sum(diag(mytable1))/sum(mytable1)*100
p_precision_s = mytable1[1,1] / sum(mytable1[,1])
p_precision_c = mytable1[2,2] / sum(mytable1[,2])
p_precision_v = mytable1[3,3] / sum(mytable1[,3])
p_recall_s = mytable1[1,1] / sum(mytable1[1,])
p_recall_c = mytable1[2,2] / sum(mytable1[2,])
p_recall_v = mytable1[3,3] / sum(mytable1[3,])

print(mytable1);cat("\n\n預測正確率 = ",p_accuracy,"% \n");cat("\n\nsetosa Precision = ",p_precision_s*100,"% \n");cat("setosa Recall = ",p_recall_s*100,"% \n")



#測試dataset的分類預測之Confusion Matrix 
Pred2_KNN = knn(X.train, X.test , Y.train, k = 3)

#以交叉表方式呈現
#table()函數會傳回一個列聯表(或稱交叉表)，dnn參數會在結果中顯示的維度名稱
mytable2 = table(Y.test, Pred2_KNN, dnn = c("實際", "預測"))  

p_accuracy = sum(diag(mytable2))/sum(mytable2)*100
print(mytable2);cat("\n\n預測正確率 = ",p_accuracy,"% \n")



#**********************************************************
#R語言輸出函數小工具說明：print() Vs. cat()
#在R語言中，可以使用print()和cat()來輸出結果。
#但這兩種方法有些許差異，分別應用在不同的情況。
#print()只能夠輸出單一個object的內容，常用在
#輸出matrix跟list這兩種資料結構的型態非常的方便，
#不需要再額外定義格式。
#範例1:
print(matrix(c(1,2,3,4), 2 ,2))
#範例2:
x=1;y=2
print("The result of ", x, "plus", y, "is", x+y)
#錯誤，因為要使用範例2上述的方法來印出資料的話，則需使用cat()。
#但是cat()無法印出matrix,list這兩種資料結構變數內容。
cat("The result of ", x, "plus", y, "is", x+y)
cat(list("Hello~", "there"))
#**********************************************************



#將iris分類後，稍微思維一下整個150筆資料的分布狀況
#首先先畫散佈圖
#install.packages("ggplot2")
library(ggplot2)
#花萼長寬分佈圖
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point(aes(color = Species))
#花瓣長寬分佈圖
ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) + geom_point(aes(color = Species))

#如果我們知道亞種(上圖中的顏色)的標準答案，
#不管用花萼或是花瓣好像都可以肉眼分辨出來，
#但如果我們不知道標準答案時，一切就看不出來誰是誰了
ggplot(iris) + geom_point(aes(x = Petal.Length, y = Petal.Width)) 



##############################################################
#決策樹常見的方法有CART、C5.0、C4.5、CHAID等
#而R軟體中支援這些決策樹方法的套件條列如下
#CART決策樹：R可使用tree套件、rpart套件
#C5.0決策樹：R提供C50套件的C5.0函數
#C4.5決策樹：R可使用RWeka套件(J48分類法)
#CHAID決策樹：R可使用CHAID套件的chaid函數，
#PS.CHAID套件目前可以從R-Forge網站下載安裝，尚未正式發佈
##############################################################

#決策樹在R語言中的語法
#先建立模型
#方式一：模型名稱 = 函數名稱(Y ~ X1 + X2 + … + Xk, 其他參數)
#方式二：模型名稱 = 函數名稱(Y ~. , 其他參數)
#一個黑點"."表示全部的變數欄位
#Y若為分類變數或稱類別變數，通常需為Factor型態，
#X1 , X2 , … , Xk例如iris中的前四個欄位，
#分別為Sepal.Length,Sepal.Width,Petal.Length,Petal.Width

#接著透過模型進行預測工作
#Ypred = predict(模型名稱 [, newdata ] [,type="class" ])


#決策樹(Decision Tree)是常用的資料探勘技術，可視為迴歸分析的擴充。
#決策樹可用於分類預測，此類決策樹可稱為分類樹(Classification Tree)。
#這裡要舉例的分類迴歸樹(CART, Classification and Regression Tree)
#是由Brieman在1984年提出，他並於2001年提出Random Forest決策樹。
#在這裡我們先將資料隨機分為90%訓練資料集，10%測試資料集，
#再使用rpart套件中的rpart函數和tree套件中的tree函數來建構CART決策樹：

n=0.1*nrow(iris)  #10%測試資料集則有15筆
test.index=sample(1:nrow(iris),n) #抽樣出10%測試資料集(15筆)的編號
iris.test=iris[test.index,]   #選出10%測試資料集
iris.train=iris[-test.index,] #不是當測試資料集的部分(135筆即90%)都當訓練資料集

#rpart()定義：rpart(formula,data,method,...)
#formula：要使用的屬性，分類變數~屬性欄位1+屬性欄位2+屬性欄位3+...
#data：資料集名稱
#method：用來分析的方法
#例如：rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris.train, method = "class")

#rpart套件
#install.packages("rpart")
library(rpart)
irisTree_1 = rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = 
                     iris.train, method = "class")
irisTree_1    #此為決策樹，但是不是很好閱讀

windows()  #另外開一個畫圖視窗quartz()
plot(irisTree_1) #繪製決策樹圖
text(irisTree_1) #將文字條件放入決策樹圖中(注意畫面調到最大化)  
plot(irisTree_1);text(irisTree_1) 

#tree套件
#install.packages("tree")
library(tree)
irisTree_2 = tree(Species ~ . , data = iris.train)
irisTree_2
windows();plot(irisTree_2);text(irisTree_2)


#C5.0決策樹(R提供C50套件的C5.0函數)
#Quinlan在1986年所提出的ID3演算法後，因其無法處理連續屬性的問題
#且不適用在處理大的資料集，因此1993又發表C5.0的前身4.5，直到現在
#所使用的C5.0決策樹演算法。C5.0演算法的結果可產生決策樹及規則集兩
#種模型，並且依最大資訊增益的欄位來切割樣本，並重複進行切割直到樣
#本子集不能再被分割為止。C5.0能處理連續型變數與類別型的變數資料，
#目標欄位必須是類別型變數。此處依然使用iris資料集來建立模型。

install.packages("C50")
library(C50)
irisC50=C5.0(Species~ . ,data=iris)   #此處將150筆全數建立模型使用
irisC50
summary(irisC50)   #此為決策樹摘要資訊，但是也不是很好閱讀
windows();plot(irisC50)









