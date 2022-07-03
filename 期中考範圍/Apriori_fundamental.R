#如果是使用RStudio軟體使用者可以執行
#下列兩行指令快速達成視窗清空的工作
rm(list = ls()) #清除RStudio軟體右邊環境視窗變數內容
cat("\014")     #清除RStudio軟體下方Console Window視窗內容

#範例一
# table 14.1, Transactional data, 商業智慧與大數據分析(滄海書局) Page.457
# Items商品項目 : 飯糰、豆漿、尿布、啤酒、麥片、綠茶(有六種不同商品項目)
# dataset資料集合: total data = 5(有五筆交易資料)
# 最小支持度(minimun support, minsup) = 60%
# 最小信賴度(minimun confidence, minconf) = 60%
#|------------------|------------------------|
#| Transaction ID   |     Items              |
#|------------------|------------------------|
#|       T1         | {飯糰,豆漿,尿布}       |
#|       T2         | {飯糰,尿布,啤酒,麥片}  |
#|       T3         | {豆漿,尿布,啤酒,綠茶}  |
#|       T4         | {飯糰,豆漿,尿布,啤酒}  |
#|       T5         | {飯糰,豆漿,尿布,綠茶}  |
#|------------------|------------------------|


#*****************
#步驟一(Step 1.) *
#*****************
#安裝(install)與載入(load)"arules" 套件
#install.packages("arules") #如果您電腦已經安裝過此行指令可以不用再執行
library(arules)            #載入arules套件



#*****************
#步驟二(Step 2.) *
#*****************
#準備資料prepare data

#透過list()函數是將T1,T2,...,T5的交易內容串接建立成
#為list資料型態的變數並以xlist命名此變數

#c()函數就是combine的意思，可以將前後資料串接起來成
#為向量vector資料型態

#所以最後xlist中有五個文字向量，語法如下：

xlist = list(
  c("飯糰","豆漿","尿布"),
  c("飯糰","尿布","啤酒","麥片"),
  c("豆漿","尿布","啤酒","綠茶"),
  c("飯糰","豆漿","尿布","啤酒"),
  c("飯糰","豆漿","尿布","綠茶")
)
#其實上面指令也可以寫成一行，可以不用多行撰寫方式
#，但是一行指令太長會比較不容易閱讀
#xlist_test = list(c("飯糰","豆漿","尿布"),c("飯糰","尿布","啤酒","麥片"),c("豆漿","尿布","啤酒","綠茶"),c("飯糰","豆漿","尿布","啤酒"),c("飯糰","豆漿","尿布","綠茶"))



#可以查看一下目前xlist變數資料型態
class(xlist)
#可以查看一下目前xlist變數資料內容，list型態在R中使用兩層中括號
xlist
xlist[[1]]     #"飯糰","豆漿","尿布"
xlist[[2]]     #"飯糰","尿布","啤酒","麥片"
xlist[[3]]     #"豆漿","尿布","啤酒","綠茶"
xlist[[4]]     #"飯糰","豆漿","尿布","啤酒"
xlist[[5]]     #"飯糰","豆漿","尿布","綠茶"
xlist[[1]][1]  #"飯糰"
xlist[[1]][2]  #"豆漿"
xlist[[1]][3]  #"尿布"
xlist[[1]][4]  #NA表示沒資料，超出範圍所以沒資料


#但是沒有交易編號，所以用names函數設定，set transaction names
#paste()可以連前後數字或文字字串
#c(1:5)就是建立一個數字向量內容為1,2,3,4,5
#sep參數就是設定前("T")後(c(1:5))資料之間粘貼時候是用什麼隔離的方式
names(xlist) = paste("T",c(1:5), sep = "") 
xlist

#force data into transactions
#因為在R中關聯規則使用的apriori()函數的資料格式為"transactions"，
#因此採用as()函數強迫將list資料型態的xlist變數轉為"transactions"
#資料格式#儲存在另一變數table14_1中

table14_1 = as(xlist, "transactions") #transactions的雙引號一定要加 


#查看目前table14_1的內容
table14_1   #只會看到摘要資訊，例如幾筆資料，幾個欄位

#使用inspect()函數可以看到真正詳細資料
inspect(table14_1)
#"transactions"資料格式中，item先出現，transactionID後出現，欄位顛倒擺


#使用str()函數順便查看table14_1變數目前變數結構
str(table14_1)



#*****************
#步驟三(Step 3.) *
#*****************
# 開始進行資料分析(analyze data)
# 先使用一些視覺化工具看資料輪廓generate level plots to visually inspect binary incidence matrices

image(table14_1)    
#透過資料密度圖查看資料密度，問一下自己這圖有很稀疏呢?還是很緊密呢?

summary(table14_1)  
#一般統計資料，記得看density value是多少?
#ANS:0.6333333  與圖一致嗎?(可以手動驗算一下)
#element (itemset/transaction) length distribution: 3 1代表長度為3的出現1筆 4 4代表長度為4的出現4筆


#*****************
#步驟四(Step 4.) *
#*****************
# 可尋找到apriori理論方法中所提到的1-itemset(C1)
itemFrequency(table14_1, type = "relative") #以支持度(support)呈現 #這裡代表五筆資料中，該項目出現的機率
itemFrequency(table14_1, type = "absolute") #以支持數量(support count)呈現


#這一段可以儲存這些1-itemset的支持度或者支持數量資料
#itemFreq = itemFrequency(table14_1)
#每個項集transaction包含item的個數
#Size = size(table14_1)
#每個item出現的次數
#itemCount = (itemFreq/sum(itemFreq)*sum(Size))



#*****************
#步驟五(Step 5.) *
#*****************
# 以長條圖(bar plot)繪製itemFrequency資料
itemFrequencyPlot(table14_1, col = "lightgreen")         #有顏色
itemFrequencyPlot(table14_1, support = 0.4, col = "red") #加support條件
itemFrequencyPlot(table14_1, topN = 4, col = "lightblue")#加topN條件



#*****************
#步驟六(Step 6.) *
#*****************
# 開始進行找出關聯規則
# Mining Association Rules，
#以min_supp = 0.6, min_conf = 0.6為條件計算找出規則(target = "rules")
#規則中的前項與後項中至少都一個品項(都不能是空集合{})，
#即規則長度為2(minlen=2)以上的才留下
rules_1 = apriori(table14_1, parameter = list(support = 0.6, 
                                              confidence = 0.6,
                                              minlen=2, #單筆資料一定要至少2個項目
                                              target = "rules"))
#ANS:writing ... [11 rule(s)] done [0.00s].  找到11條規則
#使用inspect()函數查看一下有哪些關聯規則產生
inspect(rules_1)
#a = subset(rules_1, subset = lift > 0.95) #過濾lift值
#inspect(a)

#如果剛剛上述的所有參數都沒寫，R就採用內定的預設值，
#試一下!找找看有哪些認識的預設值~~
rules_2 = apriori(table14_1)  
#ANS:writing ... [36 rule(s)] done [0.00s].  找到36條規則
#使用inspect()函數查看一下有哪些關聯規則產生
inspect(rules_2)



#****************
#步驟七(Step7.) *
#****************
# 顯示所產生的關聯規則的模型結果，看規則詳細內容使用inspect()函數

inspect(table14_1) # display transactions  again

inspect(rules_1)   # display association rules in rules_1
#各位可以調整參數重新產生rules_1，
#lift值為提升度，count值為支持數量
#一直調整修正到大數據分析師與決策者BOSS找出有趣的強規則答案


inspect(rules_2)   # display association rules in rules_2
#通常實務上不建議跑沒任何參數條件的apriori方法


#Ending 完成關聯規則apriori演算法R實作


#Practice
#練習題1
# 最小支持度(minimun support, minsup)=50%
# 最小信賴度(minimun confidence, minconf)=50%
#|------------------|-------------|
#| Transaction ID   |     Items   |
#|------------------|-------------|
#|       T100       | {1,3,4}     |
#|       T200       | {2,3,5}     |
#|       T300       | {1,2,3,5}   |
#|       T400       | {2,5}       |
#|------------------|-------------|


#練習題2
# 最小支持度(minimun support, minsup)=30%
# 最小信賴度(minimun confidence, minconf)=70%
#|------------------|-------------|
#| Transaction ID   |     Items   |
#|------------------|-------------|
#|       1          | {2,5,7}     |
#|       2          | {1,3,4,6}   |
#|       3          | {2,6,7}     |
#|       4          | {2,4,5}     |
#|       5          | {3,6}       |
#|       6          | {2,4,6}     |
#|       7          | {1,4,5}     |
#|       8          | {1,3,5}     |
#|       9          | {2,3,5}     |
#|      10          | {1,3,5}     |
#|------------------|-------------|


#練習題3
# 最小支持數量(minimun support count, minsup_count)=2
# 最小信賴度(minimun confidence, minconf)=70%
#|------------------|---------------|
#| Transaction ID   |     Items     |
#|------------------|---------------|
#|       T100       | {I1,I2,I5}    |
#|       T200       | {I2,I4}       |
#|       T300       | {I2,I3}       |
#|       T400       | {I1,I2,I4}    |
#|       T500       | {I1,I3}       |
#|       T600       | {I2,I3}       |
#|       T700       | {I1,I3}       |
#|       T800       | {I1,I2,I3,I5} |
#|       T900       | {I1,I2,I3}    |
#|------------------|---------------|
plist_3<-list(c("I1","I2","I5"),
              c("I2","I4"),
              c("I2","I3"),
              c("I1","I2","I4"),
              c("I1","I3"),
              c("I2","I3"),
              c("I1","I3"),
              c("I1","I2","I3","I5"),
              c("I1","I2","I3"))
names(plist_3)<-paste("T",c(100,200,300,400,500,600,700,800,900),sep = "")
ptable_3<-as(plist_3, "transactions")
summary(ptable_3)
rules3<-apriori(ptable_3, parameter = list(support = 0.222,#(2/9=0.222)
                                           confidence = 0.7,
                                           minlen = 2,
                                           target = "rule"))
inspect(rules3)
rules3_df<-data.frame(inspect(rules3))
View(rules3_df)

#練習題4
# 最小支持數量(minimun support count, minsup_count)=3
# 最小信賴度(minimun confidence, minconf)=50%
#|------------------|-------------------------------|
#| Transaction ID   |         Items                 |
#|------------------|-------------------------------|
#|       1          | {Bread, Milk}                 |
#|       2          | {Bread, Diaper, Beer,   Eggs} |
#|       3          | {Milk,  Diaper, Beer,   Coke} |
#|       4          | {Bread, Mile,   Diaper, Beer} |
#|       5          | {Bread, Mile,   Diaper, Coke} |
#|------------------|-------------------------------|



#練習題5
# 最小支持數量(minimun support count, minsup_count)=3
# 最小信賴度(minimun confidence, minconf)=50%
#|------------------|-------------------|
#| Transaction ID   |       Items       |
#|------------------|-------------------|
#|       1          | {f,a,c,d,g,i,m,p} |
#|       2          | {a,b,c,f,l,m,o}   |
#|       3          | {b,f,h,j,o}       |
#|       4          | {b,c,k,s,p}       |
#|       5          | {a,f,c,e,l,p,m,n} |
#|------------------|-------------------|





