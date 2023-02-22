#用Apriori、FP Growth、eclat演算法進行關聯分析時中，
#常常用到Groceries資料集
#Groceries資料集是arules套件安裝載入後才會出現
#Groceries的資料格式已經是apriori方法直接可以使用的格式
#不需要額外透過as()函數轉成"transactions"格式
#該資料集是某個雜貨店一個月真實的交易記錄，共有9835筆消費記錄，169種商品
#每一列(row)代表一筆交易所購買的產品（item）
#citrus fruit,semi-finished bread,margarine,ready soups
#tropical fruit,yogurt,coffee
#whole milk
#pip fruit,yogurt,cream cheese,meat spreads
#other vegetables,whole milk,condensed milk,long life bakery product
#whole milk,butter,yogurt,rice,abrasive cleaner
#rolls/buns
#other vegetables,UHT-milk,rolls/buns,bottled beer,liquor (appetizer)
#potted plants
#whole milk,cereals
#tropical fruit,other vegetables,white bread,bottled water,chocolate
#......
#資料轉換：
#建立稀疏矩陣(Why?)，
#每個Item一行(column)，每一列(row)代表一個transaction。
#1表示該transaction購買了該item，0表示沒有購買。
#當然，data frame是比較直觀的一種資料結構，
#但是一旦item比較多的時候，這個data frame的大多數單元格的值為0，
#大量浪費記憶體。所以，R引入了特殊設計的稀疏矩陣，
#僅存1，節省記憶體。
#arules套件函數read.transactions()可以讀入csv資料並建立稀疏矩陣。

#如果是使用RStudio軟體使用者可以執行
#下列兩行指令快速達成視窗清空的工作
rm(list = ls()) #清除RStudio軟體右邊環境視窗變數內容
cat("\014")     #清除RStudio軟體下方Console Window視窗內容

# setwd("C:/data")
# data_gro_csv = read.transactions("groceries.csv", format="basket", sep=",")
# basketSize = size(Groceries)
# groceries_2 = data_gro_csv[basketSize > 2]

#install.packages("arules")
library(arules)
data("Groceries")
summary(Groceries)
class(Groceries)
nrow(Groceries)
ncol(Groceries)
dim(Groceries)
size(Groceries)

str(Groceries)

Groceries
inspect(Groceries[1:6])

r0=apriori(Groceries, parameter = list(support=0.001,confidence=0.5))
r0
class(r0)
summary(r0)
inspect(r0[1:10])

r1=apriori(Groceries,parameter = list(support=0.005,confidence=0.5))
r1
inspect(r1[1:10])

r2=apriori(Groceries,parameter = list(support=0.005,confidence=0.6))
r2
inspect(r2)

r3=apriori(Groceries,parameter = list(support=0.005,confidence=0.64))
r3
inspect(r3)

r4_sorted_supp=sort(r0,by="support")
r4_sorted_supp
inspect(r4_sorted_supp[1:10])

r5_sorted_conf=sort(r0,by="confidence")
r5_sorted_conf
inspect(r5_sorted_conf[1:10])

r6_sorted_lift=sort(r0,by="lift")
r6_sorted_lift
inspect(r6_sorted_lift[1:10])

#假設芥末(mustard)為冷門商品，想要找出有可能的強關聯規則中有芥末，
#以利後續進行商品組合跟芥末綑綁再一起，可如此做
r7=apriori(Groceries,parameter = list(maxlen=2,support=0.001,confidence=0.1),appearance = list(rhs="mustard",default="lhs")) #設定條件rhs只要mustard
r7
inspect(r7)
#美乃滋/蛋黃醬(mayonnaise)是芥末(mustard)的強關聯商品

r8_itemset_apriori=apriori(Groceries,parameter = list(support=0.001,target="frequent itemsets"),control=list(sort=-1))
r8_itemset_apriori
inspect(r8_itemset_apriori[1:5])
#銷售前五名的商品(top 5)全脂牛奶、蔬菜、麵包卷、蘇打、優乳


r9_itemset_eclat=eclat(Groceries,parameter = list(minlen=1,maxlen=3,support=0.001,target="frequent itemsets"),control=list(sort=-1))
r9_itemset_eclat
inspect(r9_itemset_eclat[1:5])


#關聯規則視覺工具
#install.packages("arulesViz")
library(arulesViz)
r10=apriori(Groceries,parameter = list(support=0.002,confidence=0.5))
r10
windows() #quartz()
plot(r10)
#此為模型散點圖
plot(r10,measure = c("support","lift"),shading = "confidence")
plot(r10,interactive = TRUE)

plot(r10,shading = "order",control = list(main="TWO KEY PLOT"))

windows()
plot(r10,method = "grouped")
windows()
plot(r10[1:50],method = "grouped")
windows()
plot(r10[1:50],method = "matrix",measure = "lift")
windows()
plot(r10[1:50],method = "matrix3D",measure = "lift")
windows()
plot(r10[1:50],method = "paracoord")

rule1 = apriori(Groceries, parameter = list(support = 0.006,confidence = 0.3))
summary(rule1)
inspect(rule1[1:5])

#執行結果如下，
#> inspect(rule1[1:5])
#lhs             rhs                support     confidence lift     count
#[1] {pot plants} => {whole milk}       0.006914082 0.4000000  1.565460 68 
#[2] {pasta}      => {whole milk}       0.006100661 0.4054054  1.586614 60   
#[3] {herbs}      => {root vegetables}  0.007015760 0.4312500  3.956477 69   
#[4] {herbs}      => {other vegetables} 0.007727504 0.4750000  2.454874 76   
#[5] {herbs}      => {whole milk}       0.007727504 0.4750000  1.858983 76
#lhs=>rhs 代表買左邊也會買右邊的意思，
#而支持度與信賴度，則分別代表了普遍性與信心水準。
#例如，第一個關聯規則可以解釋為：
#「若某人單次消費購買的是pot plants => 也會購買whole milk」
#而lift=1.5 > 1，表示了這個規則相當具有正相關。
#除此之外，我們也可以針對特定商品產生關連規則。
#例如: 用yogurt這個產品來產生一個相關聯的rules
#其中%pin%"yog"，代表的意思是尋找找商品名稱有 “yog"的品項，
yogurtr1 = subset(rule1,items %pin% "yog")
summary(yogurtr1)
inspect(yogurtr1[1:15])

#補充：
#items %in% c("A", "B")表示 lhs+rhs的項集並集中，至少有一個item是在c("A", "B")。  #item = A or item = B
#如果僅僅想搜尋lhs或者rhs，那麼用lhs或rhs替換items即可。
#如：lhs %in% c("yogurt")

#%in%是精確匹配
#%pin%是部分匹配，也就是說只要item like '%A%' or item like '%B%'
#%ain%是完全匹配，也就是說itemset has ’A' and itemset has ‘B'
#同時可以透過條件運算子&(且), |(或), !(反/非)
#來豐富support, confidence, lift的過濾條件。

#關於關聯分析就先講到這裡啦，在資料的世界裡，
#有太多太多關聯等著我們去挖掘，每個細微的地方，
#都可能隱藏著商機，這也是為甚麼有越來越多公司花
#大錢也要進行資料探勘，常見的電影推薦、購物網站
#商品推薦，其實都是關聯分析的運用。

#See you next time.