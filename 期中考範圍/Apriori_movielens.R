#如果是使用RStudio軟體使用者可以執行
#下列兩行指令快速達成視窗清空的工作
rm(list = ls()) #清除RStudio軟體右邊環境視窗變數內容
cat("\014")     #清除RStudio軟體下方Console Window視窗內容


#資料集(dataset)：參考GroupLens is a research lab. 
#                 的MovieLens 100k(十萬筆電影評分資料)
#download the file from the URL
#https://grouplens.org/datasets/movielens/100k/

## step 1. load arules package
#install.packages("arules")
library(arules)

## step 2. Import and transform data

setwd("/Users/liuqingxuan/Downloads/資料探勘導論")    # Set working directory. Make c:\data directory and copy csv file to the directory
getwd()             # Get working directory

# import raw data (csv file)
nw_data = read.table(file="u.data.csv", header = TRUE, sep = "," )
class(nw_data)      # "data.frame"
mode(nw_data)       # "list"
View(nw_data)
# group data by orderID. using column 1(OrderID) and 2(ProductName) only
nw_temp = tapply(nw_data[,2], nw_data[,1], function(x) paste(x))
nw_temp[1]          # get the first row data
names(nw_temp[1])   # get the name of first row data
nw_temp[[1]]        # get the element of first row data
class(nw_temp)      # "array"
mode(nw_temp)       # "list"

nw2 = vector("list", length(nw_temp)) # length(nw_temp)=830
for (i in seq(nw_temp)) names(nw2)[i] = names(nw_temp[i])
for (i in seq(nw_temp)) nw2[[i]] = nw_temp[[i]]
class(nw2)          # "list"
mode(nw2)           # "list"

# force data into transactions
nw = as(nw2, "transactions")
class(nw)           # "transactions"
mode(nw)            # "S4"

# step 3. analyze data
# generate level plots to visually inspect binary incidence matrices
image(nw) # result - Figure 1 Level plot
summary(nw)

# step 4. find 1-items (L1)
# provides the generic function itemFrequency and the frequency/support for all single items in an objects based on itemMatrix.
itemFrequency(nw, type = "relative") # default: "relative"
itemFrequency(nw, type = "absolute")

# step 5.
# create an item frequency bar plot for inspecting the item frequency distribution for objects based on itemMatrix
itemFrequencyPlot(nw) # result- Figure 2 Item frequency bar plot

# step 6.
# mine association rules
# rules <- apriori(nw) # Mine association rules using default Apriori algorithm
rules1 = apriori(nw, parameter = list(supp = 0.05, target = "maximally frequent itemsets")) # set parameters
#rules2 <- apriori(nw, parameter = list(supp = 0.001, conf = 0.8, target = "rules")) # set parameters
rules2 <- apriori(nw, parameter = list(supp = 0.002, conf = 0.8, target = "rules")) # set parameters
aa=sort(rules2,by="lift")
inspect(aa)

rules22 <- apriori(nw, parameter = list(supp = 0.001, conf = 0.8, target = "rules")) # set parameters
bb=sort(rules22,by="lift")
inspect(bb)
# step7.
# display results
inspect(nw)           # display transactions
inspect(rules1)       # display maximally frequent itemset
inspect(rules2[1:5])  # display association
inspect(rules2)       # display association

#See you next time.