library(arules)

setwd("/Users/pennychen/資料探勘/") 
getwd()

nw_data = read.table(file="電影評分數據.csv", header = TRUE, sep = "," )
View(nw_data)


nw_temp<-tapply(nw_data[,2], nw_data[,1], paste)
nw_temp

nw2<-vector("list",length(nw_temp))
for (i in seq(nw_temp)) names(nw2)[i] = names(nw_temp[i])
for (i in seq(nw_temp)) nw2[[i]] = nw_temp[[i]]
class(nw2)          

movie<-as(nw2, "transactions") 

image(movie)
summary(movie)

itemFrequency(movie, type = "relative") 
itemFrequency(movie, type = "absolute")

itemFrequencyPlot(movie)

rule_s_5<-apriori(movie, parameter = list(support = 0.4,
                                       target = "maximally frequent itemsets"))
inspect(rule_s_5)

rule_s_3<-apriori(movie, parameter = list(support = 0.3,
                                          target = "maximally frequent itemsets"))

rule1_300<-subset(rule_s_3, count >=300)
count300<-data.frame(inspect(rule1_300))
View(count300)

write.csv(count300,file = "/Users/pennychen/資料探勘/count300.csv", sep = ",")


rule2_4<-apriori(movie, parameter = list(support = 0.4,
                                       confidence = 0.8,
                                       minlen = 2,
                                       target = "rules"))
inspect(rule2_4)

rule2_3<-apriori(movie, parameter = list(support = 0.3,
                                         confidence = 0.8,
                                         minlen = 2,
                                         target = "rules"))

rule2_3<-subset(rule2_3, lift >= 2)
inspect(rule2_3)


