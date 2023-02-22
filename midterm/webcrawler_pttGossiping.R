rm(list = ls()) #清除右邊視窗變數
cat("\014")     #清除console視窗內容

# install.packages("httr")
# install.packages("RCurl")
# install.packages("xml2")
# install.packages("rvest")

#此三個網路上爬文相關套件請一起載入
library(httr)
library(RCurl)
library(xml2)
library(rvest)

#據說網頁設計的方法子採取GET() VS. POST()
#兩種不同的方傳導資料，GET方法如同寄明信片般
#的簡單，將資料直接寫在明信片中然後寄出，而
#POST方法是屬於比較完整方式，會先用一張紙張
#事先寫好內容，然後放進信封袋中封存寄出。
#當您發現網頁是採用GET方法建構的，則"網路上"
#建議使用rvest套件爬資料，如果網頁是採用POST
#方法建構的，建議使用httr套件爬資料。
#但切記，不全是絕對的~~!


#****************************************
#以知名社群網站PTT的八卦版資料為例說明
#****************************************

#PTT批踢踢實業坊，網址如下(Try it!)
#https://www.ptt.cc/bbs/index.html


#請試著先學會抓回取得八卦版(Gossiping)的一些貼文
#https://www.ptt.cc/bbs/Gossiping/index39525.html
#2020/03/07 目前八卦版最新網頁的頁碼39525

#在PTT中，一頁貼文"最多"會有20筆標題列(請數一下?)
#建議:初學者，因為多項因素考量(What?)，請勿貪心抓太多資料
#等一下只先抓兩頁標題列資料(從39523到39525)，大約60筆!

#先抓取網址儲存在data變數中
data = NULL
for( i in 39038:39046 ) {
  url  = paste0("https://www.ptt.cc/bbs/Gossiping/index", i, ".html")
  tmp1 = content(GET(url, set_cookies(over18 = 1)))
  tmp2 = xml_find_all(tmp1, "//div[@class='title']/a")  
  tmp3 = xml_attr(tmp2, "href") 
  data = c(data, tmp3)
}

#看看data變數是怎麼一回事，裏頭藏有哪些資訊?
data

#data有是屬於哪一種變數型態呢?ANS:是個文字向量(vector)
class(data)

#總共有多少東西藏在data變數中?
length(data)

#head()函數可以查看前幾筆資料
head(data)

#也可以直接看前十筆資料
data[1:10]


# 把一些空值刪掉，怕抓下來的文章內容狀況不好
data = Filter(function(x) x != "www.ptt.cc", data)
length(data)



myget_doc = function(url)  {
  outDir = "/Users/liuqingxuan/Downloads/大三下課程/資料探勘導論/ptt/" # 設定文章存檔路徑######要改！！！！！
  url2 = paste0("https://www.ptt.cc", url)
  tmp1 = content(GET(url2, set_cookies(over18 = 1))) 
  tmp2 = xml_find_all(tmp1, "//div[@id='main-content']")  
  webtext = xml_text(tmp2)             # 每篇文章的內容 
  name = strsplit(url, '/')[[1]][4]    # 存檔的檔名 ## list有兩個中括號 ex:[[1]]
  write(webtext, paste0(outDir, gsub('html', 'txt', name))) #gsub是取代的意思
}


#最後一個步驟
#使用sapply函數將真實貼文資料抓到outDir
#所指定的路徑中C:/data/1ptt_crawler/
sapply(data, myget_doc)   
#s是簡化版的意思，所以sapply函數就是簡化版的apply函數

#***********************
#恭喜!這樣就有資料了~~ *
#***********************

#趕快去查看一下此路徑"c:/data/1ptt_crawler/"


#**************************************
#小叮嚀：
#myget_doc()是一個使用者自訂函數
#功能拆解如下說明
#**************************************

data

# [1] "/bbs/Gossiping/M.1583566240.A.C6A.html"
# [2] "/bbs/Gossiping/M.1583566265.A.F82.html"
# [3] "/bbs/Gossiping/M.1583566393.A.48B.html"
# [4] "/bbs/Gossiping/M.1583566399.A.0AB.html"
# [5] "/bbs/Gossiping/M.1583566461.A.9C4.html"
# ...


tmp1 = NULL
tmp2 = NULL
tmp3 = NULL


#我們以第一個網頁為範例，說明這個使用這自訂函數
#myget_doc()的每一個步驟是何解呢?
#首先，按照指令的程序
#sapply(data, myget_doc)
#是由sapply()呼叫myget_doc()函數，但是同時要根
#據data內容一個一個網址送過去myget_doc()函數
#而data變數中儲存的內容為剛抓的網址，因此送一個
#data變數中的網址給myget_doc()函數，

url = "/bbs/Gossiping/M.1583566240.A.C6A.html"
url
# [1] "/bbs/Gossiping/M.1583566240.A.C6A.html"

#上述兩條指令也可以這麼寫方便執行與察看結果
(url = "/bbs/Gossiping/M.1583566240.A.C6A.html")
# [1] "/bbs/Gossiping/M.1583566240.A.C6A.html"

# 黏貼成完整網址
(url2 = paste0("https://www.ptt.cc", url))
# [1] "https://www.ptt.cc/bbs/Gossiping/M.1583566240.A.C6A.html"



#GET()函數
(tmp1 = content(GET(url2, set_cookies(over18 = 1))))
#tmp1抓到的內容，這又是什麼?想想看?

# {html_document}
# <html>
# [1] <head>\n<meta http-equiv="Content-Type" content="text/html; charse ...
# [2] <body>\n\t\t\n<div id="topbar-container">\n\t<div id="topbar" clas ...

#ANS:基本上是將網頁標籤結構與網頁上文字資料"一股腦兒"
#    全抓回來，可以透過檢視網頁原始碼對照看一下!


#xml_find_all()函數：可以找到符合「規則」的所有html原始碼。
#此處所說的「規則」是由Xpath所定義的內容(初學者不易判讀~~)
(tmp2 = xml_find_all(tmp1, "//div[@id='main-content']"))

# {xml_nodeset (1)}
# [1] <div id="main-content" class="bbs-screen bbs-content">\n<div class="articl ...


#xml_text()：可以從html原始碼中，萃取「內容」的資訊。
(webtext = xml_text(tmp2))
# [1] "作者hsiao15165 (notdragon)看板Gossiping標題[問卦] 雷句誠是不是也被漫畫之神遺棄了...

#相關學習網站如下
#https://rpubs.com/skydome20/R-Note13-Web-Crawler-on-CIA-CREST-by-xml2



url
# [1] "/bbs/Gossiping/M.1583566240.A.C6A.html"


#strsplit()函數的結果，一定會儲存在
#List資料型態變數中，且會根據"/"這一
#個參數符號，開始進行字串切割，詳情如下
strsplit(url, '/')
# [[1]]
# [1] ""                       
# [2] "bbs"                    
# [3] "Gossiping"              
# [4] "M.1583566240.A.C6A.html"

strsplit(url, '/')[[1]]
# [1] ""                       
# [2] "bbs"                    
# [3] "Gossiping"              
# [4] "M.1583566240.A.C6A.html"

strsplit(url, '/')[[1]][1]
# [1] ""

strsplit(url, '/')[[1]][2]
# [1] "bbs"

strsplit(url, '/')[[1]][3]
# [1] "Gossiping"

strsplit(url, '/')[[1]][4]
# [1] "M.1583566240.A.C6A.html"

name = strsplit(url, '/')[[1]][4]
name
# [1] "M.1583566240.A.C6A.html"

#gsub()是一個很常用的函數，功能為"取代"
gsub('html', 'txt', name)
# [1] "M.1583566240.A.C6A.txt"

#合併執行以上檔名切割與取代的動作
outDir = "/Users/liuqingxuan/Downloads/大三下課程/資料探勘導論/ptt/"
outDir
# [1] "c:/data/1ptt_crawler/"

paste0(outDir, gsub('html', 'txt', name))
# [1] "c:/data/1ptt_crawler/M.1582817452.A.2E9.txt"

#看一下剛剛抓下來的其中一篇文章內容M.1583566240.A.C6A.html
webtext
# [1] "作者EVGA (潮牌)看板Gossiping標題[新聞] 日本群馬縣出現...

#利用write()函數將剛剛webtext文章內容寫到預測路徑中並更改副檔名為.txt
write(webtext, paste0(outDir, gsub('html', 'txt', name)))
#已經將ptt中M.1583566240.A.C6A.html內容寫進去到
#c:/data/1ptt_crawler/目錄下的M.1583566240.A.C6A.txt
#檔案中了，查查看!


#字串切割函數strsplit()
#參考網站
#https://www.cnblogs.com/awishfullyway/p/6601539.html


# 古人云...人一能之，己百之；人十能之，己千之！...
# 作業練習
# 1.抓Gossiping八卦版貼文100筆
# 2.抓ptt中其他版貼文100筆



#*********************************
#Xpath重點說明
#XPath，全稱XML Path Language，
#即XML 路徑語言，它是一門在XML
#文檔中查找信息的語言。XPath 最
#初設計是用來搜尋XML文檔的，但
#是它同樣適用於HTML 文檔的搜索。
#所以在做爬蟲時，我們完全可以使
#用XPath 來做相應的資訊抽取。
#*********************************

# 標記          意義
#========================================
#  //    不管哪一層都要找
#  []    可以對眾多div的描述
#  @     就是attribute向口語的"的"的意思
#  /     此層目標物的下一層

# 使用的套件是xml2，主要會用到下面這些函式：
# read_html()：將網址所對應的html頁面，儲存成一個物件。
# xml_find_all()：找到符合「規則」的所有html原始碼。
# xml_text()：從html原始碼中，萃取「內容」的資訊。
# xml_attr()：從html原始碼中，萃取「屬性」的資訊。


url  = paste0("https://www.ptt.cc/bbs/Gossiping/index", 39523, ".html")
url
# [1] "https://www.ptt.cc/bbs/Gossiping/index39523.html"

GET(url)  #抓到網頁背後的結構資料

# Response [https://www.ptt.cc/ask/over18?from=%2Fbbs%2FGossiping%2Findex39523.html]
# Date: 2020-03-07 16:55
# Status: 200
# Content-Type: text/html; charset=utf-8
# Size: 2.42 kB
# <!DOCTYPE html>
#   <html>
#   <head>
#   <meta charset="utf-8">
#   
#   
#   <meta name="viewport" content="width=device-width, initial-scale=1">
#   
#   <title>批踢踢實業坊</title>
#   
#   ...

#請同時看一下PTT中的"檢視網頁原始碼"，對照看一下，就是"它"了!


#接下來使用content()函數，並且點選有上視窗中的tmp1內容
#就知道content()函數儲存在List中的內容
content(GET(url))
# {html_document}
# <html>
# [1] <head>\n<meta http-equiv="Content-Type" content="text/html; charset=UTF ...
# [2] <body>\n\t\t\n<div class="bbs-screen bbs-content">\n    <div class="ove ...

#儲存在tmp1中
tmp1 = content(GET(url, set_cookies(over18 = 1)))


#翻譯一下
#請在tmp1中找
#//就是每一層都要找過喔
#//div就是div不管在哪一層都要找過喔
#//div[@class='title']在屬性class的值等於title的
#//div[@class='title']/a在屬性class的值等於title且下一層的屬性是a的


#接下來使用xml_find_all()函數，並且點選有上視窗中的tmp2內容
#就知道xml_find_all()函數儲存在List中的內容
xml_find_all(tmp1, "//div[@class='title']/a")
# {xml_nodeset (19)}
# [1] <a href="/bbs/Gossiping/M.1583566240.A.C6A.html">[新聞] 日本群馬縣出現首位武漢肺炎感染者 ...
# [2] <a href="/bbs/Gossiping/M.1583566265.A.F82.html">[問卦] 為何ＩＧ現在一堆廣告?</a>
# [3] <a href="/bbs/Gossiping/M.1583566393.A.48B.html">[問卦] 現在484很適合去帛琉玩</a>
# [4] <a href="/bbs/Gossiping/M.1583566399.A.0AB.html">Re: [問卦] 歐美人薪資憑什麼比東亞人 ...


#儲存在tmp2中
tmp2 = xml_find_all(tmp1, "//div[@class='title']/a")

#使用xml_attr()函數去萃取tmp2中一些「屬性」的資訊
#在此是找屬性為href的值
xml_attr(tmp2, "href")
# [1] "/bbs/Gossiping/M.1583566240.A.C6A.html"
# [2] "/bbs/Gossiping/M.1583566265.A.F82.html"
# [3] "/bbs/Gossiping/M.1583566393.A.48B.html"
# [4] "/bbs/Gossiping/M.1583566399.A.0AB.html"
# [5] "/bbs/Gossiping/M.1583566461.A.9C4.html"
# ...

#儲存在tmp3中
tmp3 = xml_attr(tmp2, "href")

#試著想看看如果改成xml_text(tmp2, "href")會變成怎麼?
#猜對了!就是那些標列中的短短的文字內容
# [1] "[新聞] 日本群馬縣出現首位武漢肺炎感染者"         
# [2] "[問卦] 為何ＩＧ現在一堆廣告?"                    
# [3] "[問卦] 現在484很適合去帛琉玩"                    
# [4] "Re: [問卦] 歐美人薪資憑什麼比東亞人高?"          
# [5] "[問卦] 點九層塔蛋餅的是把九層塔也吃下去嘛?"      
# ...

#您猜對了嗎?


#一步一步解說Xpath結束。

#各位從這一R語言寫的基本爬蟲中學到了什麼?說說看。


#**************************************************************
#簡單爬蟲案例二：靜態網頁的爬蟲練習
#抓取台灣銀行每天的匯率資料
#方法可以先參考下列網站說明
#https://hy-chou.blogspot.com/2018/01/rxpathweb-crawler.html
#
#
#台灣銀行牌告匯率查詢網站(先到這裡觀察觀察網頁結構)
#https://rate.bot.com.tw/xrt?Lang=zh-TW
#*************************************************************

rm(list = ls()) #清除右邊視窗變數
cat('\014')     #清除console視窗內容

# install.packages("httr")
# install.packages("RCurl")
# install.packages("xml2")
# install.packages("rvest")

#此三個網路上爬文相關套件請一起載入
library(httr)
library(RCurl)
library(xml2)
library(rvest)

#只要稍微改寫PTT爬蟲就可以了

tmp1  = NULL
tmp2  = NULL
tmp21 = tmp22 = tmp23 = tmp24 = tmp25 = NULL
tmp3  = NULL
tmp31 = tmp32 = tmp33 = tmp34 = tmp35 = NULL


#首先，想取得匯率資料的網址
url_bktw  = "https://rate.bot.com.tw/xrt?Lang=zh-TW"

#接著透過content()與GET()結合取得整個匯率資料的網頁結構
tmp1 = content(GET(url_bktw))

#然後看看要的資料的位置F12功能鍵或執行網頁檢查功能
#找到您要的網頁資料位置，然後Copy XPath
#看一下"您要的網頁位置的XPath"的語法!

#譬如匯率網頁中美金本行買入29.6元
#牌價最新掛牌時間：2020/03/09 AM 10:00
#取得29.6的原先XPath的語法是
#/html/body/div[1]/main/div[4]/table/tbody/tr[2]/td[2]


#可以使用XPath Helper小工具重新翻譯是否為您要的網頁上資料29.6
#試試看~~
#如何?ANS:...


#後來整理一下不管哪一個國家的本行買入的XPath的語法是
#/html/body/div/main/div/table/tbody/tr/td[2]/text()
#一樣道理使用XPath Helper驗證是否為網頁上本行買入的所有資料
#試試看~~
#如何?ANS:...

#接著要看如何將幣別、本行買入、本行賣出、本行買入、本行賣出
#五個欄位的19筆資料全抓取下來，並儲存在資料框(data.frame)或
#矩陣(matrix)中

tmp21 = xml_find_all(tmp1, "/html/body/div[1]/main/div[4]/table/tbody/tr/td[1]/div/div[3]/text()")
#tmp21 = xml_find_all(tmp1, "//div/table/tbody/tr/td/div/div[3]/text()")

tmp22 = xml_find_all(tmp1, "/html/body/div/main/div/table/tbody/tr/td[2]/text()")
tmp23 = xml_find_all(tmp1, "/html/body/div/main/div/table/tbody/tr/td[3]/text()") 
tmp24 = xml_find_all(tmp1, "/html/body/div/main/div/table/tbody/tr/td[4]/text()") 
tmp25 = xml_find_all(tmp1, "/html/body/div/main/div/table/tbody/tr/td[5]/text()") 


tmp31 = xml_text(tmp21)
tmp31 = gsub("\r\n", "", tmp31)
tmp31 = gsub(" ", "", tmp31)

tmp32 = xml_text(tmp22)
tmp33 = xml_text(tmp23)
tmp34 = xml_text(tmp24)
tmp35 = xml_text
#tmp3 = xml_text(tmp2)

#tmp3 = xml_attr(tmp2, "href") 

#最後成果將各個欄位合併
#可以有兩種做法：矩陣(Matrix)與資料框(data.frame)
#相關技術可以參考網址
#https://joe11051105.gitbooks.io/r_basic/content/variable_and_data/matrix.html
#或
#https://blog.gtwang.org/r/r-lists-and-data-frames/4/


#合併成矩陣
bkrate_M1 = cbind(tmp31,tmp32,tmp33,tmp34,tmp35)

bkrate_M2 = cbind(tmp32,tmp33,tmp34,tmp35)
colnames(bkrate_M2) = c("現金.本行買入","現金.本行賣出","即期.本行買入","即期.本行賣出")
rownames(bkrate_M2) = tmp31

#合併成資料框
bkrate_df1 = data.frame(
  幣別 = tmp31,
  現金.本行買入 = tmp32,
  現金.本行賣出 = tmp33,
  即期.本行買入 = tmp34,
  即期.本行賣出 = tmp35
)


bkrate_df2 = data.frame(
  現金.本行買入 = tmp32,
  現金.本行賣出 = tmp33,
  即期.本行買入 = tmp34,
  即期.本行賣出 = tmp35,
  row.names = tmp31
)


#*********************************************************  
#進階練習
#
#1.請模仿抓PTT貼文方式練習抓目前國內各大電子報新聞資料
#  包含聯合、中時、蘋果、東森、Google新聞、自由等電子報
#  中頭條新聞或焦點新聞。
#
#2.請有空抓一下國內重要人力網站(104 or 1111 or Yes123)
#  中根據某個職缺條件下的職缺資料所需要的條件。
#
#3.請利用抓取四大電子報的頭條或焦點新聞，匯聚成一份每日
#  輿情決策報告(pdf or word or html format)，報告規格至
#  少有(I)時間、(II)標題、(III)摘要以及(IV)超連結等四項
#  ，據此每天提供給主管目前輿情所談論的事情的焦點~~
#
#4.(有趣題目)請上司法院網站去抓取大法官解釋函資料，總共
#  有789號解釋，請試著抓看看，將來可以進一步作為分析大
#  法官解釋爭點的文字主題重點。
#  http://cons.judicial.gov.tw/jcc/modify/wall.html
#  
#  
#  Fighting~
#
#*********************************************************
