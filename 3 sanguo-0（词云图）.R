library(wordcloud)
library(RColorBrewer)
library(chinese.misc)
library(Rwordseg) 
##优化词库http://pinyin.sogou.com/dict/
installDict("E:\\R\\文本挖掘\\三国志.scel","sanguozhi1")
installDict("E:\\R\\文本挖掘\\真 三国史.scel","sanguozhi2")
installDict("E:\\R\\文本挖掘\\比较全的三国人名.scel","sanguozhi3")
installDict("E:\\R\\文本挖掘\\三国人名.scel","sanguozhi4")
installDict("E:\\R\\文本挖掘\\三国.scel","sanguozhi5")
installDict("E:\\R\\文本挖掘\\三国策录将词库.scel","sanguozhi6")
installDict("E:\\R\\文本挖掘\\三国典故.scel","sanguozhi7")
segmentCN("曹操与刘备大战司马懿")

#读入数据  
lecture<-read.csv("三国志.txt",stringsAsFactors=FALSE,header=FALSE)
res=lecture[]

words=unlist(lapply(X=res, FUN=segmentCN))
#unlist将list类型的数据，转化为vector
#lapply()返回一个长度与X一致的列表，每个元素为FUN计算出的结果，且分别对应到X中的每个元素。
word=lapply(X=words, FUN=strsplit, " ") 
v=table(unlist(word))
#table统计数据的频数
# 降序排序  
v=rev(sort(v))
##创建数据框
d<-data.frame(v)##d<-data.frame(词汇=d$Var1,词频=d$Freq)
#过滤掉1个字和词频小于200的记录  
d<-subset(d, nchar(as.character(d$Var1))>1 & d$Freq>=50)  
#输出结果
write.csv(d, file="sanguozhi.csv",row.names=FALSE) 


##画出标签云
#1 读入文件
mydata<-read.csv("sanguozhi.csv",header = T)
##2 设置字体和颜色
mycolors <- brewer.pal(12,"Paired")
windowsFonts(myFont=windowsFont("华文隶书"))
##3 画标签云
wordcloud(mydata$Var1,mydata$Freq,random.order=FALSE,random.color=TRUE,colors=mycolors,family="myFont")


