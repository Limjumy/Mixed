##library(showtext)
### read data
sanguo = readLines("三国志.txt")
#### cut paragraph
para_head = grep("\\s+", sanguo)
cut_para1 = cbind(para_head[1:(length(para_head)-1)], para_head[-1]-1)
sanguo = sapply(1:nrow(cut_para1), function(i) paste(sanguo[cut_para1[i,1]:cut_para1[i,2]], collapse = ""))
sanguo[1:10]
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
segmentCN("张飞来也，刘备与关羽煮酒论英雄，为备将张飞所拒，引还南郑。拜荡寇将军。刘备屯阳，张角起，以植为北中郎将征角，失利抵罪。顷之，复以为尚书。张让劫少帝奔小平津，")

sanguo_split2<-segmentCN(sanguo)
#sanguo_split2<-sapply(sanguo_split2, function(x) x[[1]])
#str(sanguo_split2)
#mode(sanguo_split2)
#is.vector(sanguo_split2)
writeLines(unlist(sanguo_split2), "sanguo_split2.txt")
#write.table(sanguo_split2, "sanguo_split2.txt")
#help("writeLines")
####unlist(sanguo_split2)####正确！！

#### read in main roles
roles = readLines("三国主角名单.txt")
#### density of different names of one role 
rolesl = strsplit(roles, " ")

#### train word2vec
library(wordVectors)
model = train_word2vec("sanguo_split2.txt", output="sanguo_split2.bin", 
                       threads = 3, vectors = 100, window=12, force = T)
vec = read.vectors("sanguo_split.bin")
length(vec)
nrow(vec)
ncol(vec)
rr = sapply(rolesl, function(x) x[1])
#vec["郭嘉",],vec 
cos_dist = cosineDist(vec[rr,],vec[rr,])
hc = hclust(as.dist(cos_dist), method = "average")
plot(hc)
