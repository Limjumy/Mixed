##library(showtext)
### read data
sanguo = readLines("三国志－白话版.txt")
#### cut paragraph
para_head = grep("\\s+", sanguo)
cut_para1 = cbind(para_head[1:(length(para_head)-1)], para_head[-1]-1)
sanguo = sapply(1:nrow(cut_para1), function(i) paste(sanguo[cut_para1[i,1]:cut_para1[i,2]], collapse = ""))
sanguo[1:10]
library(wordcloud)
library(rJava)
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

sanguo_split3<-segmentCN(sanguo)

writeLines(unlist(sanguo_split3), "sanguo_split3.txt")
#write.table(sanguo_split2, "sanguo_split2.txt")
#help("writeLines")
####unlist(sanguo_split2)####正确！！

#### read in main roles
roles = readLines("三国主角名单.txt")######此文件最后不能有空格，会在rr中变成NA空值
#### density of different names of one role 
rolesl = strsplit(roles, " ")

#### train word2vec
library(wordVectors)
#model = train_word2vec("sanguo_split3.txt", output="sanguo_split3_cbow.bin", 
#                       threads = 3, vectors = 100, window=12, force = T,cbow=1,negative_samples = 5)

model = train_word2vec("sanguo_split3.txt", output="sanguo_split3_skipgram.bin", 
                                              threads = 3, vectors = 100, window=12, force = T,cbow=0,negative_samples = 5)
vec = read.vectors("sanguo_split3_skipgram.bin")


rr = sapply(rolesl, function(x) x[1])
#vec["郭嘉",],vec 
cos_dist = cosineDist(vec[rr,],vec[rr,])
hc = hclust(as.dist(cos_dist), method = "average")
plot(hc)

help(train_word2vec)
closest_to(model, vec[["曹操"]], n = 100, fancy_names = TRUE)


nearest_to(model, model[["曹操"]], n = 10)

nearest_to(model,model[["曹操"]])

closest_to(model,vec[["诸葛亮"]],n=10,fancy_names = TRUE)

nearest_to(model,model[["赵云"]])
nearest_to(model,model[["刘禅"]])
nearest_to(model,model[["庞统"]])
nearest_to(model,model[["魏延"]])
nearest_to(model,model[["孙权"]])
nearest_to(model,model[["孙策"]])
nearest_to(model,model[["周瑜"]])
nearest_to(model,model[["陆逊"]])
nearest_to(model,model[["吕蒙"]])
nearest_to(model,model[["鲁肃"]])
nearest_to(model,model[["董卓"]])
nearest_to(model,model[["吕布"]])

nearest_to(model,model[["王允"]])
nearest_to(model,model[["马腾"]])
nearest_to(model,model[["马超"]])
nearest_to(model,model[["法正"]])
nearest_to(model,model[["张松"]])

nearest_to(model,model[["司马懿"]])
nearest_to(model,model[["曹丕"]])
nearest_to(model,model[["曹真"]])
nearest_to(model,model[["曹爽"]])
nearest_to(model,model[["荀彧"]])
nearest_to(model,model[["荀攸"]])

nearest_to(model,model[["张辽"]])
nearest_to(model,model[["刘备"]])

nearest_to(model,model[["关羽"]])
nearest_to(model,model[["张飞"]])

nearest_to(model,model[["黄忠"]])
nearest_to(model,model[["姜维"]])


