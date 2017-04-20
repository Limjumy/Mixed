
library(chinese.misc)
a_new='hehe' #If you want to change folder name, just change this.
gwd=getwd()
f=paste(gsub('/$', '', gwd), a_new, sep='/')
if (dir.exists(f)) stop ('Folder already exists. Please change a name.')
dir.create(f)
dir.create(paste(f, 'f1', sep='/'))
dir.create(paste(f, 'f2', sep='/'))
x='以事件为选题的数据新闻最常出现在重大新闻事件的报道中。在这类事件中，数据报道可能是媒体精心制作的报道主体，也可能是媒体对事件的整个专题报道中的一个有机组成部分。可预见的重大新闻事件一般多指会议、活动、庆典或赛事，媒体可以把较为充足的时间投入到选题策划中。除了可预见的重大新闻事件以外，更多此类数据新闻的选题是突发新闻事件。近年来，越来越多的媒体将数据新闻运用于突发新闻事件的报道中，大量数据资源的整合和运用为此类新闻报道增添了更多科学性。'
write.table(x, paste(f, 'f1/d1.txt', sep='/'), row.names=FALSE, col.names=FALSE, quote=FALSE, fileEncoding='UTF-8')
x='人们对数据可视化的适用范围有着不同观点。例如，有专家认为数据可视化是可视化的一个子类目，主要处理统计图形、抽象的地理信息或概念型的空间数据。现代的主流观点将数据可视化看成传统的科学可视化和信息可视化的泛称，即处理对象可以是任意数据类型、任意数据特性，以及异构异质数据的组合。大数据时代的数据复杂性更高，如数据的流模式获取、非结构化、语义的多重性等。'
write.table(x, paste(f, 'f1/d2.txt', sep='/'), row.names=FALSE, col.names=FALSE, quote=FALSE, fileEncoding='UTF-8')
x='政治传播学是政治学与传播学的交叉学科，它是对政治传播现象的总结和政治传播规律的探索和运用，它包括政治传播的结构、功能、本质及技巧等方方面面。它的研究范围包括：政治传播行为，即政治传播的主体、客体及他们之间的相互关系体系；政治传播内容，即对政治的信息处理体系；政治传播途径，即政治符号和传播媒介体系；政治传播环境，即政治传播与相关社会现象；政治传播形态，即政治传播本体的形貌或表现体系。'
write.table(x, paste(f, 'f2/d3.txt', sep='/'), row.names=FALSE, col.names=FALSE, quote=FALSE, fileEncoding='GB18030')
x='改进社会治理方式。坚持系统治理，加强党委领导，发挥政府主导作用，鼓励和支持社会各方面参与，实现政府治理和社会自我调节、居民自治良性互动。坚持依法治理，加强法治保障，运用法治思维和法治方式化解社会矛盾。坚持综合治理，强化道德约束，规范社会行为，调节利益关系，协调社会关系，解决社会问题。坚持源头治理，标本兼治、重在治本，以网格化管理、社会化服务为方向，健全基层综合服务管理平台，及时反映和协调人民群众各方面各层次利益诉求。'
write.table(x, paste(f, 'f2/d4.txt', sep='/'), row.names=FALSE, col.names=FALSE, quote=FALSE, fileEncoding='GB18030')
x='所有这三种活动和它们的相应境况都与人存在的最一般状况相关：出生和死亡，诞生性和有死性。劳动不仅确保了个体生存，而且保证了类生命的延续。工作和它的产物——人造物品，为有死者生活的空虚无益和人寿的短促易逝赋予了一种持久长存的尺度。而行动，就它致力于政治体的创建和维护而言，为记忆，即为历史创造了条件。'
write.table(x, paste(f, 'd5.txt', sep='/'), row.names=FALSE, col.names=FALSE, quote=FALSE, fileEncoding='UTF-8')

##用dir(f)可以收集这个文件里的文件名
dir(f)
