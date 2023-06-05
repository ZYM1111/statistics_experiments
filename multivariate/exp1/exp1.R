H.clust <- function(X, d = "euc", m = "comp", proc = F, plot = T)
{
  D = dist(X, d)
  hc <- hclust(D, m)
  if (proc) { cat("\n cluster procdure: \n");
    print(cbind(hc$merge, hc$height)) }
  PROC = cbind(merge = hc$merge, height = hc$height)
  if (proc) print(PROC)
  if (plot) plot(hc, ylab = d, main = m)
  #plot(hc,hang=hang,xlab="",ylab="",main="")
  #hc1=as.dendrogram(hc)
  #plot(hc1,xlab="G",ylab="D",horiz=TRUE)
  #list(D=D,hc=hc,proc=proc)
  return(hc)
}







d3.1=read.xlsx('mvstats5.xlsx','d3.1',rowNames=TRUE);d3.1 #读取数据
plot(d3.1,gap=0) #矩阵散点图
D=dist(d3.1);D
plot(hclust(D,'single')) #最短距离法
plot(hclust(D,'complete')) #最长距离法
plot(hclust(D,'median')) #中间距离法
plot(hclust(D,'average')) #类平均法
plot(hclust(D,'centroid')) #重心法
plot(hclust(D,'ward.D')) #ward.D法
plot(hclust(D,'ward.D2')) #ward.D2法

H=hclust(D,'ward.D2');H
plot(H); rect.hclust(H,2); cutree(H,2) #分两类
plot(H); rect.hclust(H,3); cutree(H,3) #分三类
plot(H); rect.hclust(H,4); cutree(H,4) #分四类

set.seed(123) #设定种子数
#产生均值0,标准差为0.3的100x10的正态随机数矩阵
x1=matrix(rnorm(1000,0,0.3),ncol=10)
#产生均值1,标准差为0.3的100x10的正态随机数矩阵
x2=matrix(rnorm(1000,1,0.3),ncol=10)
#形成200x10的正态随机数矩阵
X=rbind(x1,x2); #按行合并
summary(X) #基本统计

H=hclust(dist(X)) #系统聚类
plot(H); rect.hclust(H,2); #分两类
cutree(H,2) #画两类框
#——7.4快速聚类法----
km=kmeans(X,2)     #kmeans聚类
km$cluster         #分类结果
par(mar=c(4,4,1,1)) #设置画图参数
plot(X,pch=km$cluster) #画分类图

set.seed(123) #设定种子数
x1=matrix(rnorm(10000,0,0.25),ncol=10) #生成矩阵1
x2=matrix(rnorm(10000,1,0.25),ncol=10) #生成矩阵2
X=rbind(x1,x2) #按行合并
km=kmeans(X,2) #kmeans聚类
kc=km$cluster;kc #kmeans聚类结果
plot(X,pch=kc,col=kc) #画聚类结果图

km=kmeans(d3.1,3)     #kmeans聚类
km$cluster         #分类结果

kmeans