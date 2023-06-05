##距离计算 dist()

x1 = c(5, 7, 3, 6, 6)
x2 = c(7, 1, 2, 5, 6)
X = cbind(x1, x2)
dist(X)    ##默认的是eulcidean 距离
dist(X, diag = TRUE)
dist(X, upper = TRUE)

# 注释：在聚类中求两点的距离有：

# 1，绝对距离：manhattan

# 2，欧氏距离：euclidean 默认

# 3，闵科夫斯基距离：minkowski

# 4，切比雪夫距离：chebyshev

# 5，马氏距离：mahalanobis

# 6，蓝氏距离：canberra

dist(X, method = 'manhattan') ##绝对值距离
dist(X, method = 'minkowski', p = 1) ##绝对值距离
dist(X, method = 'minkowski', p = 2)


###系统聚类法
# 1，类平均法：average

# 2，重心法：centroid

# 3，中间距离法:median

# 4，最长距离法：complete 默认

# 5，最短距离法：single

# 6，离差平方和法：ward

# 7，密度估计法：density


##最短距离法
hc <- hclust(dist(X), 'single')  ##最短距离法
cbind(hc$merge, hc$height)  ##分类过程
plot(hc)  ##聚类图


##Ward法(采用欧氏距离)
hc <- hclust(dist(X), 'ward') ##ward法
cbind(hc$merge, hc$height)  ##分类过程
plot(hc)  ##聚类图


##自编系统聚类函数H.clust()

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

#C=H.clust(X)


setwd('E:\多元统计分析数据\多元统计分析及R语言建模（第4版）\聚类分析')
d7.2 = read.table('7.2.txt', header = T)
d7.2 <- as.matrix(d7.2)
plot(d7.2)

#install.packages('mvstats')
#library(mvstats)
H.clust(d7.2, 'euclidean', 'single', plot = T)  ##最短距离法
H.clust(d7.2, 'euclidean', 'complete', plot = T) ##最长距离法
H.clust(d7.2, 'euclidean', 'median', plot = T) ##中间距离法
H.clust(d7.2, 'euclidean', 'average', plot = T) ##类平均法
H.clust(d7.2, 'euclidean', 'centrod', plot = T) ##重心法
H.clust(d7.2, 'euclidean', 'ward', plot = T) ##ward法


###kmeans聚类法
x1 = matrix(rnorm(1000, mean = 0, sd = 0.3), ncol = 10)
x2 = matrix(rnorm(1000, mean = 1, sd = 0.3), ncol = 10)
x = rbind(x1, x2)
H.clust(x, 'euclidean', 'complete')

cl = kmeans(x, 2)


pch1 = rep('1', 100)
pch2 = rep('2', 100)
plot(x, col = cl$cluster, pch = c(pch1, pch2), cex = 0.7)
points(cl$centers, col = 3, pch = '*', cex = 3)


###kmeans处理大样本

x1 = matrix(rnorm(10000, mean = 0, sd = 0.3), ncol = 10)
x2 = matrix(rnorm(10000, mean = 1, sd = 0.3), ncol = 10)
x = rbind(x1, x2)
H.clust(x, 'euclidean', 'complete')

cl = kmeans(x, 2)


pch1 = rep('1', 100)
pch2 = rep('2', 100)
plot(x, col = cl$cluster, pch = c(pch1, pch2), cex = 0.7)
points(cl$centers, col = 3，pch='*', cex=3)


## case study
Case6=read.table('7_case.txt', header=T)
Z=scale(Case6)
hc=hclust(dist(Z))
plot(hc); rect.hclust(hc, 2); cutree(hc, 2)
plot(hc); rect.hclust(hc, 3); cutree(hc, 3)
plot(hc); rect.hclust(hc, 4); cutree(hc, 4)
plot(hc); rect.hclust(hc, 5); cutree(hc, 5)
plot(hc, hang=-1)   #hang是表明谱系图中各类所在的位置 当hang取负值时，谱系图中的类从底部画起  生成谱系图
opar<-par(mfrow=c(2, 1), mar=c(5.2, 4, 1, 0))  #生成两行一列 ;图像距离边界的距离
plot(hc); rect.hclust(hc, 4); cutree(hc, 4)
plot(hc); rect.hclust(hc, 5); cutree(hc, 5)
par(opar)
##kmeans聚类法
kmeans(Z, 2)$cluster
kmeans(Z, 3)$cluster
kmeans(Z, 4)$cluster
kmeans(Z, 5)$cluster

###hcluster的结果
1. merge
其行编号表示聚类过程的步骤, X1,X2表示在该步合并的两类, 该编号为负代表原始的样本序号, 编号为正代表新合成的类;
2. 变量height表示合并时两类类间距离.







