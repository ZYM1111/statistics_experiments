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
