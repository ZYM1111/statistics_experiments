### Name: plot.text

### Aliases: plot.text
### Keywords: ~kwd1 ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--    or do  help(data=index)  for the standard data sets.

## The function is currently defined as
plot.text<-function(X,h=0,v=0)
{
   plot(X);abline(h=h,v=v,lty=3)
   text(X,label=rownames(X),pos=1.1,adj=0.5,cex=0.85) 
}
