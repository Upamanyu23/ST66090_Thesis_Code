volcano <- function(x, y, ref=levels(y)[1],
                    eff.low=-.6,eff.high=+.6,
                    sig=0.05,adjust="none",...){
  p = ncol(x)
  ps = fc = numeric(p)
  for(j in 1:p){
    xj = x[,j]
    if(!is.numeric(xj)){ xj = as.numeric(xj) }
    #fc[j] = mean(xj[y==ref])/mean(xj[y!=ref])
    fc[j] = mean(xj[y!=ref])/mean(xj[y==ref])
    o = t.test(log(xj)~y)
    ps[j] = o$p.value
  }
  if(adjust!="none"){
    ps = p.adjust(ps,method=adjust)
  }
  xv = log2(fc)
  yv = -log10(ps)
  cls = 8+0*xv
  #cls[which((ps<0.05)&(xv>=eff.high))] = 'tomato'
  #cls[which((ps<0.05)&(xv<=eff.low))] = 'blue'
  cls[which((ps<0.05))] = 'tomato'
  cls[which(ps>0.05 & ps<0.1)] = 'orange'
  cls[which((ps>0.1))] = 'blue'
  plot(xv,yv,pch=20,col=cls, cex = 2,...) ##xlim = c(-0.5,0.5)
  abline(v=0,h=-log10(sig),lty=3,col=8)
  return(list(fold.changes=fc, p.values=ps))
}

?p.adjust
j = volcano(X, y)

vol_merged = volcano(X.merged, y.merged)
names(X.merged)[which(vol_merged$p.values < 0.05)]

merged_vol_out_indices = which(vol_merged$p.values < 0.05)

levels(y)[1]
levels(y.male)[1]

names(X)[which(j$p.values < 0.05)] ##"Ile"      "MetSO"    "LPC_20.3"
names(X)[which(j$p.values > 0.05 & j$p.values < 0.1)] ##"Leu"      "Met"      "LPC_18.1" "LPC_18.2" "PC_O44.3"


###detailed analysis
set.seed(6041)
K = 10
folds = cut(1:n, breaks=K, labels=FALSE)
sel.vol = matrix(0, nrow=K, ncol=ncol(X))
colnames(sel.vol) = names(X)
for(k in 1:K){
  itr = which(folds!=k)
  vol.out = volcano(X[itr,], y[itr])
  isel = which(vol.out$p.values < 0.1)
  sel.vol[k,isel] = 1
}
var.sel.vol = apply(sel.vol,2,mean)*100

sel.vol[1,]

which(var.sel.vol>=50)
#MetSO, Ile, Leu, LPC_18.2, LPC_20.3, LPC_18.1, PC_O34.3, PC_O44.3

mod.volc = glm(Case_Control~MetSO+Ile+Leu+LPC_18.2+LPC_20.3+LPC_18.1+PC_O34.3+PC_O44.3, data=dat, family = binomial)
pred.vol = predict(mod.volc, type='response')
preds.vol = as.factor(as.numeric(pred.vol>.5))


tb.vol = table(preds.vol, dat$Case_Control)
acc.vol = sum(diag(tb.vol))/sum(tb.vol) #0.5789474

##include the glm fit inside the loop, check the validation fold performance
##if the result is good, we can use bootstrapping as well!
##do bootstrapping anyway

##check the change in ratios in the volcano plot

##why is METSO not having any predictive power??
