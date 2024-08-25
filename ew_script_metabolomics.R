rm(list=ls())

library(caret)
library(glmnet)
library(pROC)
library(randomForest)

choix = 'males'

volcano <- function(x,y,ref=levels(y)[1],adjust='none'){
  p = ncol(x)
  ps = fc = numeric(p)
  for(j in 1:p){
    xj = x[,j]
    if(!is.numeric(xj)){xj = as.numeric(xj)}
    fc[j] = mean(xj[y==ref])/mean(xj[y!=ref])
    if(any(xj==0)){xj = xj+1}
    o = t.test(log(xj)~y)
    ps[j] = o$p.value
  }
  if(adjust!='none'){
    ps = p.adjust(ps,method=adjust)
  }
  #xv = log2(fc)
  #yv = -log10(ps)
  return(list(fold.changes=fc,p.values=ps))
}

setwd('O:\\HE_PH-VDI-BSIG-201825-DAOB\\Handover_DNBC_clinical\\Metabolomics\\Merged clinical and metabolite data')
dc = read.csv('full_set_with_metabolites_clean.csv', 
                stringsAsFactors=TRUE, sep=',')
dc = na.omit(dc)
ej = which((dc$ASD=='Control')&(dc$Case_Control.x=='CASE'))
dc = dc[-ej,]

dat_merged = dc
table(dc$Case_Control.x,dc$ASD)
dc$X = NULL
dc$new_lbgravnr = NULL
dc$Sample.Identification = NULL
X.merged = dc
X.merged$ASD = NULL
X.merged$Case_Control.x = NULL
y.merged = as.factor(dc$Case_Control.x)
dc$Case_Control.x = NULL

i.female = which(X.merged$sex=='K')
i.male = which(X.merged$sex=='M')
X.male = X.merged[i.male,]
X.female = X.merged[i.female,]
dc.male = dc[i.male,]
dc.female = dc[i.female,]
y.male = y.merged[i.male]
y.female = y.merged[i.female]

if(choix=='all'){
  x = X.merged
  y = y.merged
} else {
  if(choix=='males'){
    x = X.male
    y = y.male
  } else {
    x = X.female
    y = y.female
  }
}
fpath = 'O:/HE_PH-VDI-BSIG-201825-DAOB/Handover_DNBC_clinical/ew_July24/out/'
fpath = paste(fpath,choix,'_',sep='')

fff <- function(v){
  return(length(unique(v)))
}
i1 = which(as.numeric(apply(x,2,fff))<2)
if(length(i1)){x = x[,-i1]}
P = ncol(x)
nms = names(x)

xm = model.matrix(y~.+0, data=x)
y = relevel(y,ref='CONTROL')

### Univariate Tests
pvals = numeric(P)
for(j in 1:P){
  if(!is.factor(x[,j])){
    pv = wilcox.test(x[,j]~y)$p.value
    pvals[j] = pv
  }
}
pas = p.adjust(pvals, method='fdr')
nms[which(pas<0.05)]
p.df = data.frame(p.value=pvals, p.fdr=pas)
row.names(p.df) = nms
write.csv(p.df,file=paste(fpath,'wilcoxon_output.csv',sep=''))

vol.out = volcano(x, y, adjust = 'none')
vc.df = as.data.frame(vol.out)
vc.df$p.fdr = p.adjust(vc.df$p.values, method='fdr')
row.names(vc.df) = nms
write.csv(vc.df,file=paste(fpath,'volcano_output.csv',sep=''))

# CV parameters
n = nrow(x)
K = 10
if(choix=='all'){
  folds = createFolds(x$sex,k=K,list=FALSE)
} else {
  folds = createFolds(x$B202_1.depression,k=K,list=FALSE)
  # [1] "Child.Birth.no.multiple.birth"        
  # [2] "Preeclamsia"                          
  # [3] "A060.father.have.asthma"              
  # [4] "Fish_Oil"                             
  # [5] "B092.smoking.hashish.during.pregnancy"
  # [6] "Gestational_Diabetes"                 
  # [7] "B202_1.depression"                    
  # [8] "B202_4.anxiety"                       
  # [9] "PTB"                                  
  # [10] "Mental_Disorder_ICD"  
}
names(x)
#### Volcano analysis with Lasso

aucs = accs = sens = spec = numeric(K)
sel.mat = matrix(0,nrow=K,ncol=ncol(xm))
colnames(sel.mat) = colnames(xm)
set.seed(6041)
for(k in 1:K){
  itr = which(folds!=k)
  vol.out = volcano(x[itr,], y[itr])
  isel = which(vol.out$p.values < 0.1)
  lam = cv.glmnet(xm[itr,isel], y[itr], family='binomial')
  lasso = glmnet(xm[itr,isel], y[itr], family='binomial', lambda=lam$lambda.min)
  lasso.pred = predict(lasso, newx=xm[-itr,isel], type='response')
  preds = as.factor(as.numeric(lasso.pred>.5))
  preds = relevel(preds,ref='0')
  sel.mat[k,isel] = 1
  tb = table(preds, y[-itr])
  aucs[k] = roc(y[-itr],lasso.pred[,1])$auc
  accs[k] = sum(diag(tb))/sum(tb)
  sens[k] = tb[2,2]/sum(tb[,2])
  spec[k] = tb[1,1]/sum(tb[,1])
}
round(c(mean(accs), mean(aucs), mean(sens), mean(spec)), 4)
res = data.frame(accs,aucs,sens,spec)
write.csv(res,file=paste(fpath,'lasso_output.csv',sep=''))
write.csv(sel.mat,file=paste(fpath,'lasso_selection_matrix.csv',sep=''))

#### Volcano analysis with glm

aucs = accs = sens = spec = numeric(K)
sel.mat = matrix(0,nrow=K,ncol=ncol(x))
colnames(sel.mat) = colnames(x)
set.seed(6041)
for(k in 1:K){
  itr = which(folds!=k)
  vol.out = volcano(x[itr,], y[itr])
  isel = order(vol.out$p.values)[1:5]
  sel.mat[k,isel] = 1
  df.glm = data.frame(y,x[,isel])
  gmo = glm(y~., data=df.glm, subset = itr, family='binomial')
  glm.pred = predict(gmo, newdata= df.glm[-itr,], type='response')
  preds = as.factor(as.numeric(glm.pred>.5))
  preds = relevel(preds,ref='0')
  tb = table(preds, y[-itr])
  aucs[k] = roc(y[-itr],glm.pred)$auc
  accs[k] = sum(diag(tb))/sum(tb)
  sens[k] = tb[2,2]/sum(tb[,2])
  spec[k] = tb[1,1]/sum(tb[,1])
}
round(c(mean(accs), mean(aucs), mean(sens), mean(spec)), 4)
res = data.frame(accs,aucs,sens,spec)
write.csv(res,file=paste(fpath,'glm_output.csv',sep=''))
write.csv(sel.mat,file=paste(fpath,'glm_selection_matrix.csv',sep=''))

### Using Random Forests
aucs = accs = sens = spec = numeric(K)
sel.mat = matrix(0,nrow=K,ncol=ncol(x))
colnames(sel.mat) = colnames(x)
set.seed(6041)
for(k in 1:K){
  itr = which(folds!=k)
  vol.out = volcano(x[itr,], y[itr])
  isel = order(vol.out$p.values)[1:5]
  sel.mat[k,isel] = 1
  df.rf = data.frame(y,x[,isel])
  rf.mo = randomForest(y~., data=df.rf[itr,])
  rf.pred.v = predict(rf.mo, newdata= df.rf[-itr,],type='prob')[,1]
  preds = predict(rf.mo, newdata= df.rf[-itr,], type='class')
  tb = table(preds, y[-itr])
  aucs[k] = roc(response=y[-itr],predictor=rf.pred.v)$auc
  accs[k] = sum(diag(tb))/sum(tb)
  sens[k] = tb[2,2]/sum(tb[,2])
  spec[k] = tb[1,1]/sum(tb[,1])
}
round(c(mean(accs), mean(aucs), mean(sens), mean(spec)), 4)
res = data.frame(accs,aucs,sens,spec)
write.csv(res,file=paste(fpath,'rf_output.csv',sep=''))
write.csv(sel.mat,file=paste(fpath,'rf_selection_matrix.csv',sep=''))

