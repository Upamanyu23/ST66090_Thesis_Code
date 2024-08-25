dat_merged = read.csv("C:/Upamanyu/Ireland/Data Science/Classes/Thesis project/dnbc_merged.csv")

dat_merged = read.csv("C:/Upamanyu/Ireland/Data Science/Classes/Thesis project/dnbc_merged_modified.csv")
dim(d)
dim(dat_merged)
dat_merged = dat_merged[-c(5,78,92,124,247,284,294,396),]

str(dat_merged)
dat_merged[c(1,2,3,4,5), c(1:10)]

dat_merged[, c('HMDB0303494',	'HMDB0240748')]

dat_merged = dat_merged[sample(1:nrow(dat_merged)),]

normalized_data <- data.frame(lapply(X.merged, normalize_by_median))

normalized_data = log10(normalized_data)

X.merged = data.frame(scale(normalized_data, center = TRUE, scale = TRUE))

dat_merged$Case_Control = ifelse(dat_merged$Case_Control == 'CASE', 1, 0)
dat_merged$Sex = ifelse(dat_merged$Sex == 'Male', 1, 2)
dat_merged$Case_Control = factor(dat_merged$Case_Control)
dat_merged$Sex = factor(dat_merged$Sex)

boxplot(X.merged[,c(1:5)],col = "green", cex.axis = 1,las=1)

48584.71385/3837.552519

49457.44206/3837.736639

X.merged = dat_merged
X.merged$Case_Control = NULL
X.merged$Sex= NULL
y.merged = dat_merged$Case_Control

dim(X.merged)
###Correlation filter
library(caret)
M = cor(X.merged)
ic = findCorrelation(M,cutoff=.90)
nms = names(X.merged)
frm = nms[ic]

X.merged.rem.cor = X.merged[,-ic]
dim(X.merged.rem.cor)

###Near zero var filter
nzv_indices = nearZeroVar(X.merged, saveMetrics = TRUE)



######Univariate analysis
covariates <- names(X.merged)[sapply(X.merged, is.numeric)]##[-c(1,2)]
dat[,c]
length(covariates)
p.matrix = matrix(NA,nrow=708,ncol=2)
r=1
for(c in covariates){
  p = wilcox.test(dat_merged[,c]~dat_merged[,1])$p.value
  p.matrix[r,1] = c
  p.matrix[r,2] = p
  r = r + 1
}

ind = which(p.matrix[,2] < 0.05)
X.merged.red = X.merged[,ind]
dim(X.merged.red)

head(p.matrix)
p.matrix[13,]
names(X.merged)

head(X.merged[,c(1,5)])
library(gbm)
?predict.gbm


####Volcano analysis with glm
library(glmnet)
library(pROC)
#xm = as.matrix(X.merged)
K = 10
n = nrow(X.merged)
folds = createFolds(dat_merged$Sex, k = 10, list = FALSE)
glm.acc.tar = numeric(K)
sens.targeted = numeric(K)
spec.targeted = numeric(K)
top.var.glm.tar = list()
auc.m = numeric(K)
covariates = names(X.merged)[sapply(X.merged, is.numeric)]
#acc.gbm = numeric(K)
set.seed(6041)
for(k in 1:K){
  itr = which(folds!=k)
  
  p.matrix = matrix(NA,nrow=851,ncol=2)
  r=1
  for(c in covariates){
    p = t.test(X.merged[itr,c]~y.merged[itr])$p.value
    p.matrix[r,1] = c
    p.matrix[r,2] = p
    r = r + 1
  }
  isel = which(p.matrix[,2] < 0.05)
  
  
  #vol.out = volcano(X.targeted_whole[itr,], y.targeted_whole[itr])
  #isel = which(vol.out$p.values < 0.05)
  #isel = order(vol.out$p.values)[1:5]
  #top.var.glm.tar[k] = list(isel)
  df.glm = data.frame(y.merged,X.merged[,isel])
  gmo = glm(y.merged~., data=df.glm, subset = itr, family='binomial')
  glm.pred = predict(gmo, newdata= df.glm[-itr,], type='response')
  preds.glm = as.factor(as.numeric(glm.pred>.5))
  tb.glm = table(preds.glm, y.merged[-itr])
  acc.glm = sum(diag(tb.glm))/sum(tb.glm)
  glm.acc.tar[k] = acc.glm
  sens.targeted[k] = tb.glm[2,2]/sum(tb.glm[,2])
  spec.targeted[k] = tb.glm[1,1]/sum(tb.glm[,1])
  auc.m[k] = roc(y.merged[-itr],glm.pred)$auc
  plot(roc(y.merged[-itr],glm.pred))
}
mean(glm.acc.tar)
mean(sens.targeted)
mean(spec.targeted)
mean(auc.m)

###Using Random Forests
library(randomForest)
K = 10
n = nrow(X.merged)
folds = createFolds(dat_merged$Sex, k = 10, list = FALSE)
rf.acc.tar = numeric(K)
top.var.list.rf = list()
sens.rf.tar = numeric(K)
spec.rf.tar =numeric(K)
covariates = names(X.merged)[sapply(X.merged, is.numeric)]
auc.m = numeric(K)
#acc.gbm = numeric(K)
set.seed(6041)
for(k in 1:K){
  itr = which(folds!=k)
  
  p.matrix = matrix(NA,nrow=851,ncol=2)
  r=1
  for(c in covariates){
    p = t.test(X.merged[itr,c]~y.merged[itr])$p.value
    p.matrix[r,1] = c
    p.matrix[r,2] = p
    r = r + 1
  }
  
  #isel = which(vol.out$p.values < 0.1)
  isel = which(p.matrix[,2] < 0.05)
  #top.var.list.rf[k] = list(isel)
  df.rf = data.frame(y.merged,X.merged[,isel])
  rf.mo = randomForest(y.merged~., data=df.rf[itr,])
  rf.pred = predict(rf.mo, newdata= df.rf[-itr,], type='class')
  rf.prob = predict(rf.mo, newdata= df.rf[-itr,], type='prob')
  #preds.glm = as.factor(as.numeric(glm.pred>.5))
  tb.rf = table(rf.pred, y.merged[-itr])
  acc.rf = sum(diag(tb.rf))/sum(tb.rf)
  rf.acc.tar[k] = acc.rf
  sens.rf.tar[k] = tb.rf[2,2]/sum(tb.rf[,2])
  spec.rf.tar[k] = tb.rf[1,1]/sum(tb.rf[,1])
  auc.m[k] = roc(y.merged[-itr],rf.prob[,1])$auc
}

mean(rf.acc.tar)
mean(sens.targeted)
mean(spec.targeted)
mean(auc.m)

sd(auc.m)/(sqrt(10))
0.01475504 * 1.96
0.5640745 + 0.02891988
0.5640745 - 0.02891988

###CV WilcoxTest
K = 10
n = nrow(X.merged)
folds = cut(1:n, breaks=K, labels=FALSE)
sel.wilcox = matrix(0, nrow=K, ncol=ncol(X.merged))
colnames(sel.wilcox) = names(X.merged)
covariates = names(X.merged)[sapply(X.merged, is.numeric)]
set.seed(6041)
for(k in 1:K){
  itr = which(folds!=k)
  p.matrix = matrix(NA,nrow=708,ncol=2)
  r=1
  for(c in covariates){
    p = wilcox.test(X.merged[itr,c]~y.merged[itr])$p.value
    p.matrix[r,1] = c
    p.matrix[r,2] = p
    r = r + 1
  }
  ind = which(p.matrix[,2] < 0.05)
  #isel = which(vol.out$p.values < 0.1)
  sel.wilcox[k,ind] = 1
}
var.sel.wilcox = apply(sel.wilcox,2,mean)*100
length(which(var.sel.wilcox >= 80))

X.merged.red = X.merged[,which(var.sel.wilcox >= 80)]

dim(X.merged.red)

####Volcano analysis with Lasso
library(glmnet)
library(pROC)
xm = as.matrix(X.merged.red)
K = 10
n = nrow(X.merged.red)
folds = cut(1:n, breaks=K, labels=FALSE)
lasso.acc.m = numeric(K)
auc.m = numeric(K)
acc.gbm = numeric(K)
set.seed(6041)
for(k in 1:K){
  itr = which(folds!=k)
  vol.out = volcano(X.merged.red[itr,], y.merged[itr])
  isel = which(vol.out$p.values < 0.1)
  lam = cv.glmnet(xm[itr,isel], y.merged[itr], family='binomial')
  lasso = glmnet(xm[itr,isel], y.merged[itr], family='binomial', lambda=lam$lambda.min)
  lasso.pred = predict(lasso, newx=xm[-itr,isel], type='response')
  preds.lasso = as.factor(as.numeric(lasso.pred>.5))
  tb.lasso = table(preds.lasso, y.merged[-itr])
  acc.lasso = sum(diag(tb.lasso))/sum(tb.lasso)
  lasso.acc.m[k] = acc.lasso
  auc.m[k] = roc(y.merged[-itr],lasso.pred)$auc
}
mean(lasso.acc.m)
mean(auc.m)

auc.glm[k] = roc(y.test, glm.p)$auc


##RFE
subsets <- c(1:10,20,30,40,50,60,70,80,90)
ctrl <- rfeControl(functions = rfFuncs,
                   method = "cv",
                   number = 10,
                   # method = "repeatedcv",
                   # repeats = 5,
                   verbose = FALSE)
rf.rfe <- rfe(X.merged.red, y.merged,
              sizes = subsets,
              rfeControl = ctrl)



set.seed(6041)
K = 10
n=nrow(X.merged)
folds = cut(1:n, breaks=K, labels=FALSE)
sel.vol.merged = matrix(0, nrow=K, ncol=ncol(X.merged))
colnames(sel.vol.merged) = names(X.merged)
p = ncol(X.merged)
for(k in 1:K){
  itr = which(folds!=k)
  for(j in 1:p){
    xj = X.merged[itr, j]
    if(!is.numeric(xj)){ xj = as.numeric(xj) }
    #fc[j] = mean(xj[y==ref])/mean(xj[y!=ref])
    fc = abs(log2(mean(xj[y.merged[itr]!="CONTROL"])/mean(xj[y.merged[itr]=="CONTROL"])))
    if(fc < 0.1){
      sel.vol.merged[k,j] = 1
    }
  }
}

var.sel.vol.m = apply(sel.vol.merged,2,mean)*100
which(var.sel.vol.m==100)


X.merged.vol = X.merged[,merged_vol_out_indices]
length(merged_vol_out_indices)

write.csv(X.merged.vol, "C:/Upamanyu/Ireland/Data Science/Classes/Thesis project/reduced_dnbc_merged.csv", row.names = FALSE)

pca.scaled = prcomp(X.merged.vol,scale=TRUE)
summary(pca.scaled)


##PLS

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("mixOmics")

library(mixOmics)
install.packages("gtable")

install.packages('ggplot2')
pca.autism = pca(X.merged.vol, ncomp = 3, scale = TRUE)

?plotIndiv
plotIndiv(pca.autism, group = y.merged, ind.names = FALSE,
          legend = TRUE, 
          title = 'SRBCT, PCA comp 1 - 2')

dim(X.merged.vol)

browseVignettes("mixOmics")

plsda.srbct <- plsda(X.merged.std,y.merged, ncomp = 10)

set.seed(30) # For reproducibility with this handbook, remove otherwise
perf.plsda.srbct <- perf(plsda.srbct, validation = 'Mfold', folds = 10, 
                         progressBar = FALSE,  # Set to TRUE to track progress
                         nrepeat = 10)         # We suggest nrepeat = 50

plot(perf.plsda.srbct, sd = TRUE, legend.position = 'horizontal')


final.plsda.srbct <- plsda(X.merged.std,y.merged, ncomp = 2)

plotIndiv(final.plsda.srbct, ind.names = FALSE, legend=TRUE,
          comp=c(1,2), ellipse = TRUE, 
          title = 'PLS-DA on comp 1-2',
          X.label = 'PLS-DA comp 1', Y.label = 'PLS-DA comp 2')

plotVar(final.plsda.srbct, comp = c(1, 2), legend = TRUE, title = "PLS-DA Variable Importance")

perf.plsda.srbct$error.rate$BER[, 'max.dist']

final.plsda.srbct

final.plsda.srbct$ind.mat
##sPLS-DA

list.keepX <- c(1:20,  seq(30, 170, 10))
list.keepX

tune.splsda.srbct <- tune.splsda(X.merged.vol.std, y.merged, ncomp = 5, validation = 'Mfold', 
                                 folds = 5, dist = 'max.dist', 
                                 test.keepX = list.keepX, nrepeat = 50)

head(tune.splsda.srbct$error.rate)
plot(tune.splsda.srbct, sd = TRUE)

tune.splsda.srbct$choice.ncomp$ncomp
tune.splsda.srbct$choice.keepX


ncomp <- tune.splsda.srbct$choice.ncomp$ncomp 
ncomp

select.keepX <- tune.splsda.srbct$choice.keepX[1:ncomp]  
select.keepX

splsda.srbct <- splsda(X.merged.vol.std, y.merged, ncomp = ncomp, keepX = select.keepX) 

set.seed(34)  # For reproducibility with this handbook, remove otherwise

perf.splsda.srbct <- perf(splsda.srbct, folds = 5, validation = "Mfold", 
                          dist = "max.dist", progressBar = FALSE, nrepeat = 10)

# perf.splsda.srbct  # Lists the different outputs
perf.splsda.srbct$error.rate

perf.splsda.srbct$error.rate.class


par(mfrow=c(1,2))
# For component 1
stable.comp1 <- perf.splsda.srbct$features$stable$comp1
barplot(stable.comp1, xlab = 'variables selected across CV folds', 
        ylab = 'Stability frequency',
        main = 'Feature stability for comp = 1')

# For component 2
stable.comp2 <- perf.splsda.srbct$features$stable$comp2
barplot(stable.comp2, xlab = 'variables selected across CV folds', 
        ylab = 'Stability frequency',
        main = 'Feature stability for comp = 2')
par(mfrow=c(1,1))



###Checking correlation
M = cor(X.merged.vol.std)
ic = findCorrelation(M,cutoff=.90)
nms = names(X.merged.vol.std)
frm = nms[ic]

XS = X.merged.vol.std[,-ic]
dim(XS)


##Univariate GLM
pr = ncol(XS)
p.v = numeric(pr)
for(j in 1:pr){
  xj = XS[,j]
  mod1 = glm(y.merged~xj, family=binomial)
  p.v[j] = summary(mod1)$coefficients[2,4]
}

length(which(p.v < 0.05))

XS.upd = XS[,which(p.v < 0.05)]
dim(XS.upd)

##RFE
subsets <- c(1:5, 10, 15, 20, seq(30, 80, 10))
ctrl <- rfeControl(functions = rfFuncs,
                   #method = "cv",
                   number = 10,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)
rf.rfe <- rfe(XS.upd, y.merged,
              sizes = subsets,
              rfeControl = ctrl)


###Forward Selection
library(leaps)
dat = data.frame(y.merged,XS.upd)
head(dat)
str(dat$y.merged)
reg.fwd = regsubsets(y.merged~., data=dat, nvmax = 50, method = 'forward')

mod = summary(reg.fwd)$which[which.max(summary(reg.fwd)$adjr2),]

names(which(mod))

summary(reg.fwd)$adjr2

names(XS.upd)
dat[,(which(mod)-1)]

mod.fwd = glm(y.merged~., data=dat[], family = 'binomial')
pred =predict(mod.fwd, type="response")
preds = as.factor(as.numeric(pred>.5))

tb.fwd = table(preds, dat$Case_Control)
acc.fwd = sum(diag(tb.fwd))/sum(tb.fwd) #0.5745614



###Stepwise CV

df_merged = data.frame(y.merged, X.merged.red)
dim(df_merged)
names(df_merged)

n = nrow(df_merged)
#df_neg_r = df_neg[sample(1:nrow(df_neg)),]
K = 10
folds = cut(1:n, breaks=K, labels=FALSE)
accuracy.stepwise = numeric(K)
auc.step = numeric(K)
sens = numeric(K)
spec = numeric(K)
acc.gbm = numeric(K)
covariates = names(X.merged)[sapply(X.merged, is.numeric)]
set.seed(4061) # for reproducibility
for(k in 1:K){
  itr = which(folds!=k)
  p.matrix = matrix(NA,nrow=708,ncol=2)
  r=1
  for(c in covariates){
    p = wilcox.test(X.merged[itr,c]~y.merged[itr])$p.value
    p.matrix[r,1] = c
    p.matrix[r,2] = p
    r = r + 1
  }
  ind = which(p.matrix[,2] < 0.05)
  X.merged.red = X.merged[,ind]
  vol.out = volcano(X.merged.red[itr,], y.merged[itr])
  isel = which(vol.out$p.values < 0.05)
  df_merged_step = data.frame(y.merged, X.merged.red[,isel])
  
  lmo = glm(y.merged~., data=df_merged_step,subset = itr, family = 'binomial')
  reg.fwd = step(lmo, direction="forward")
  pred = predict(reg.fwd, newdata=df_merged_step[-itr,], type = 'response')
  preds.vol = as.factor(as.numeric(pred>.5))
  tb.vol = table(preds.vol, df_merged_step[-itr,]$y.merged)
  acc.vol = sum(diag(tb.vol))/sum(tb.vol)
  sens[k] = tb.vol[2,2]/sum(tb.vol[,2])
  spec[k] = tb.vol[1,1]/sum(tb.vol[,1])
  accuracy.stepwise[k] = acc.vol
  auc.step[k] = roc(df_merged_step[-itr,]$y.merged, c(pred))$auc

}
mean(spec)
mean(sens)

mean(auc.step)
mean(accuracy.stepwise)


###GBM

df_merged_gb = data.frame(y.merged, X.merged.red)
df_merged_gb$y.merged = (as.numeric(df_merged_gb$y.merged=="1"))
dim(df_merged_gb)

n = nrow(df_merged_gb)
#df_neg_r = df_neg[sample(1:nrow(df_neg)),]
K = 10
folds = cut(1:n, breaks=K, labels=FALSE)
acc.gbm = numeric(K)
sens.gb = numeric(K)
spec.gb = numeric(K)
auc.gb = numeric(K)
covariates = names(X.merged)[sapply(X.merged, is.numeric)]
sel.wilcox = matrix(0, nrow=K, ncol=ncol(X.merged))
set.seed(4061) # for reproducibility
for(k in 1:K){
  itr = which(folds!=k)
  #vol.out = volcano(X.merged.red[itr,], y.merged[itr])
  #isel = which(vol.out$p.values < 0.1)
  #df_merged_gb = data.frame(y.merged, X.merged.red[,isel])
  #df_merged_gb$y.merged = (as.numeric(df_merged_gb$y.merged=="1"))
  
  p.matrix = matrix(NA,nrow=708,ncol=2)
  r=1
  for(c in covariates){
    p = wilcox.test(X.merged[itr,c]~y.merged[itr])$p.value
    p.matrix[r,1] = c
    p.matrix[r,2] = p
    r = r + 1
  }
  ind = which(p.matrix[,2] < 0.05)
  X.merged.red = X.merged[,ind]
  vol.out = volcano(X.merged.red[itr,], y.merged[itr])
  isel = which(vol.out$p.values < 0.03)
  df_merged_gb = data.frame(y.merged, X.merged.red[,isel])
  df_merged_gb$y.merged = (as.numeric(df_merged_gb$y.merged=="1"))
  
  gb.out= gbm(y.merged~., data= df_merged_gb[itr,], distribution = 'bernoulli',
              n.trees=15000, interaction.depth=1)
  gb.p = predict(gb.out, newdata = df_merged_gb[-itr,], n.trees=15000, type = 'response')
  cl.pred = as.factor(as.numeric(gb.p>.5))
  tb.gb = table(cl.pred,df_merged_gb[-itr,]$y.merged)
  acc.gb = sum(diag(tb.gb))/sum(tb.gb)
  acc.gbm[k] = acc.gb
  sens.gb[k] = tb.gb[2,2]/sum(tb.gb[,2])
  spec.gb[k] = tb.gb[1,1]/sum(tb.gb[,1])
  auc.gb[k] = roc(df_merged_gb[-itr,]$y.merged, c(gb.p))$auc
}
mean(acc.gbm)
mean(sens.gb)
mean(spec.gb)
mean(auc.gb)

dim(df_merged)

