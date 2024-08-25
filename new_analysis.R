####Analysis on merged data###
library(DataExplorer)
plot_histogram(X.merged.std[,c(1:20)])
warnings()

###Without removing correlated features
dim(dat_merged.log)

dat_merged.log = dat_merged

dat_merged.log[,-c(1,2)] = log2(dat_merged.log[,-c(1,2)])

dat_merged.log[,-c(1,2)] = 

dat_merged.log = log2(dat_merged[,-c(1,2)])

boxplot(dat_merged.log$HMDB0006528 ~ dat_merged.log$Case_Control)
boxplot(dat_male$HMDB0034441 ~ dat_male$Case_Control)

mean(dat_male$HMDB0034441[dat_male$Case_Control=='1'])/mean(dat_male$HMDB0034441[dat_male$Case_Control=='0'])
mean(dat_male$HMDB0034441[dat_male$Case_Control=='0'])

library(randomForest)
K = 10
n = nrow(dat_merged.log)
folds = createFolds(dat_merged.log$Sex, k = 10, list = FALSE)
rf.acc.tar = numeric(K)
#top.var.list.rf = list()
sens.rf.tar = numeric(K)
spec.rf.tar =numeric(K)
#auc.m = numeric(K)
#acc.gbm = numeric(K)
set.seed(6041)
for(k in 1:K){
  itr = which(folds!=k)
  #vol.out = volcano(X.targeted[itr,], y.targeted[itr])
  #isel = which(vol.out$p.values < 0.1)
  #isel = order(vol.out$p.values)[1:10]
  #top.var.list.rf[k] = list(isel)
  #df.rf = data.frame(y.targeted,X.targeted[,isel])
  rf.mo = randomForest(Case_Control~., data=dat_merged.log[itr,], ntree =700)
  rf.pred = predict(rf.mo, newdata= dat_merged.log[-itr,], type='class')
  #preds.glm = as.factor(as.numeric(glm.pred>.5))
  tb.rf = table(rf.pred, dat_merged.log$Case_Control[-itr])
  acc.rf = sum(diag(tb.rf))/sum(tb.rf)
  rf.acc.tar[k] = acc.rf
  sens.rf.tar[k] = tb.rf[2,2]/sum(tb.rf[,2])
  spec.rf.tar[k] = tb.rf[1,1]/sum(tb.rf[,1])
  #auc.m[k] = roc(y.merged[-itr],lasso.pred)$auc
}
mean(spec.rf.tar)
?randomForest

###CV student t test
K = 10
n = nrow(X.merged)
folds = createFolds(dat_merged$Sex, k = 10, list = FALSE)
sel.t = matrix(0, nrow=K, ncol=ncol(X.merged))
colnames(sel.t) = names(X.merged)
covariates = names(X.merged)[sapply(X.merged, is.numeric)]
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
  ind = which(p.matrix[,2] < 0.05)
  #isel = which(vol.out$p.values < 0.1)
  sel.t[k,ind] = 1
}
var.sel.t = apply(sel.t,2,mean)*100
length(which(var.sel.t >= 80))

p.matrix[ind,]
##Lasso Feature Selection

###Lasso
library(glmnet)
set.seed(4061) # for reproducibility
n = nrow(dat_merged.log)
xm = as.matrix(dat_merged.log[,-c(1,2)])
K = 10
folds = createFolds(dat_merged.log$Sex, k = 10, list = FALSE)
sel = matrix(0, nrow=K, ncol=851)
colnames(sel) = names(dat_merged.log[,-c(1,2)])
rf.acc.tar = numeric(K)
sens.rf.tar = numeric(K)
spec.rf.tar = numeric(K)
for(k in 1:K){
  itr = which(folds!=k)
  lasso.cv = cv.glmnet(xm[itr,], dat_merged.log$Case_Control[itr], family='binomial', alpha=0.5)
  lasso = glmnet(xm[itr,], dat_merged.log$Case_Control[itr], lambda=lasso.cv$lambda.min,  family='binomial', alpha=0.5)
  isel = which(coef(lasso)[-1] != 0)
  sel[k,isel] = 1
  rf.mo = randomForest(Case_Control~., data=dat_merged.log[itr,c(1,(isel+2))])
  rf.pred = predict(rf.mo, newdata= dat_merged.log[-itr,c(1,(isel+2))], type='class')
  tb.rf = table(rf.pred, dat_merged.log$Case_Control[-itr])
  acc.rf = sum(diag(tb.rf))/sum(tb.rf)
  rf.acc.tar[k] = acc.rf
  sens.rf.tar[k] = tb.rf[2,2]/sum(tb.rf[,2])
  spec.rf.tar[k] = tb.rf[1,1]/sum(tb.rf[,1])
}
var.sel = apply(sel,2,mean)*100
which(var.sel>=100)
names(dat_merged.log[itr,c(1,(isel+2))])
c(1,(isel+2))


##Volcano
set.seed(6041)
K = 10
n = nrow(X.merged)
folds = createFolds(dat_merged$Sex, k = 10, list = FALSE)
sel.vol = matrix(0, nrow=K, ncol=ncol(X.merged))
colnames(sel.vol) = names(X.merged.tr)
for(k in 1:K){
  itr = which(folds!=k)
  vol.out = volcano(X.merged[itr,], y.merged[itr], adjust = 'fdr')
  isel = which(vol.out$p.values < 0.1)
  sel.vol[k,isel] = 1  #vol.out$p.values[isel]
}
var.sel.vol = apply(sel.vol,2,mean)*100

dim(var.sel.vol)

sel.vol[1,]

length(which(var.sel.vol>0))
var.sel.vol[which(var.sel.vol>0)]
order(var.sel.vol)[1:5]

var.sel.vol['HMDB0030198']

?which.min


vec <- c(10, 3, 5, 8, 1, 9, 6, 2, 7, 4)

# Obtain the indexes of the 5 minimum values
min_indexes <- order(vol.out$p.values)[1:5]

# Print the result
print(min_indexes)

names(X.merged)[min_indexes]

folds <- createFolds(dat_merged$Sex, k = 10, list = TRUE, returnTrain = TRUE)

length(folds$Fold01)

table(dat_merged$Sex)

table(dat_merged[folds$Fold01,]$Sex)

folds[1]


###CV WilcoxTest
K = 10
n = nrow(X.merged.std)
folds = createFolds(dat_merged$Sex, k = 10, list = FALSE)
sel.wilcox = matrix(0, nrow=K, ncol=ncol(X.merged.std))
colnames(sel.wilcox) = names(X.merged.std)
covariates = names(X.merged.std)[sapply(X.merged.std, is.numeric)]
set.seed(6041)
for(k in 1:K){
  itr = which(folds!=k)
  p.matrix = matrix(NA,nrow=851,ncol=2)
  r=1
  for(c in covariates){
    p = wilcox.test(X.merged.std[itr,c]~y.merged[itr])$p.value
    p.matrix[r,1] = c
    p.matrix[r,2] = p
    r = r + 1
  }
  ind = which(p.adjust(p.matrix[,2], method = 'fdr') < 0.1)
  #isel = which(vol.out$p.values < 0.1)
  sel.wilcox[k,ind] = 1
}
var.sel.wilcox = apply(sel.wilcox,2,mean)*100
length(which(var.sel.wilcox > 0))

boxplot(X.merged.std[,'HMDB0241867']~y.merged)

which(var.sel.vol >=80)

intersect(which(var.sel.vol >=100), which(var.sel.wilcox >= 100))

names(X.merged)[intersect(which(var.sel.vol >=100), which(var.sel.wilcox >= 100))]

####Volcano analysis with elastic net
library(glmnet)
library(pROC)
xm = as.matrix(X.merged)
K = 10
n = nrow(X.merged)
folds = createFolds(dat_merged$Sex, k = 10, list = TRUE, returnTrain = TRUE)
en.acc.m = numeric(K)
top.var.list.en = list()
#auc.m = numeric(K)
#acc.gbm = numeric(K)
set.seed(6041)
for(k in 1:K){
  itr = unname(unlist(folds[k]))
  vol.out = volcano(X.merged[itr,], y.merged[itr])
  #isel = which(vol.out$p.values < 0.1)
  isel = order(vol.out$p.values)[1:10]
  top.var.list.en[k] = list(isel)
  lam = cv.glmnet(xm[itr,isel], y.merged[itr], family='binomial', alpha=0.5)
  lasso = glmnet(xm[itr,isel], y.merged[itr], family='binomial', alpha=0.5, lambda=lam$lambda.min)
  lasso.pred = predict(lasso, newx=xm[-itr,isel], type='response')
  preds.lasso = as.factor(as.numeric(lasso.pred>.5))
  tb.lasso = table(preds.lasso, y.merged[-itr])
  acc.lasso = sum(diag(tb.lasso))/sum(tb.lasso)
  en.acc.m[k] = acc.lasso
  #auc.m[k] = roc(y.merged[-itr],lasso.pred)$auc
}
names(X.merged)[sort(unique(unlist(top.var.list.en)))]
sort(unique(unlist(top.var.list.en)))

####Volcano analysis with Lasso
library(glmnet)
library(pROC)
xm = as.matrix(X.merged)
K = 10
n = nrow(X.merged)
folds = createFolds(dat_merged$Sex, k = 10, list = TRUE, returnTrain = TRUE)
lasso.acc.m = numeric(K)
top.var.list.lasso = list()
#auc.m = numeric(K)
#acc.gbm = numeric(K)
set.seed(6041)
for(k in 1:K){
  itr = unname(unlist(folds[k]))
  vol.out = volcano(X.merged[itr,], y.merged[itr])
  #isel = which(vol.out$p.values < 0.1)
  isel = order(vol.out$p.values)[1:5]
  top.var.list.lasso[k] = list(isel)
  lam = cv.glmnet(xm[itr,isel], y.merged[itr], family='binomial')
  lasso = glmnet(xm[itr,isel], y.merged[itr], family='binomial', lambda=lam$lambda.min)
  lasso.pred = predict(lasso, newx=xm[-itr,isel], type='response')
  preds.lasso = as.factor(as.numeric(lasso.pred>.5))
  tb.lasso = table(preds.lasso, y.merged[-itr])
  acc.lasso = sum(diag(tb.lasso))/sum(tb.lasso)
  lasso.acc.m[k] = acc.lasso
  #auc.m[k] = roc(y.merged[-itr],lasso.pred)$auc
}
mean(lasso.acc.m)

####Volcano analysis with glm
library(glmnet)
library(pROC)
#xm = as.matrix(X.merged)
K = 10
n = nrow(X.merged)
folds = createFolds(dat_merged$Sex, k = 10, list = TRUE, returnTrain = TRUE)
glm.acc.m = numeric(K)
top.var.list.glm = list()
#auc.m = numeric(K)
#acc.gbm = numeric(K)
set.seed(6041)
for(k in 1:K){
  itr = unname(unlist(folds[k]))
  vol.out = volcano(X.merged[itr,], y.merged[itr])
  #isel = which(vol.out$p.values < 0.1)
  isel = order(vol.out$p.values)[1:5]
  top.var.list.glm[k] = list(isel)
  df.glm = data.frame(y.merged,X.merged[,isel])
  gmo = glm(y.merged~., data=df.glm, subset = itr, family='binomial')
  glm.pred = predict(gmo, newdata= df.glm[-itr,], type='response')
  preds.glm = as.factor(as.numeric(glm.pred>.5))
  tb.glm = table(preds.glm, y.merged[-itr])
  acc.glm = sum(diag(tb.glm))/sum(tb.glm)
  glm.acc.m[k] = acc.glm
  #auc.m[k] = roc(y.merged[-itr],lasso.pred)$auc
}

mean(glm.acc.m)


####After removing correlated features

####Volcano analysis with Lasso
library(glmnet)
library(pROC)
xm = as.matrix(X.merged.rem.cor)
K = 10
n = nrow(X.merged.rem.cor)
folds = createFolds(dat_merged$Sex, k = 10, list = TRUE, returnTrain = TRUE)
lasso.acc.m = numeric(K)
top.var.list.lasso = list()
#auc.m = numeric(K)
#acc.gbm = numeric(K)
set.seed(6041)
for(k in 1:K){
  itr = unname(unlist(folds[k]))
  vol.out = volcano(X.merged.rem.cor[itr,], y.merged[itr])
  #isel = which(vol.out$p.values < 0.1)
  isel = order(vol.out$p.values)[1:5]
  top.var.list.lasso[k] = list(isel)
  lam = cv.glmnet(xm[itr,isel], y.merged[itr], family='binomial')
  lasso = glmnet(xm[itr,isel], y.merged[itr], family='binomial', lambda=lam$lambda.min)
  lasso.pred = predict(lasso, newx=xm[-itr,isel], type='response')
  preds.lasso = as.factor(as.numeric(lasso.pred>.5))
  tb.lasso = table(preds.lasso, y.merged[-itr])
  acc.lasso = sum(diag(tb.lasso))/sum(tb.lasso)
  lasso.acc.m[k] = acc.lasso
  #auc.m[k] = roc(y.merged[-itr],lasso.pred)$auc
}

mean(lasso.acc.m)
names(X.merged.rem.cor)[c(339,133,314,24,142)]
names(X.merged)[c(404,162,375,27,343)]


###NeuralNet CV (volcano analysis)
library(neuralnet)
library(nnet)
?neuralnet
dim(df_pos)

n = nrow(X.merged)
#df_neg_r = df_neg[sample(1:nrow(df_neg)),]
K = 10
folds = createFolds(dat_merged$Sex, k = 10, list = FALSE)
accuracy.neuralnet = numeric(K)
#auc.step.pos = numeric(K)
sens.nn = numeric(K)
spec.nn = numeric(K)
#acc.gbm = numeric(K)
#covariates = names(X.pos)[sapply(X.pos, is.numeric)]
set.seed(4061) # for reproducibility
for(k in 1:K){
  itr = which(folds!=k)
  vol.out = volcano(X.merged[itr,], y.merged[itr])
  #isel = which(vol.out$p.values < 0.1)
  isel = order(vol.out$p.values)[1:10]
  #top.var.list.glm[k] = list(isel)
  df.nn = data.frame(y.merged,X.merged[,isel])
  
  #nno = nnet(y.pos~., data=df_pos_step[itr,], size=7)
  nno = neuralnet(y.merged~., data=df.nn[itr,], hidden=c(9,8))
  
  pred = predict(nno, newdata = df.nn[-itr,], type= 'class')
  
  #lmo = glm(y.pos~., data=df_pos_step,subset = itr, family = 'binomial')
  #reg.fwd = step(lmo, direction="forward")
  #pred = predict(reg.fwd, newdata=df_pos_step[-itr,], type = 'response')
  preds.class = as.factor(as.numeric(pred[,2]>.5))
  cms = confusionMatrix(preds.class,y.merged[-itr], positive = '1')
  accuracy.neuralnet[k] = cms$overall[1]
  tb.vol = cms$table
  #acc.vol = sum(diag(tb.vol))/sum(tb.vol)
  sens.nn[k] = tb.vol[2,2]/sum(tb.vol[,2])
  spec.nn[k] = tb.vol[1,1]/sum(tb.vol[,1])
  #accuracy.neuralnet[k] = acc.vol
  #auc.step.pos[k] = roc(df_pos_step[-itr,]$y.pos, c(pred))$auc
}

mean(accuracy.neuralnet)
mean(spec.nn)
mean(sens.nn)
cms$tab
table(y.merged[itr])

###Separating males and females in the analysis

####Using the MALE records
nrow(dat_merged[dat_merged$Sex==1,])

dat_male = dat_merged[dat_merged$Sex==1,]

dim(dat_male)

X.male = dat_male
X.male$Case_Control = NULL
X.male$Sex= NULL
y.male = dat_male$Case_Control

?log

library(knockoff)
X.male.tr = log(X.male)

install.packages('ranger')
library(ranger)
result = knockoff.filter(X.male.tr, y.male, statistic = stat.random_forest)

library(randomForest)
K = 10
n = nrow(dat_male)
folds = createFolds(dat_male$Case_Control, k = 10, list = FALSE)
rf.acc.tar = numeric(K)
#top.var.list.rf = list()
sens.rf.tar = numeric(K)
spec.rf.tar =numeric(K)
#auc.m = numeric(K)
#acc.gbm = numeric(K)
set.seed(6041)
for(k in 1:K){
  itr = which(folds!=k)
  #vol.out = volcano(X.targeted[itr,], y.targeted[itr])
  #isel = which(vol.out$p.values < 0.1)
  #isel = order(vol.out$p.values)[1:10]
  #top.var.list.rf[k] = list(isel)
  #df.rf = data.frame(y.targeted,X.targeted[,isel])
  rf.mo = randomForest(Case_Control~., data=dat_male[itr,])
  rf.pred = predict(rf.mo, newdata= dat_male[-itr,], type='class')
  #preds.glm = as.factor(as.numeric(glm.pred>.5))
  tb.rf = table(rf.pred, dat_male$Case_Control[-itr])
  acc.rf = sum(diag(tb.rf))/sum(tb.rf)
  rf.acc.tar[k] = acc.rf
  sens.rf.tar[k] = tb.rf[2,2]/sum(tb.rf[,2])
  spec.rf.tar[k] = tb.rf[1,1]/sum(tb.rf[,1])
  #auc.m[k] = roc(y.merged[-itr],lasso.pred)$auc
}

##Univariate GLM
pr = ncol(X.male)
p.v = numeric(pr)
for(j in 1:pr){
  xj = X.male[,j]
  mod1 = glm(y.male~xj, family=binomial)
  p.v[j] = summary(mod1)$coefficients[2,4]
}

which(p.v < 0.05)


####Volcano analysis with Lasso
library(glmnet)
library(pROC)
xm = as.matrix(X.male)
K = 10
n = nrow(X.male)
folds = createFolds(dat_male$Case_Control, k = 10, list = TRUE, returnTrain = TRUE)
lasso.acc = numeric(K)
top.var.lasso.male = list()
#auc.m = numeric(K)
#acc.gbm = numeric(K)
set.seed(6041)
for(k in 1:K){
  itr = unname(unlist(folds[k]))
  vol.out = volcano(X.male[itr,], y.male[itr])
  #isel = which(vol.out$p.values < 0.1)
  isel = order(vol.out$p.values)[1:10]
  top.var.lasso.male[k] = list(isel)
  lam = cv.glmnet(xm[itr,isel], y.male[itr], family='binomial')
  lasso = glmnet(xm[itr,isel], y.male[itr], family='binomial', lambda=lam$lambda.min)
  lasso.pred = predict(lasso, newx=xm[-itr,isel], type='response')
  preds.lasso = as.factor(as.numeric(lasso.pred>.5))
  tb.lasso = table(preds.lasso, y.male[-itr])
  acc.lasso = sum(diag(tb.lasso))/sum(tb.lasso)
  lasso.acc[k] = acc.lasso
  #auc.m[k] = roc(y.merged[-itr],lasso.pred)$auc
}
mean(lasso.acc)
table(dat_male[itr,]$Case_Control)
M1 = names(X.male)[sort(unique(unlist(top.var.lasso.male)))]

####Volcano analysis with Lasso (Using bootstrapping instead of CV)
library(glmnet)
library(pROC)
xm = as.matrix(X.male)
K = 100
n = nrow(X.male)
#folds = createFolds(dat_male$Case_Control, k = 10, list = TRUE, returnTrain = TRUE)
lasso.acc = numeric(K)
top.var.lasso.male = list()
sens.b = numeric(K)
spec.b = numeric(K)
#auc.m = numeric(K)
#acc.gbm = numeric(K)
set.seed(6041)
for(k in 1:K){
  itr = sample(1:n, n, replace=TRUE)
  vol.out = volcano(X.male[itr,], y.male[itr])
  #isel = which(vol.out$p.values < 0.1)
  isel = order(vol.out$p.values)[1:10]
  top.var.lasso.male[k] = list(isel)
  lam = cv.glmnet(xm[itr,isel], y.male[itr], family='binomial')
  lasso = glmnet(xm[itr,isel], y.male[itr], family='binomial', lambda=lam$lambda.min)
  lasso.pred = predict(lasso, newx=xm[-itr,isel], type='response')
  preds.lasso = as.factor(as.numeric(lasso.pred>.5))
  tb.lasso = table(preds.lasso, y.male[-itr])
  acc.lasso = sum(diag(tb.lasso))/sum(tb.lasso)
  lasso.acc[k] = acc.lasso
  sens.b[k] = tb.lasso[2,2]/sum(tb.lasso[,2])
  spec.b[k] = tb.lasso[1,1]/sum(tb.lasso[,1])
  #auc.m[k] = roc(y.merged[-itr],lasso.pred)$auc
}
mean(lasso.acc)
mean(sens.b)
mean(spec.b)
par(mfrow=c(1,3))
boxplot(lasso.acc, xlab ='Accuracy')
boxplot(sens.b, xlab = 'Sensitivity')
boxplot(spec.b, xlab = 'Specificity')

###Using Random Forests
library(randomForest)
library(caret)
K = 10
n = nrow(X.male)
folds = createFolds(dat_male$Case_Control, k = 10, list = FALSE)
rf.acc.m = numeric(K)
top.var.list.rf = list()
sens.rf = numeric(K)
spec.rf = numeric(K)
#auc.m = numeric(K)
#acc.gbm = numeric(K)
set.seed(6041)
for(k in 1:K){
  itr = which(folds!=k)
  vol.out = volcano(X.male[itr,], y.male[itr])
  #isel = which(vol.out$p.values < 0.1)
  isel = order(vol.out$p.values)[1:10]
  top.var.list.rf[k] = list(isel)
  df.rf = data.frame(y.male,X.male[,isel])
  rf.mo = randomForest(y.male~., data=df.rf[itr,])
  rf.pred = predict(rf.mo, newdata= df.rf[-itr,], type='class')
  #preds.glm = as.factor(as.numeric(glm.pred>.5))
  tb.rf = table(rf.pred, y.male[-itr])
  acc.rf = sum(diag(tb.rf))/sum(tb.rf)
  rf.acc.m[k] = acc.rf
  sens.rf[k] = tb.rf[2,2]/sum(tb.rf[,2])
  spec.rf[k] = tb.rf[1,1]/sum(tb.rf[,1])
  #auc.m[k] = roc(y.merged[-itr],lasso.pred)$auc
}

mean(rf.acc.m)
mean(sens.rf)
mean(spec.rf)
par(mfrow=c(1,3))
boxplot(rf.acc.m, xlab ='Accuracy')
boxplot(sens.rf, xlab = 'Sensitivity')
boxplot(spec.rf, xlab = 'Specificity')
?boxplot

###Using SVMs
library(e1071)

svm.tune = e1071::tune(svm, train.x=X.male, train.y=y.male,
                       kernel='polynomial',
                       ranges=list(cost=10^(-2:4), gamma=c(0.25,0.5,1,1.5,2)))
svm.tune$best.parameters


xm = as.matrix(X.male)
K = 10
n = nrow(X.male)
folds = createFolds(dat_male$Case_Control, k = 10, list = FALSE)
svm.acc = numeric(K)
top.var.svm.male = list()
sens.svm = numeric(K)
spec.svm = numeric(K)
#auc.m = numeric(K)
#acc.gbm = numeric(K)
set.seed(6041)
for(k in 1:K){
  itr = which(folds!=k)
  vol.out = volcano(X.male[itr,], y.male[itr])
  #isel = which(vol.out$p.values < 0.1)
  isel = order(vol.out$p.values)[1:10]
  top.var.svm.male[k] = list(isel)
  svm.tune = e1071::tune(svm, train.x=X.male[itr,isel], train.y=y.male[itr],
                         kernel='radial',
                         ranges=list(cost=10^(-2:4), gamma=c(0.25,0.5,1,1.5,2)))
  svmo.final = svm(xm[itr,isel], y.male[itr], kernel='radial',
                   gamma=svm.tune$best.parameters$gamma,
                   cost=svm.tune$best.parameters$cost)
  
  #lam = cv.glmnet(xm[itr,isel], y.male[itr], family='binomial')
  #lasso = glmnet(xm[itr,isel], y.male[itr], family='binomial', lambda=lam$lambda.min)
  svm.pred = predict(svmo.final, newdata=xm[-itr,isel])
  #preds.lasso = as.factor(as.numeric(lasso.pred>.5))
  tb.svm = table(svm.pred, y.male[-itr])
  acc.svm = sum(diag(tb.svm))/sum(tb.svm)
  svm.acc[k] = acc.svm
  sens.svm[k] = tb.svm[2,2]/sum(tb.svm[,2])
  spec.svm[k] = tb.svm[1,1]/sum(tb.svm[,1])
  #auc.m[k] = roc(y.merged[-itr],lasso.pred)$auc
}

mean(svm.acc)
mean(sens.svm)
mean(spec.svm)
length(itr)
nrow(xm[-itr,isel])
fitted(svmo.final)
?predict.svm

par(mfrow=c(1,3))
boxplot(svm.acc, xlab ='Accuracy')
boxplot(sens.svm, xlab = 'Sensitivity')
boxplot(spec.svm, xlab = 'Specificity')

###NeuralNetworks
library(nnet)
n = nrow(X.male)
#df_neg_r = df_neg[sample(1:nrow(df_neg)),]
K = 10
folds = createFolds(dat_male$Case_Control, k = 10, list = FALSE)
accuracy.neuralnet = numeric(K)
#auc.step.pos = numeric(K)
sens.nn = numeric(K)
spec.nn = numeric(K)
#acc.gbm = numeric(K)
#covariates = names(X.pos)[sapply(X.pos, is.numeric)]
set.seed(4061) # for reproducibility
for(k in 1:K){
  itr = which(folds!=k)
  vol.out = volcano(X.male[itr,], y.male[itr])
  #isel = which(vol.out$p.values < 0.1)
  isel = order(vol.out$p.values)[1:10]
  #top.var.list.glm[k] = list(isel)
  df.nn = data.frame(y.male,X.male[,isel])
  
  #nno = nnet(y.pos~., data=df_pos_step[itr,], size=7)
  #nno = neuralnet(y.male~., data=df.nn[itr,], hidden=c(10,9))
  nno = nnet(y.male ~ ., data = df.nn[itr,], size = 6, maxit = 600, decay = 0.1, trace = FALSE)
  pred = predict(nno, newdata = df.nn[-itr,], type= 'class')
  
  #lmo = glm(y.pos~., data=df_pos_step,subset = itr, family = 'binomial')
  #reg.fwd = step(lmo, direction="forward")
  #pred = predict(reg.fwd, newdata=df_pos_step[-itr,], type = 'response')
  #preds.class = as.factor(as.numeric(pred[,2]>.5))
  cms = confusionMatrix(as.factor(pred),y.male[-itr], positive = '1')
  accuracy.neuralnet[k] = cms$overall[1]
  tb.vol = cms$table
  #acc.vol = sum(diag(tb.vol))/sum(tb.vol)
  sens.nn[k] = tb.vol[2,2]/sum(tb.vol[,2])
  spec.nn[k] = tb.vol[1,1]/sum(tb.vol[,1])
  #accuracy.neuralnet[k] = acc.vol
  #auc.step.pos[k] = roc(df_pos_step[-itr,]$y.pos, c(pred))$auc
}
mean(accuracy.neuralnet)
mean(sens.nn)
mean(spec.nn)

?neuralnet
par(mfrow=c(1,3))
boxplot(accuracy.neuralnet, xlab ='Accuracy')
boxplot(sens.nn, xlab = 'Sensitivity')
boxplot(spec.nn, xlab = 'Specificity')

###Using FEMALE records

nrow(dat_merged[dat_merged$Sex==2,])

dat_female = dat_merged[dat_merged$Sex==2,]

dim(dat_female)

X.female = dat_female
X.female$Case_Control = NULL
X.female$Sex= NULL
y.female = dat_female$Case_Control

###Using Random Forests
library(randomForest)
K = 10
n = nrow(X.female)
folds = createFolds(dat_female$Case_Control, k = 10, list = TRUE, returnTrain = TRUE)
rf.acc.f = numeric(K)
top.var.list.rf = list()
mod.lst.rf = list()
#auc.m = numeric(K)
#acc.gbm = numeric(K)
set.seed(6041)
for(k in 1:K){
  itr = unname(unlist(folds[k]))
  vol.out = volcano(X.female[itr,], y.female[itr])
  #isel = which(vol.out$p.values < 0.1)
  isel = order(vol.out$p.values)[1:10]
  top.var.list.rf[k] = list(isel)
  df.rf = data.frame(y.female,X.female[,isel])
  rf.mo = randomForest(y.female~., data=df.rf[itr,])
  rf.pred = predict(rf.mo, newdata= df.rf[-itr,], type='class')
  mod.lst.rf[k] = list(rf.mo)
  #preds.glm = as.factor(as.numeric(glm.pred>.5))
  tb.rf = table(rf.pred, y.female[-itr])
  acc.rf = sum(diag(tb.rf))/sum(tb.rf)
  rf.acc.f[k] = acc.rf
  #auc.m[k] = roc(y.merged[-itr],lasso.pred)$auc
}

mean(rf.acc.f)
summary(mod.lst.rf[[3]])
summary(rf.mo)
rf.mo$importance
mod.lst.rf[[5]]$importance


####Volcano analysis with glm
library(glmnet)
library(pROC)
#xm = as.matrix(X.merged)
K = 10
n = nrow(X.female)
folds = createFolds(dat_female$Case_Control, k = 10, list = TRUE, returnTrain = TRUE)
glm.acc.f = numeric(K)
top.var.list.glm = list()
mod.lst.glm.f = list()
sens = numeric(K)
spec = numeric(K)
#auc.m = numeric(K)
#acc.gbm = numeric(K)
set.seed(6041)
for(k in 1:K){
  itr = unname(unlist(folds[k]))
  vol.out = volcano(X.female[itr,], y.female[itr])
  #isel = which(vol.out$p.values < 0.1)
  isel = order(vol.out$p.values)[1:10]
  top.var.list.glm[k] = list(isel)
  df.glm = data.frame(y.female,X.female[,isel])
  gmo = glm(y.female~., data=df.glm, subset = itr, family='binomial')
  glm.pred = predict(gmo, newdata= df.glm[-itr,], type='response')
  mod.lst.glm.f = list(gmo)
  preds.glm = as.factor(as.numeric(glm.pred>.5))
  tb.glm = table(preds.glm, y.female[-itr])
  acc.glm = sum(diag(tb.glm))/sum(tb.glm)
  sens[k] = tb.glm[2,2]/sum(tb.glm[,2])
  spec[k] = tb.glm[1,1]/sum(tb.glm[,1])
  glm.acc.f[k] = acc.glm
  #auc.m[k] = roc(y.merged[-itr],lasso.pred)$auc
}
table(y.female[itr])
mean(glm.acc.f)
mean(spec)
mean(sens)
length(itr)
boxplot(glm.acc.f,sens, spec)

F1 = names(X.female)[s1]
s1 = sort(unique(unlist(top.var.list.glm)))
s2 = sort(unique(unlist(top.var.list.rf)))
intersect(M1,F1)
####Repeated CV
R = 5
K = 10
n = nrow(X.female)
#folds = cut(1:n, K, labels=FALSE)
folds = createFolds(dat_female$Case_Control, k = 10, list = FALSE)
glm.acc.f.rep = numeric(K)
top.var.list.glm = list()
sens = NULL
spec = NULL
#mod.lst.glm.f = list()
#auc.m = numeric(K)
#acc.gbm = numeric(K)
set.seed(6041)

for(r in 1:R){
  is = sample(1:n,n)
  X.female = X.female[is,]
  y.female = y.female[is]
  
  for(k in 1:K){
    itr = which(folds!=k)
    vol.out = volcano(X.female[itr,], y.female[itr])
    #isel = which(vol.out$p.values < 0.1)
    isel = order(vol.out$p.values)[1:10] ##use 3 or 5
    top.var.list.glm[k] = list(isel)
    df.glm = data.frame(y.female,X.female[,isel])
    gmo = glm(y.female~., data=df.glm, subset = itr, family='binomial')
    glm.pred = predict(gmo, newdata= df.glm[-itr,], type='response')
    #mod.lst.glm.f = list(gmo)
    preds.glm = as.factor(as.numeric(glm.pred>.5))
    tb.glm = table(preds.glm, y.female[-itr])
    acc.glm = sum(diag(tb.glm))/sum(tb.glm)
    glm.acc.f.rep[k+(r-1)*K] = acc.glm
    sens[k+(r-1)*K] = tb.glm[2,2]/sum(tb.glm[,2])
    spec[k+(r-1)*K] = tb.glm[1,1]/sum(tb.glm[,1])
    #auc.m[k] = roc(y.merged[-itr],lasso.pred)$auc
  }
}

mean(glm.acc.f.rep)
mean(sens)
mean(spec)

boxplot(glm.acc.f.rep, sens, spec)
par(mfrow=c(1,3))
boxplot(glm.acc.f.rep, xlab ='Accuracy')
boxplot(sens, xlab = 'Sensitivity')
boxplot(spec, xlab = 'Specificity')


dim(dat_)
####Volcano vs Wilcoxon
###CV WilcoxTest
dim(X.male)
K = 10
n = nrow(X.female)
folds = cut(1:n, breaks=K, labels=FALSE)
sel.wilcox = matrix(0, nrow=K, ncol=ncol(X.female))
colnames(sel.wilcox) = names(X.female)
covariates = names(X.female)[sapply(X.female, is.numeric)]
set.seed(6041)
for(k in 1:K){
  itr = which(folds!=k)
  p.matrix = matrix(NA,nrow=851,ncol=2)
  r=1
  for(c in covariates){
    p = wilcox.test(X.female[itr,c]~y.female[itr])$p.value
    p.matrix[r,1] = c
    p.matrix[r,2] = p
    r = r + 1
  }
  ind = which(p.matrix[,2] < 0.05)
  #isel = which(vol.out$p.values < 0.1)
  sel.wilcox[k,ind] = 1
}
warnings()
var.sel.wilcox = apply(sel.wilcox,2,mean)*100

##Volcano
set.seed(6041)
K = 10
n = nrow(X.female)
folds = createFolds(dat_female$Case_Control, k = 10, list = FALSE)
sel.vol = matrix(0, nrow=K, ncol=ncol(X.female))
colnames(sel.vol) = names(X.female)
for(k in 1:K){
  itr = which(folds!=k)
  vol.out = volcano(X.female[itr,], y.female[itr])
  isel = which(vol.out$p.values < 0.05)
  sel.vol[k,isel] = 1  #vol.out$p.values[isel]
}
var.sel.vol = apply(sel.vol,2,mean)*100

names(X.male)[intersect(which(var.sel.vol>=100), which(var.sel.wilcox>=100))]


boxplot(log(dat_male$HMDB0006528)~dat_male$Case_Control)
t.test(dat_male$HMDB0006528~dat_male$Case_Control)


set.seed(6041)
K = 10
n = nrow(X.male)
folds = createFolds(dat_male$Case_Control, k = 10, list = FALSE)
accuracy.glm = numeric(K)
sens.glm = numeric(K)
spec.glm = numeric(K)
#sel.vol = matrix(0, nrow=K, ncol=ncol(X.merged))
#colnames(sel.vol) = names(X.merged)
for(k in 1:K){
  itr = which(folds!=k)
  glm.mod = glm(Case_Control~., data = dat_male[itr,c(1,(which(p.v < 0.05)+2))], family = 'binomial')
  pred = predict(glm.mod, newdata =dat_male[-itr,c(1,(which(p.v < 0.05)+2))])
  pred.class = as.factor(as.numeric(pred>.5))
  
  cms = confusionMatrix(pred.class,dat_male$Case_Control[-itr], positive = '1')
  accuracy.glm[k] = cms$overall[1]
  tb.vol = cms$table
  #acc.vol = sum(diag(tb.vol))/sum(tb.vol)
  sens.glm[k] = tb.vol[2,2]/sum(tb.vol[,2])
  spec.glm[k] = tb.vol[1,1]/sum(tb.vol[,1])
  
}


table(dat_male$Case_Control[itr])

mean(accuracy.glm)

boxplot(accuracy.glm,sens.glm, spec.glm)
boxp