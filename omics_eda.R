require(caret)
require(ISLR)
install.packages('pls')
library(pls)

##import dataset
dat = read.csv("C:/Upamanyu/Ireland/Data Science/Classes/Thesis project/dnbc.csv")

head(dat)


dim(dat)

table(dat$Sex)


X = dat
X$Case_Control = NULL
y = dat$Case_Control
str(dat$Case_Control)

dat$Case_Control = factor(dat$Case_Control)


####EDA###
require(DataExplorer)
plot_bar(dat)

?plot_boxplot

boxplot(dat$MetSO~dat$Case_Control, ylim = c(-2,5))
pdf("C:/Upamanyu/Ireland/Data Science/Classes/Thesis project/output_plot.pdf")
plot_histogram(dat)
dev.off()
dat$Case_Control = as.factor(dat$Case_Control)
dat$Sex = as.factor(dat$Sex)
p = wilcox.test(MetSO~Case_Control,data=dat)
p$p.value

?p.adjust

o = ew.heatmap(X, y, col.dendrogram=TRUE)

######Univariate analysis
covariates <- names(dat)[sapply(dat, is.numeric)][-c(1,2)]
dat[,c]
length(covariates)
dat$
p.matrix = matrix(NA,nrow=138,ncol=2)
r=1
for(c in covariates){
  p = wilcox.test(dat[,c]~dat[,1])$p.value
  p.matrix[r,1] = c
  p.matrix[r,2] = p
  r = r + 1
}

##Univariate GLM
pr = ncol(X)
p.v = numeric(pr)
for(j in 1:pr){
  xj = X[,j]
  mod1 = glm(y~xj, family=binomial)
  p.v[j] = summary(mod1)$coefficients[2,4]
}

which(p.v < 0.05)
X[,c(38,48)] #MetSO LPC_16.1


###check the distributions wrt outcome
### T test?? or log transform
##check if bimodal or not
##PC_42.1, PC_O42.2
##should we remove all of those observations
##question to jane - why is the outlier value so low for PC_42.1 (is the value correct? can we remove?)
##investigate the spikes(check if they are all males or females, control or not)
##maintain separate doc for reference
##create a doc
 ##dimension of the dataset(unbalanced data - matching broken?)
##reduce data for only the subjects that have a value above a certain cutoff

p.matrix[,c(82,49,50,35)]
#"PC_42.1", "LPC_18.2", "LPC_20.3", "MetSO"
plot_boxplot(dat[,c("Case_Control","PC_42.1", "LPC_18.2", "LPC_20.3", "MetSO")],by="Case_Control")

p.adjust(p.matrix[,2],'fdr')

# ------------------------------------------------------------
# pre-filtering, here using a correlation filter

M = cor(X[,-1])
ic = findCorrelation(M,cutoff=.90)
nms = names(X)[-1]
frm = nms[ic]
XS = X[,-(ic+1)] # subset of less correlated features
head(XS) 

str(XS)
names(XS)


###RFE with RF (Full Features)

n = nrow(dat)

itrain = sample(1:n,round(0.7*n), replace=FALSE)
dat.train = dat[itrain,]
x.train = dat.train[,-1]
y.train = dat.train$Case_Control

set.seed(4061)
subsets <- c(1:5, 10, 15, 20, 25, 138)
ctrl <- rfeControl(functions = rfFuncs,
                   method = "cv",
                   number = 10,
                   # method = "repeatedcv",
                   # repeats = 5,
                   verbose = FALSE)
rf.rfe <- rfe(x.train, y.train,
              sizes = subsets,
              rfeControl = ctrl)

pr.val = predict(rf.rfe,newdata = dat[-itrain,])

tb = table(dat[-itrain,1], pr.val$pred)

acc = sum(diag(tb))/sum(tb)


###RFE with RF (Reduced Features)

n = nrow(dat)

itrain = sample(1:n,round(0.7*n), replace=FALSE)
dat.train = dat[itrain,]
x.train = XS[itrain,]
y.train = dat.train$Case_Control

set.seed(4061)
subsets <- c(1:5, 10, 15, 20, 25, 125)
ctrl <- rfeControl(functions = rfFuncs,
                   method = "cv",
                   number = 10,
                   # method = "repeatedcv",
                   # repeats = 5,
                   verbose = FALSE)
rf.rfe <- rfe(x.train, y.train,
              sizes = subsets,
              rfeControl = ctrl)

pr.val = predict(rf.rfe,newdata = dat[-itrain,])

tb = table(dat[-itrain,1], pr.val$pred)

acc = sum(diag(tb))/sum(tb)


## Pipeline for Males##

dat.male= dat[which(dat$Sex==1),]
x.male = dat.male[,-c(1,2)]
head(x.male)

y.male = dat.male[,1]

set.seed(4061)
subsets <- c(1:5, 10, 15, 20, 25, 138)
ctrl <- rfeControl(functions = rfFuncs,
                   method = "cv",
                   number = 10,
                   # method = "repeatedcv",
                   # repeats = 5,
                   verbose = FALSE)
rf.rfe <- rfe(x.male, y.male,
              sizes = subsets,
              rfeControl = ctrl)



####
##univariate glm
##sensitivity, specificity
##no clear signal
##total 180 features but we 140
##Simple PCA check for outliers



summary(dat$PC_42.1)

table(dat[which(dat$PC_42.1 <0.05), 2])
table(dat[which(dat$PC_O42.2 <0.1), 1])

dat$PC_42.1[which(dat$PC_42.1 <0.05)]

head(dat$Putrescine)



###Forward Selection
library(leaps)

reg.fwd = regsubsets(Case_Control~., data=dat, nvmax = 5, method = 'forward')

mod = summary(reg.fwd)$which[which.max(summary(reg.fwd)$adjr2),]

names(which(mod))

##"Lys"         "MetSO"       "LPC_16.1"    "PC_38.4"     "SM_20.2"  "SM_24.0" 
summary(reg.fwd)

mod.fwd = glm(Case_Control~Lys + MetSO + LPC_16.1 + PC_38.4 + SM_20.2 + SM_24.0, data=dat, family = 'binomial')
pred =predict(mod.fwd, type="response")
preds = as.factor(as.numeric(pred>.5))

tb.fwd = table(preds, dat$Case_Control)
acc.fwd = sum(diag(tb.fwd))/sum(tb.fwd) #0.5745614

####PCA Analysis

names(X)
X.pca = X
X.pca$Sex = NULL
pca.unscaled = prcomp(X.pca) 
pca.scaled = prcomp(X.pca,scale=TRUE)

par(mfrow=c(1,3)) # scree plots
plot(pca.unscaled)
plot(pca.scaled)
plot(pca.scaled.2)

cols = rep(8,ncol(X)) 
cols[which(names(X)=="MetSO")] = 2
barplot(pca.scaled$rotation[,1], las=2, col = cols)

which(pca.scaled$rotation[,1] < 0)

summary(pca.scaled)
##removing noise?? 
##near zero variance features? remove them
##use different filters
##barplots of first 3 PCAs

##new techniques:
#-----------------------#
##Independent Component Analysis (similar to PCA) - maybe useful
##Partial Least Squares



# plot the data on its first 2 dimensions in each space: 
par(mfrow=c(1,1))
plot(x[,1:2], pch=20, col=y, main="Data in original space") 
biplot(pca.unscaled, main="Data in PCA space")

biplot(pca.scaled, main="Data in PCA space")
abline(v=0, col='orange')
# see the binary separation in orange along PC1? 
# re-use this into biplot in original space:
pca.cols = c("blue","orange")[1+as.numeric(pca.unscaled$x[,1]>0)]
plot(x[,1:2], pch=20, col=pca.cols, 
     main="Data in original space\n colour-coded using PC1-split") 



###Lasso
library(glmnet)
set.seed(4061) # for reproducibility
n = nrow(X)
xm = as.matrix(X.pca)
K = 10
folds = cut(1:n, breaks=K, labels=FALSE)
sel = matrix(0, nrow=K, ncol=ncol(X.pca))
colnames(sel) = names(X.pca)
for(k in 1:K){
  itr = which(folds!=k)
  lasso.cv = cv.glmnet(xm[itr,], y[itr], family='binomial')
  lasso = glmnet(xm[itr,], y[itr], lambda=lasso.cv$lambda.min,  family='binomial')
  isel = which(coef(lasso)[-1] != 0)
  sel[k,isel] = 1
}
var.sel = apply(sel,2,mean)*100
#LPC_20.3 (10), MetSO (10), PC_O34.3 (20)

mod.lasso = glm(Case_Control~MetSO + LPC_20.3 + PC_O34.3, data=dat, family = 'binomial')
pred =predict(mod.lasso, type="response")
preds = as.factor(as.numeric(pred>.5))

tb.lasso = table(preds, dat$Case_Control)
acc.lasso = sum(diag(tb.lasso))/sum(tb.lasso) #0.5789474

sum(var.sel > 0)


##
glm.metso = glm(Case_Control~MetSO, data=dat, family= binomial)

preds = predict(glm.metso, type='response')

pred = as.factor(as.numeric(preds>0.5))

tb.glm = table(pred, dat$Case_Control)

acc.glm = sum(diag(tb.glm))/sum(tb.glm) #0.5285088


####PLS Model
pls.mod = plsr(Case_Control~., data = dat, scale=TRUE, validation = 'CV') ##not working
?plsda

install.packages("plsgenomics")
library(plsgenomics)

pls.mod2 = pls.lda(X, y, nruncv = 10, ncomp = 5)

tb.pls = table(y, pls.mod2$pred.lda.out$class)
sum(diag(tb.pls))/sum(tb.pls)
summary(pls.mod2)

##modify the code if possible!!
trainControl = trainControl(method = "cv", number = 10)
plsda_model = train(Case_Control ~ ., data = dat, method = "pls", 
                     trControl = trainControl, tuneLength = 20)


summary(plsda_model) ##0.5480193 ##why error with summary?

train_pred = predict(plsda_model, newdata = dat)

confusionMatrix(test_pred, dat$Case_Control) ##0.6447!!

###Pre filtering with Fold Change cutoff

set.seed(6041)
K = 10
n=nrow(X)
folds = cut(1:n, breaks=K, labels=FALSE)
sel.vol = matrix(0, nrow=K, ncol=ncol(X))
colnames(sel.vol) = names(X)
p = ncol(X)
for(k in 1:K){
  itr = which(folds!=k)
  for(j in 1:p){
    xj = X[itr, j]
    if(!is.numeric(xj)){ xj = as.numeric(xj) }
    #fc[j] = mean(xj[y==ref])/mean(xj[y!=ref])
    fc = abs(log2(mean(xj[y[itr]!="0"])/mean(xj[y[itr]=="0"])))
    if(fc < 0.1){
      sel.vol[k,j] = 1
    }
  }
}
?abs
X[,1]
var.sel.vol = apply(sel.vol,2,mean)*100

sel.vol[10,139]

length(which(var.sel.vol==100))
length(X)

X.reduced = X[,-which(var.sel.vol==100)]
###124 features were removed, 15 features kept

##using lasso
library(glmnet)
set.seed(4061) # for reproducibility
n = nrow(X)
#xm = as.matrix(X.pca)
K = 10
folds = cut(1:n, breaks=K, labels=FALSE)
sel = matrix(0, nrow=K, ncol=ncol(X.reduced))
colnames(sel) = names(X.reduced)
for(k in 1:K){
  itr = which(folds!=k)
  lasso.cv = cv.glmnet(xm.red[itr,], y[itr], family='binomial')
  lasso = glmnet(xm.red[itr,], y[itr], lambda=lasso.cv$lambda.min,  family='binomial')
  isel = which(coef(lasso)[-1] != 0)
  sel[k,isel] = 1
}
var.sel.red = apply(sel,2,mean)*100

##lasso end

###PCA on the reduced dataset

####PCA Analysis

names(X.reduced)
pca.scaled.red = prcomp(X.reduced,scale=TRUE)

par(mfrow=c(1,3)) # scree plots
plot(pca.unscaled)
plot(pca.scaled)
plot(pca.scaled.2)

cols = rep(8,ncol(X)) 
cols[which(names(X)=="MetSO")] = 2
barplot(pca.scaled$rotation[,1], las=2, col = cols)

which(pca.scaled$rotation[,1] < 0)

summary(pca.scaled.red)

names(X.reduced)
ncol(dat.reduced)
names(dat.reduced)
dat.reduced = dat[,-which(var.sel.vol==100) - 1]
glm.reduced = glm(Case_Control~., data=dat.reduced, family= binomial)

preds.red = predict(glm.reduced, type='response')

pred.red = as.factor(as.numeric(preds.red>0.5))

tb.glm.red = table(pred.red, dat.reduced$Case_Control)

acc.glm.red = sum(diag(tb.glm.red))/sum(tb.glm.red)
##Accuracy with glm model with reduced features: 0.5833333


levels(dat.reduced$Case_Control) = c("Control","Case")

trC = trainControl(method="cv", number=5,
                   savePredictions = TRUE, 
                   classProbs = TRUE)
co = train(Case_Control~., data=dat.reduced, method='glmStepAIC', 
           trControl=trC, distribution='binomial') 

str(dat.reduced$Case_Control)
980-851
summary(co$finalModel)
names(co)
preds <- predict(co, dat.reduced) 
table(dat.reduced$Case_Control,preds)
(146+120)/(146+86+104+120) ##58.3 %
warnings()



set.seed(4061)
subsets <- c(1:15)
ctrl <- rfeControl(functions = rfFuncs,
                   method = "cv",
                   number = 10,
                   # method = "repeatedcv",
                   # repeats = 5,
                   verbose = FALSE)
rf.rfe <- rfe(X.reduced, y,
              sizes = subsets,
              rfeControl = ctrl)

pr.val = predict(rf.rfe,newdata = dat[-itrain,])

tb = table(dat[-itrain,1], pr.val$pred)

acc = sum(diag(tb))/sum(tb)


###Neural Network
library(nnet)

nno = nnet(Case_Control~., data=dat.reduced, size=10)
nno$fitted.values
nnop = predict(nno, dat.reduced)
pred.red.nn = as.factor(as.numeric(nnop>0.5))

tb.nn.red = table(pred.red.nn, dat.reduced$Case_Control)

acc.nn.red = sum(diag(tb.nn.red))/sum(tb.nn.red) ##0.7412281

varImp(nno)


set.seed(4061) # for reproducibility
n = nrow(X.reduced)
K = 10
folds = cut(1:n, breaks=K, labels=FALSE)
accuracy.nn = numeric(K)
for(k in 1:K){
  itr = which(folds!=k)
  nno = nnet(Case_Control~., data=dat.reduced, subset = itr, size=10)
  nnop = predict(nno, dat.reduced[-itr,])
  pred.red.nn = as.factor(as.numeric(nnop>0.5))
  tb.nn.red = table(pred.red.nn, dat.reduced[-itr,]$Case_Control)
  acc.nn.red = sum(diag(tb.nn.red))/sum(tb.nn.red)
  accuracy.nn[k] = acc.nn.red
}

mean(accuracy.nn)


### SVM##
library(e1071)

xm.red = as.matrix(X.reduced)

svmo.pol = svm(xm.red, y, kernel='radial')
svmy.pol = fitted(svmo.pol)
tb.svm = table(y, svmy.pol)
acc.svm = sum(diag(tb.svm))/sum(tb.svm) 


set.seed(4061) # for reproducibility
n = nrow(X.reduced)
K = 10
folds = cut(1:n, breaks=K, labels=FALSE)
accuracy.svm = numeric(K)
for(k in 1:K){
  itr = which(folds!=k)
  #nno = nnet(Case_Control~., data=dat.reduced, subset = itr, size=10)
  svmo.rad = svm(xm.red[itr,], y[itr], kernel='radial')
  #svmy.rad = fitted(svmo.pol)
  pred.rad = predict(svmo.rad, newdata = xm.red[-itr,])
  tb.svm = table(y[-itr], pred.rad)
  acc.svm = sum(diag(tb.svm))/sum(tb.svm)
  accuracy.svm[k] = acc.svm
}

mean(accuracy.svm) #0.5440097
