##importing targeted dataset

df_targeted = read.csv("C:/Upamanyu/Ireland/Data Science/Classes/Thesis project/dnbc.csv")

dim(df_targeted)

table(df_targeted$Sex)
names(df_targeted)

plot_bar(df_targeted$Case_Control)
?plot_bar

install.packages("summarytools")
library(summarytools)

install.packages("stargazer")
library(stargazer)
stargazer(df_targeted[,c(1:10)], type = "latex", title = "Descriptive Statistics", summary = TRUE)
dfSummary(df_targeted[,c(1:10)])

library(ggplot2)
class_counts <- table(df_targeted$Sex)

# Convert to a data frame
class_counts_df <- as.data.frame(class_counts)
colnames(class_counts_df) <- c("class", "count")

# Create bar plot
ggplot(class_counts_df, aes(x = class, y = count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "blue") +
  theme_minimal() +
  labs(title = "Number of Samples belonging to Males and Females", x = "Sex", y = "Count") +
  theme(
    axis.title.x = element_text(size = 20),  # Change the font size of x-axis label
    axis.title.y = element_text(size = 20)
  )

normalized_data <- data.frame(lapply(df_targeted[,-c(1,2)], normalize_by_median))

normalized_data = log10(normalized_data)

scaled_normalized_data = data.frame(scale(normalized_data, center = TRUE, scale = TRUE))
?scale
dim(scaled_normalized_data)
plot_histogram(scaled_normalized_data)
normalized_data$
barplot(df_targeted$Case_Control)
df_targeted = df_targeted[sample(1:nrow(df_targeted)),]
par(mfrow = c(2, 3))
boxplot(df_targeted[,c('MetSO', 'LPC_20.3', 'LPC_18.2', 'LPC_18.1', 'PC_O34.3', 'SM_20.2')] ~ df_targeted$Case_Control)
boxplot(scaled_normalized_data$MetSO ~ df_targeted$Case_Control, cex.axis = 1.5, xlab = "", names = c('Control','Case'), ylab = "MetSO", col = "skyblue", cex.lab = 1.5)
boxplot(scaled_normalized_data$LPC_20.3 ~ df_targeted$Case_Control, cex.axis = 1.5, names = c('Control','Case'), xlab="", ylab = "LPC_20.3", col = "skyblue", cex.lab = 1.5)
boxplot(scaled_normalized_data$LPC_18.2 ~ df_targeted$Case_Control, cex.axis = 1.5, names = c('Control','Case'), xlab="", ylab = "LPC_18.2", col = "skyblue", cex.lab = 1.5)
boxplot(scaled_normalized_data$LPC_18.1 ~ df_targeted$Case_Control, cex.axis = 1.5, names = c('Control','Case'), xlab="", ylab = "LPC_18.1", col = "skyblue", cex.lab = 1.5)
boxplot(scaled_normalized_data$PC_O34.3 ~ df_targeted$Case_Control, cex.axis = 1.5, names = c('Control','Case'), xlab="", ylab = "PC_O34.3", col = "skyblue", cex.lab = 1.5)
boxplot(scaled_normalized_data$SM_20.2 ~ df_targeted$Case_Control, cex.axis = 1.5, names = c('Control','Case'), xlab="", ylab = "SM_20.2", col = "skyblue", cex.lab = 1.5)

plot_boxplot(scaled_normalized_data[,c(1:30)])
?plot_boxplot
?boxplot

boxplot(scaled_normalized_data[,c(1:20)], col = "green", cex.lab = 1.5, las=2)
#df_targeted$Case_Control = ifelse(df_targeted$Case_Control == 'CASE', 1, 0)
#dat_merged$Sex = ifelse(dat_merged$Sex == 'Male', 1, 2)
df_targeted$Case_Control = factor(df_targeted$Case_Control)
df_targeted$Sex = factor(df_targeted$Sex)

library(MetaboAnalystR)
?Normalization
norma
mSet<-Normalization(df_targeted, "MedianNorm", "LogNorm", "AutoNorm", ratio=FALSE, ratioNum=20)

X.targeted = df_targeted
X.targeted$Case_Control = NULL
X.targeted$Sex= NULL
y.targeted = df_targeted$Case_Control


###Doing a pca analysis
pca.tar = pca(X.targeted, ncomp = 2, scale = TRUE)

?plotIndiv
plotIndiv(pca.tar, group = y.targeted, ind.names = FALSE,
          legend = TRUE, 
          title = 'PCA Targeted Dataset 1 - 2')

nrow(pca.tar$x)

which(pca.tar$x[,'PC1'] > 20) ##200, 181, 49

##PLS

plsda.tar <- plsda(X.targeted,y.targeted, ncomp = 10)

set.seed(6041) # For reproducibility with this handbook, remove otherwise
perf.plsda.srbct <- perf(plsda.tar, validation = 'Mfold', folds = 10, 
                         progressBar = FALSE,  # Set to TRUE to track progress
                         nrepeat = 50)         # We suggest nrepeat = 50

plot(perf.plsda.srbct, sd = TRUE, legend.position = 'horizontal')


final.plsda.tar <- plsda(X.targeted,y.targeted, ncomp = 2)

plotIndiv(final.plsda.tar, ind.names = FALSE, legend=TRUE,
          comp=c(1,2), ellipse = TRUE, 
          title = 'PLS-DA on comp 1-2',
          X.label = 'PLS-DA comp 1', Y.label = 'PLS-DA comp 2')


##Volcano Analysis

set.seed(6041)
n = nrow(X.targeted)
K = 10
folds = cut(1:n, breaks=K, labels=FALSE)
sel.vol = matrix(0, nrow=K, ncol=ncol(X.targeted))
colnames(sel.vol) = names(X.targeted)
for(k in 1:K){
  itr = which(folds!=k)
  vol.out = volcano(X.targeted[itr,], y.targeted[itr], adjust = 'hochberg')
  isel = which(vol.out$p.values < 0.05)
  sel.vol[k,isel] = 1
}
var.sel.vol = apply(sel.vol,2,mean)*100
which(var.sel.vol>=80)
p.adjust(vol.out$p.values, method='fdr')

####Volcano analysis with glm
library(glmnet)
library(pROC)
#xm = as.matrix(X.merged)
K = 10
n = nrow(X.targeted)
folds = createFolds(df_targeted$Sex, k = 10, list = FALSE)
glm.acc.tar = numeric(K)
top.var.glm.tar = list()
#auc.m = numeric(K)
#acc.gbm = numeric(K)
set.seed(6041)
for(k in 1:K){
  itr = which(folds!=k)
  vol.out = volcano(X.targeted[itr,], y.targeted[itr])
  #isel = which(vol.out$p.values < 0.1)
  isel = order(vol.out$p.values)[1:5]
  top.var.glm.tar[k] = list(isel)
  df.glm = data.frame(y.targeted,X.targeted[,isel])
  gmo = glm(y.targeted~., data=df.glm, subset = itr, family='binomial')
  glm.pred = predict(gmo, newdata= df.glm[-itr,], type='response')
  preds.glm = as.factor(as.numeric(glm.pred>.5))
  tb.glm = table(preds.glm, y.targeted[-itr])
  acc.glm = sum(diag(tb.glm))/sum(tb.glm)
  glm.acc.tar[k] = acc.glm
  #auc.m[k] = roc(y.merged[-itr],lasso.pred)$auc
}

mean(glm.acc.tar)

table(df_targeted$Sex)


####Repeated CV
R = 5
K = 10
n = nrow(X.targeted)
#folds = cut(1:n, K, labels=FALSE)
folds = createFolds(df_targeted$Sex, k = 10, list = FALSE)
glm.acc.rep.tar = NULL
top.var.list.glm.rep = list()
sens.targeted = NULL
spec.targeted = NULL
#mod.lst.glm.f = list()
#auc.m = numeric(K)
#acc.gbm = numeric(K)
set.seed(6041)

for(r in 1:R){
  is = sample(1:n,n)
  X.targeted = X.targeted[is,]
  y.targeted = y.targeted[is]
  
  for(k in 1:K){
    itr = which(folds!=k)
    vol.out = volcano(X.targeted[itr,], y.targeted[itr])
    #isel = which(vol.out$p.values < 0.1)
    isel = order(vol.out$p.values)[1:10] ##use 3 or 5
    top.var.list.glm.rep[k+(r-1)*K] = list(isel)
    df.glm = data.frame(y.targeted,X.targeted[,isel])
    gmo = glm(y.targeted~., data=df.glm, subset = itr, family='binomial')
    glm.pred = predict(gmo, newdata= df.glm[-itr,], type='response')
    #mod.lst.glm.f = list(gmo)
    preds.glm = as.factor(as.numeric(glm.pred>.5))
    tb.glm = table(preds.glm, y.targeted[-itr])
    acc.glm = sum(diag(tb.glm))/sum(tb.glm)
    glm.acc.rep.tar[k+(r-1)*K] = acc.glm
    sens.targeted[k+(r-1)*K] = tb.glm[2,2]/sum(tb.glm[,2])
    spec.targeted[k+(r-1)*K] = tb.glm[1,1]/sum(tb.glm[,1])
    #auc.m[k] = roc(y.merged[-itr],lasso.pred)$auc
  }
}

mean(glm.acc.rep.tar)
mean(sens.targeted)
mean(spec.targeted)

###Using Random Forests
library(randomForest)
K = 10
n = nrow(X.targeted)
folds = createFolds(df_targeted$Sex, k = 10, list = FALSE)
rf.acc.tar = numeric(K)
top.var.list.rf = list()
sens.rf.tar = numeric(K)
spec.rf.tar =numeric(K)
#auc.m = numeric(K)
#acc.gbm = numeric(K)
set.seed(6041)
for(k in 1:K){
  itr = which(folds!=k)
  vol.out = volcano(X.targeted[itr,], y.targeted[itr])
  #isel = which(vol.out$p.values < 0.1)
  isel = order(vol.out$p.values)[1:10]
  top.var.list.rf[k] = list(isel)
  df.rf = data.frame(y.targeted,X.targeted[,isel])
  rf.mo = randomForest(y.targeted~., data=df.rf[itr,])
  rf.pred = predict(rf.mo, newdata= df.rf[-itr,], type='class')
  #preds.glm = as.factor(as.numeric(glm.pred>.5))
  tb.rf = table(rf.pred, y.targeted[-itr])
  acc.rf = sum(diag(tb.rf))/sum(tb.rf)
  rf.acc.tar[k] = acc.rf
  sens.targeted[k] = tb.rf[2,2]/sum(tb.rf[,2])
  spec.targeted[k] = tb.rf[1,1]/sum(tb.rf[,1])
  #auc.m[k] = roc(y.merged[-itr],lasso.pred)$auc
}

mean(rf.acc.tar)
mean(sens.targeted)
mean(spec.targeted)

###RF on whole data

library(randomForest)
K = 10
n = nrow(X.targeted)
folds = createFolds(df_targeted$Sex, k = 10, list = FALSE)
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
  rf.mo = randomForest(Case_Control~., data=df_targeted[itr,])
  rf.pred = predict(rf.mo, newdata= df_targeted[-itr,], type='class')
  #preds.glm = as.factor(as.numeric(glm.pred>.5))
  tb.rf = table(rf.pred, df_targeted$Case_Control[-itr])
  acc.rf = sum(diag(tb.rf))/sum(tb.rf)
  rf.acc.tar[k] = acc.rf
  sens.rf.tar[k] = tb.rf[2,2]/sum(tb.rf[,2])
  spec.rf.tar[k] = tb.rf[1,1]/sum(tb.rf[,1])
  #auc.m[k] = roc(y.merged[-itr],lasso.pred)$auc
}
mean(rf.acc.tar)
mean(spec.rf.tar)
rf.mo$importance

?t.test
###Separating males and females in the analysis

####Using the MALE records
nrow(df_targeted_whole[df_targeted_whole$Sex==1,])

df_male_targeted = df_targeted_whole[df_targeted_whole$Sex==1,]

dim(df_male_targeted)

X.male.tar = df_male_targeted
X.male.tar$Case_Control = NULL
X.male.tar$Sex= NULL
y.male.tar = df_male_targeted$Case_Control


####Volcano analysis with glm
library(glmnet)
library(pROC)
#xm = as.matrix(X.merged)
K = 10
n = nrow(df_male_targeted)
folds = createFolds(df_male_targeted$Case_Control, k = 10, list = FALSE)
glm.acc.tar = numeric(K)
sens.targeted = numeric(K)
spec.targeted = numeric(K)
top.var.glm.tar = list()
auc.m = numeric(K)
covariates = names(df_male_targeted[,-c(1,2)])[sapply(df_male_targeted[,-c(1,2)], is.numeric)]
#acc.gbm = numeric(K)
set.seed(6041)
for(k in 1:K){
  itr = which(folds!=k)
  
  p.matrix = matrix(NA,nrow=138,ncol=2)
  r=1
  for(c in covariates){
    p = t.test(X.male.tar[itr,c]~y.male.tar[itr])$p.value
    p.matrix[r,1] = c
    p.matrix[r,2] = p
    r = r + 1
  }
  isel = which(p.matrix[,2] < 0.05)
  
  
  #vol.out = volcano(X.targeted_whole[itr,], y.targeted_whole[itr])
  #isel = which(vol.out$p.values < 0.05)
  #isel = order(vol.out$p.values)[1:5]
  #top.var.glm.tar[k] = list(isel)
  df.glm = data.frame(y.male.tar,X.male.tar[,isel])
  gmo = glm(y.male.tar~., data=df.glm, subset = itr, family='binomial')
  glm.pred = predict(gmo, newdata= df.glm[-itr,], type='response')
  preds.glm = as.factor(as.numeric(glm.pred>.5))
  tb.glm = table(preds.glm, y.male.tar[-itr])
  acc.glm = sum(diag(tb.glm))/sum(tb.glm)
  glm.acc.tar[k] = acc.glm
  sens.targeted[k] = tb.glm[2,2]/sum(tb.glm[,2])
  spec.targeted[k] = tb.glm[1,1]/sum(tb.glm[,1])
  
  if (auc.m[1]==0){
    plot(roc(y.male.tar[-itr],glm.pred), col='blue', ylim=c(0,1))
  }
  else{
    plot(roc(y.male.tar[-itr],glm.pred), add=TRUE, col=k, ylim=c(0,1))
  }
  
  auc.m[k] = roc(y.male.tar[-itr],glm.pred)$auc
  #plot(roc(y.male.tar[-itr],glm.pred))
}

mean(glm.acc.tar)
mean(sens.targeted)
mean(auc.m)
mean(spec.targeted)

sd(glm.acc.tar)
boxplot(glm.acc.tar)

####Using the FEMALE records
nrow(df_targeted_whole[df_targeted_whole$Sex==2,])

df_female_targeted = df_targeted_whole[df_targeted_whole$Sex==2,]

dim(df_female_targeted)

X.female.tar = df_female_targeted
X.female.tar$Case_Control = NULL
X.female.tar$Sex= NULL
y.female.tar = df_female_targeted$Case_Control

which(folds==8)
par(mfrow=c(2,5))
table(df_female_targeted$Case_Control[which(folds==8)])
table(df_female_targeted$Case_Control[which(folds!=8)])
table(df_female_targeted$Case_Control[which(folds==10)])
par(pty='s')
####Volcano analysis with glm
library(glmnet)
library(pROC)
#xm = as.matrix(X.merged)
K = 10
n = nrow(df_female_targeted)
folds = createFolds(df_female_targeted$Case_Control, k = 10, list = FALSE)
glm.acc.tar = numeric(K)
sens.targeted = numeric(K)
spec.targeted = numeric(K)
top.var.glm.tar = list()
auc.m = numeric(K)
covariates = names(df_female_targeted[,-c(1,2)])[sapply(df_female_targeted[,-c(1,2)], is.numeric)]
#acc.gbm = numeric(K)
set.seed(6041)
for(k in 1:K){
  itr = which(folds!=k)
  
  p.matrix = matrix(NA,nrow=138,ncol=2)
  r=1
  for(c in covariates){
    p = t.test(X.female.tar[itr,c]~y.female.tar[itr])$p.value
    p.matrix[r,1] = c
    p.matrix[r,2] = p
    r = r + 1
  }
  isel = which(p.matrix[,2] < 0.1)
  
  
  #vol.out = volcano(X.targeted_whole[itr,], y.targeted_whole[itr])
  #isel = which(vol.out$p.values < 0.05)
  #isel = order(vol.out$p.values)[1:5]
  #top.var.glm.tar[k] = list(isel)
  df.glm = data.frame(y.female.tar,X.female.tar[,isel])
  gmo = glm(y.female.tar~., data=df.glm, subset = itr, family='binomial')
  glm.pred = predict(gmo, newdata= df.glm[-itr,], type='response')
  preds.glm = as.factor(as.numeric(glm.pred>.5))
  tb.glm = table(preds.glm, y.female.tar[-itr])
  acc.glm = sum(diag(tb.glm))/sum(tb.glm)
  glm.acc.tar[k] = acc.glm
  sens.targeted[k] = tb.glm[2,2]/sum(tb.glm[,2])
  spec.targeted[k] = tb.glm[1,1]/sum(tb.glm[,1])
  
  if (auc.m[1]==0){
    plot(roc(y.female.tar[-itr],glm.pred), col='blue', ylim=c(0,1))
  }
  else{
    plot(roc(y.female.tar[-itr],glm.pred), add=TRUE, col=k, ylim=c(0,1))
  }
  
  auc.m[k] = roc(y.female.tar[-itr],glm.pred)$auc
  #plot(roc(y.female.tar[-itr],glm.pred), ylim=c(0,1))
}

###Using Random Forests
library(randomForest)
K = 10
n = nrow(df_targeted_whole)
folds = createFolds(df_targeted_whole$Sex, k = 10, list = FALSE)
rf.acc.tar = numeric(K)
top.var.list.rf = list()
sens.rf.tar = numeric(K)
spec.rf.tar =numeric(K)
covariates = names(df_targeted_whole[,-c(1,2)])[sapply(df_targeted_whole[,-c(1,2)], is.numeric)]
auc.m = numeric(K)
#acc.gbm = numeric(K)
set.seed(6041)
for(k in 1:K){
  itr = which(folds!=k)
  
  p.matrix = matrix(NA,nrow=138,ncol=2)
  r=1
  for(c in covariates){
    p = t.test(X.targeted_whole[itr,c]~y.targeted_whole[itr])$p.value
    p.matrix[r,1] = c
    p.matrix[r,2] = p
    r = r + 1
  }
  
  #isel = which(vol.out$p.values < 0.1)
  isel = which(p.matrix[,2] < 0.05)
  #top.var.list.rf[k] = list(isel)
  df.rf = data.frame(y.targeted_whole,X.targeted_whole[,isel])
  rf.mo = randomForest(y.targeted_whole~., data=df.rf[itr,])
  rf.pred = predict(rf.mo, newdata= df.rf[-itr,], type='class')
  rf.prob = predict(rf.mo, newdata= df.rf[-itr,], type='prob')
  #preds.glm = as.factor(as.numeric(glm.pred>.5))
  tb.rf = table(rf.pred, y.targeted_whole[-itr])
  acc.rf = sum(diag(tb.rf))/sum(tb.rf)
  rf.acc.tar[k] = acc.rf
  sens.targeted[k] = tb.rf[2,2]/sum(tb.rf[,2])
  spec.targeted[k] = tb.rf[1,1]/sum(tb.rf[,1])
  auc.m[k] = roc(y.targeted_whole[-itr],rf.prob[,1])$auc
}

str(df_targeted_whole$)
mean(rf.acc.tar)
mean(sens.targeted)
mean(spec.targeted)
mean(auc.m)
sd(auc.m)
1.96* (0.09179246/sqrt(10))
0.5650007-0.05689356
0.5650007+0.05689356
boxplot(auc.m)

?predict.randomForest

####Repeated CV
R = 5
K = 10
n = nrow(X.male.tar)
#folds = cut(1:n, K, labels=FALSE)
folds = createFolds(df_male_targeted$Sex, k = 10, list = FALSE)
glm.acc.male.tar = NULL
top.var.list.glm.rep = list()
sens.tar.male = NULL
spec.tar.male = NULL
#mod.lst.glm.f = list()
#auc.m = numeric(K)
#acc.gbm = numeric(K)
set.seed(6041)

for(r in 1:R){
  is = sample(1:n,n)
  X.male.tar = X.male.tar[is,]
  y.male.tar = y.male.tar[is]
  
  for(k in 1:K){
    itr = which(folds!=k)
    vol.out = volcano(X.male.tar[itr,], y.male.tar[itr])
    #isel = which(vol.out$p.values < 0.1)
    isel = order(vol.out$p.values)[1:5] ##use 3 or 5
    top.var.list.glm.rep[k+(r-1)*K] = list(isel)
    df.glm = data.frame(y.male.tar,X.male.tar[,isel])
    gmo = glm(y.male.tar~., data=df.glm, subset = itr, family='binomial')
    glm.pred = predict(gmo, newdata= df.glm[-itr,], type='response')
    #mod.lst.glm.f = list(gmo)
    preds.glm = as.factor(as.numeric(glm.pred>.5))
    tb.glm = table(preds.glm, y.male.tar[-itr])
    acc.glm = sum(diag(tb.glm))/sum(tb.glm)
    glm.acc.male.tar[k+(r-1)*K] = acc.glm
    sens.tar.male[k+(r-1)*K] = tb.glm[2,2]/sum(tb.glm[,2])
    spec.tar.male[k+(r-1)*K] = tb.glm[1,1]/sum(tb.glm[,1])
    #auc.m[k] = roc(y.merged[-itr],lasso.pred)$auc
  }
}

mean(glm.acc.male.tar)
mean(spec.tar.male)
mean(sens.tar.male)
