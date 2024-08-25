dat_pr_comb = read.csv("C:/Upamanyu/Ireland/Data Science/Classes/Thesis project/proteomics/combined_imputed_data.csv")

dim(dat_pr_comb)

fem_sub = c('1003_S2-D8_1_11189.d',
            '1007_S2-D10_1_11191.d',
            '101_S3-B3_1_10644.d',
            '1029_S2-E11_1_11204.d',
            '103_S3-B4_1_10645.d',
            '1035_S2-F4_1_11209.d',
            '1037_S2-F5_1_11210.d',
            '1039_S2-F6_1_11211.d',
            '1047_S2-F10_1_11215.d',
            '1053_S2-F12_1_11217.d',
            '1063_S2-G6_1_11223.d',
            '1069_S2-G8_1_11225.d',
            '111_S3-B10_1_10651.d',
            '121_S3-C3_1_10656.d',
            '135_S3-C11_1_10664.d',
            '137_S3-C12_1_10665.d',
            '153_S3-D10_1_10675.d',
            '155_S3-D11_1_10676.d',
            '187_S3-F4_1_10693.d',
            '193_S3-F9_1_10698.d',
            '197_S3-F11_1_10700.d',
            '231_S2-B2_1_10737.d',
            '241_S2-B7_1_10742.d',
            '247_S2-B9_1_10744.d',
            '269_S2-C10_1_10757.d',
            '281_S2-D6_1_10765.d',
            '283_S2-D7_1_10766.d',
            '291_S2-D11_1_10770.d',
            '293_S2-E2_1_10773.d',
            '295_S2-E3_1_10774.d',
            '303_S2-E7_1_10778.d',
            '31_S3-B2_1_10602.d',
            '311_S2-E10_1_10781.d',
            '313_S2-E11_1_10782.d',
            '319_S2-F4_1_10787.d',
            '353_S2-G10_1_10708.d',
            '361_S2-H4_1_10714.d',
            '373_S2-H9_1_10719.d',
            '377_S2-H11_1_10721.d',
            '379_S3-A2_1_10805.d',
            '381_S3-A3_1_10806.d',
            '385_S3-A5_1_10808.d',
            '403_S3-B4_1_10819.d',
            '411_S6-A3_1_10823.d',
            '415_S6-A5_1_10825.d',
            '435_S6-B5_1_10837.d',
            '445_S6-B12_1_10844.d',
            '45_S3-B9_1_10611.d',
            '461_S6-C10_1_10854.d',
            '469_S6-D2_1_10858.d',
            '491_S6-E3_1_10871.d',
            '495_S6-E4_1_10872.d',
            '5_S3-A3_1_10589.d',
            '511_S6-F2_1_10882.d',
            '527_S2-H12_1_11241.d',
            '529_S3-A1_1_11242.d',
            '531_S3-A4_1_11245.d',
            '539_S3-A8_1_11249.d',
            '549_S3-A12_1_11253.d',
            '551_S3-B1_1_11254.d',
            '555_S3-B5_1_11258.d',
            '557_S3-B6_1_11259.d',
            '573_S2-A1_1_10918.d',
            '589_S2-A11_1_10928.d',
            '613_S3-C8_1_11273.d',
            '623_S3-D1_1_11278.d',
            '627_S3-D5_1_11282.d',
            '63_S3-C6_1_10620.d',
            '641_S2-D5_1_10978.d',
            '647_S2-D9_1_10982.d',
            '649_S2-D10_1_10983.d',
            '665_S2-E8_1_10993.d',
            '669_S2-E10_1_10995.d',
            '67_S3-C8_1_10625.d',
            '673_S2-E12_1_10997.d',
            '675_S2-F1_1_10998.d',
            '683_S2-F4_1_11001.d',
            '695_S2-F12_1_11009.d',
            '697_S2-G1_1_11010.d',
            '7_S3-A4_1_10590.d',
            '703_S2-G4_1_11013.d',
            '729_S2-H9_1_11030.d',
            '77_S3-A1_1_10630.d',
            '783_S4-B12_1_11061.d',
            '787_S4-C4_1_11065.d',
            '805_S4-D1_1_11074.d',
            '821_S4-D11_1_11084.d',
            '831_S4-E6_1_11091.d',
            '839_S4-E10_1_11095.d',
            '851_S4-F5_1_11102.d',
            '855_S4-F7_1_11104.d',
            '869_S4-G4_1_11113.d',
            '873_S4-G6_1_11115.d',
            '881_S4-G10_1_11119.d',
            '889_S4-H4_1_11125.d',
            '899_S4-H8_1_11129.d',
            '901_S4-H9_1_11130.d',
            '909_S5-A1_1_11134.d',
            '913_S5-A5_1_11138.d',
            '915_S5-A6_1_11139.d',
            '923_S5-A10_1_11143.d',
            '925_S5-A11_1_11144.d',
            '929_S2-A1_1_11146.d',
            '943_S2-A10_1_11155.d',
            '953_S2-B4_1_11161.d',
            '959_S2-B7_1_11164.d',
            '967_S2-B11_1_11168.d',
            '973_S2-C4_1_11173.d',
            '975_S2-C5_1_11174.d',
            '977_S2-C6_1_11175.d',
            '987_S2-C10_1_11179.d',
            '99_S3-B2_1_10643.d',
            '995_S2-D4_1_11185.d')

length(fem_sub)

library(dplyr)

dat_pr_comb$Subject.id


female_pr_dat = dat_pr_comb %>% filter(Subject.id %in% fem_sub)

male_pr_dat = dat_pr_comb %>% filter(!Subject.id %in% fem_sub)

dim(male_pr_dat)

###data exploration
table(male_pr_dat$Autism_NT)

library(DataExplorer)

str(male_pr_dat$Autism_NT)

male_pr_dat$Autism_NT = ifelse(male_pr_dat$Autism_NT == 'Autism', 1, 0)
male_pr_dat$Autism_NT = factor(male_pr_dat$Autism_NT)

dat_pr_comb
dat_pr_comb$Autism_NT = ifelse(dat_pr_comb$Autism_NT == 'Autism', 1, 0)
dat_pr_comb$Autism_NT = factor(dat_pr_comb$Autism_NT)

plot_histogram(male_pr_dat[,-c(1,2)])
plot_histogram(dat_pr_comb[,c(3:50)])

plot_boxplot(male_pr_dat[,-c(1)], by= "Autism_NT")

boxplot(male_pr_dat[,'P31949']~male_pr_dat$Autism_NT)

mean(dat_pr_comb$O00299)

test_df = data.frame(scale(dat_pr_comb[,-c(1,2)], center = TRUE))
plot_histogram(test_df[,c(3:50)])
mean(test_df$O00299)

dat_pr_comb$Subject.id = NULL
library(xtable)
dat_significant_latex = read.csv("C:/Upamanyu/Ireland/Data Science/Classes/Thesis project/proteomics/significant_features_proteomics.csv")

latex_table <- xtable(dat_significant_latex, digits = c(0,0,3,3,3,3), caption = "Sample Dataframe", label = "sample_dataframe")

?xtable
print(latex_table, caption = "Sample Dataframe", label = "sample_dataframe")

boxplot(test_df[,c(2:21)],col = "green", cex.lab = 1.5, las=2)

###RF on whole data
1.6286E-4


library(randomForest)
library(caret)
K = 10
n = nrow(male_pr_dat)
folds = createFolds(male_pr_dat$Autism_NT, k = 10, list = FALSE)
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
  rf.mo = randomForest(Autism_NT~., data=male_pr_dat[itr,-c(1)], ntree =700)
  rf.pred = predict(rf.mo, newdata= male_pr_dat[-itr,-c(1)], type='class')
  #preds.glm = as.factor(as.numeric(glm.pred>.5))
  tb.rf = table(rf.pred, male_pr_dat$Autism_NT[-itr])
  acc.rf = sum(diag(tb.rf))/sum(tb.rf)
  rf.acc.tar[k] = acc.rf
  sens.rf.tar[k] = tb.rf[2,2]/sum(tb.rf[,2])
  spec.rf.tar[k] = tb.rf[1,1]/sum(tb.rf[,1])
  #auc.m[k] = roc(y.merged[-itr],lasso.pred)$auc
}

mean(rf.acc.tar)
mean(sens.rf.tar)
mean(spec.rf.tar)

##RF on whole data

K = 10
n = nrow(pr_data)
folds = createFolds(dat_pr_comb$Autism_NT, k = K, list = FALSE)
rf.acc.tar = numeric(K)
#top.var.list.rf = list()
sens.rf.tar = numeric(K)
spec.rf.tar =numeric(K)
auc.rf = numeric(K)
#acc.gbm = numeric(K)
set.seed(6041)
for(k in 1:K){
  itr = which(folds!=k)
  #vol.out = volcano(X.targeted[itr,], y.targeted[itr])
  #isel = which(vol.out$p.values < 0.1)
  #isel = order(vol.out$p.values)[1:10]
  #top.var.list.rf[k] = list(isel)
  #df.rf = data.frame(y.targeted,X.targeted[,isel])
  rf.mo = randomForest(Autism_NT~., data=dat_pr_comb[itr,])
  rf.pred = predict(rf.mo, newdata= dat_pr_comb[-itr,], type='class')
  rf.prob = predict(rf.mo, newdata= dat_pr_comb[-itr,], type='prob')
  #preds.glm = as.factor(as.numeric(glm.pred>.5))
  tb.rf = table(rf.pred, dat_pr_comb$Autism_NT[-itr])
  acc.rf = sum(diag(tb.rf))/sum(tb.rf)
  rf.acc.tar[k] = acc.rf
  sens.rf.tar[k] = tb.rf[2,2]/sum(tb.rf[,2])
  spec.rf.tar[k] = tb.rf[1,1]/sum(tb.rf[,1])
  
  if (auc.rf[1]==0){
    plot(roc(dat_pr_comb$Autism_NT[-itr],rf.prob[,1]), col='blue',  cex.lab = 1.5, ylim=c(0,1))
  }
  else{
    plot(roc(dat_pr_comb$Autism_NT[-itr],rf.prob[,1]), add=TRUE, col=k,  cex.lab = 1.5, ylim=c(0,1))
  }
  
  auc.rf[k] = roc(dat_pr_comb$Autism_NT[-itr],rf.prob[,1])$auc
}
mean(rf.acc.tar) ##0.47
mean(sens.rf.tar) ##0.46
mean(spec.rf.tar) ##0.47
mean(auc.rf) ##0.57


###GLM whole data

K = 10
n = nrow(pr_data)
folds = createFolds(dat_pr_comb$Autism_NT, k = K, list = FALSE)
glm.acc.tar = numeric(K)
sens.prot = numeric(K)
spec.prot = numeric(K)
top.var.glm.tar = list()
auc.m = numeric(K)
#covariates = names(X.merged)[sapply(X.merged, is.numeric)]
#acc.gbm = numeric(K)
set.seed(6041)
for(k in 1:K){
  itr = which(folds!=k)
  #df.glm = data.frame(y.merged,X.merged[,isel])
  gmo = glm(Autism_NT~., data=dat_pr_comb, subset = itr, family='binomial')
  glm.pred = predict(gmo, newdata= dat_pr_comb[-itr,], type='response')
  preds.glm = as.factor(as.numeric(glm.pred>.5))
  tb.glm = table(preds.glm, dat_pr_comb$Autism_NT[-itr])
  acc.glm = sum(diag(tb.glm))/sum(tb.glm)
  glm.acc.tar[k] = acc.glm
  sens.prot[k] = tb.glm[2,2]/sum(tb.glm[,2])
  spec.prot[k] = tb.glm[1,1]/sum(tb.glm[,1])
  if (auc.m[1]==0){
    plot(roc(dat_pr_comb$Autism_NT[-itr],glm.pred), col='blue', cex.lab = 1.5, ylim=c(0,1))
  }
  else{
    plot(roc(dat_pr_comb$Autism_NT[-itr],glm.pred), add=TRUE, col=k, cex.lab = 1.5, ylim=c(0,1))
  }
  auc.m[k] = roc(dat_pr_comb$Autism_NT[-itr],glm.pred)$auc
  #plot(roc(pr_data$case_control[-itr],glm.pred))
}

mean(glm.acc.tar) #0.51
mean(sens.prot) #0.48
mean(spec.prot) #0.54
mean(auc.m) #0.54


##Lasso Feature Selection

###Lasso
library(glmnet)
set.seed(4061) # for reproducibility
n = nrow(male_pr_dat)
xm = as.matrix(male_pr_dat[,-c(1,2)])
K = 10
folds = createFolds(male_pr_dat$Autism_NT, k = 10, list = FALSE)
sel = matrix(0, nrow=K, ncol=920)
colnames(sel) = names(male_pr_dat[,-c(1,2)])
rf.acc.tar = numeric(K)
sens.rf.tar = numeric(K)
spec.rf.tar = numeric(K)
for(k in 1:K){
  itr = which(folds!=k)
  lasso.cv = cv.glmnet(xm[itr,], male_pr_dat$Autism_NT[itr], family='binomial')
  lasso = glmnet(xm[itr,], male_pr_dat$Autism_NT[itr], lambda=lasso.cv$lambda.min,  family='binomial')
  isel = which(coef(lasso)[-1] != 0)
  sel[k,isel] = 1
  rf.mo = randomForest(Autism_NT~., data=male_pr_dat[itr,c(2,(isel+2))])
  rf.pred = predict(rf.mo, newdata= male_pr_dat[-itr,c(2,(isel+2))], type='class')
  tb.rf = table(rf.pred, male_pr_dat$Autism_NT[-itr])
  acc.rf = sum(diag(tb.rf))/sum(tb.rf)
  rf.acc.tar[k] = acc.rf
  sens.rf.tar[k] = tb.rf[2,2]/sum(tb.rf[,2])
  spec.rf.tar[k] = tb.rf[1,1]/sum(tb.rf[,1])
}

###dataframe with significant features

dat_sig_male = male_pr_dat[,c(1:43)]

dim(dat_sig_male)

###RF with significant features


library(randomForest)
library(caret)
K = 10
n = nrow(pr_data)
folds = createFolds(pr_data$case_control, k = K, list = FALSE)
rf.acc.tar = numeric(K)
#top.var.list.rf = list()
sens.rf.tar = numeric(K)
spec.rf.tar =numeric(K)
auc.rf = numeric(K)
#acc.gbm = numeric(K)
set.seed(6041)
for(k in 1:K){
  itr = which(folds!=k)
  #vol.out = volcano(X.targeted[itr,], y.targeted[itr])
  #isel = which(vol.out$p.values < 0.1)
  #isel = order(vol.out$p.values)[1:10]
  #top.var.list.rf[k] = list(isel)
  #df.rf = data.frame(y.targeted,X.targeted[,isel])
  rf.mo = randomForest(case_control~., data=pr_data[itr,])
  rf.pred = predict(rf.mo, newdata= pr_data[-itr,], type='class')
  rf.prob = predict(rf.mo, newdata= pr_data[-itr,], type='prob')
  #preds.glm = as.factor(as.numeric(glm.pred>.5))
  tb.rf = table(rf.pred, pr_data$case_control[-itr])
  acc.rf = sum(diag(tb.rf))/sum(tb.rf)
  rf.acc.tar[k] = acc.rf
  sens.rf.tar[k] = tb.rf[2,2]/sum(tb.rf[,2])
  spec.rf.tar[k] = tb.rf[1,1]/sum(tb.rf[,1])
  
  if (auc.rf[1]==0){
    plot(roc(pr_data$case_control[-itr],rf.prob[,1]), col='blue', cex.lab = 1.5, ylim=c(0,1))
  }
  else{
    plot(roc(pr_data$case_control[-itr],rf.prob[,1]), add=TRUE, col=k, cex.lab = 1.5, ylim=c(0,1))
  }
  
  auc.rf[k] = roc(pr_data$case_control[-itr],rf.prob[,1])$auc
}
mean(rf.acc.tar) ##0.56
mean(sens.rf.tar) ##0.53
mean(spec.rf.tar) ##0.58
mean(auc.rf) ##0.60


###CV WilcoxTest
K = 10
n = nrow(male_pr_dat)
folds = createFolds(male_pr_dat$Autism_NT, k = 10, list = FALSE)
sel.wilcox = matrix(0, nrow=K, ncol=ncol(male_pr_dat[,-c(1,2)]))
colnames(sel.wilcox) = names(male_pr_dat[,-c(1,2)])
covariates = names(male_pr_dat[,-c(1,2)])[sapply(male_pr_dat[,-c(1,2)], is.numeric)]
set.seed(6041)
for(k in 1:K){
  itr = which(folds!=k)
  p.matrix = matrix(NA,nrow=920,ncol=2)
  r=1
  for(c in covariates){
    p = wilcox.test(male_pr_dat[itr,c]~male_pr_dat$Autism_NT[itr])$p.value
    p.matrix[r,1] = c
    p.matrix[r,2] = p
    r = r + 1
  }
  ind = which(p.matrix[,2] < 0.05)
  #isel = which(vol.out$p.values < 0.1)
  sel.wilcox[k,ind] = 1
}
var.sel.wilcox = apply(sel.wilcox,2,mean)*100
which(var.sel.wilcox >= 100)

o = t.test(log(xj)~y)
ps[j] = o$p.value

###CV student t test
K = 10
n = nrow(male_pr_dat)
folds = createFolds(male_pr_dat$Autism_NT, k = 10, list = FALSE)
sel.t = matrix(0, nrow=K, ncol=ncol(male_pr_dat[,-c(1,2)]))
colnames(sel.t) = names(male_pr_dat[,-c(1,2)])
covariates = names(male_pr_dat[,-c(1,2)])[sapply(male_pr_dat[,-c(1,2)], is.numeric)]
set.seed(6041)
for(k in 1:K){
  itr = which(folds!=k)
  p.matrix = matrix(NA,nrow=920,ncol=2)
  r=1
  for(c in covariates){
    p = t.test(male_pr_dat[itr,c]~male_pr_dat$Autism_NT[itr])$p.value
    p.matrix[r,1] = c
    p.matrix[r,2] = p
    r = r + 1
  }
  ind = which(p.matrix[,2] < 0.05)
  #isel = which(vol.out$p.values < 0.1)
  sel.t[k,ind] = 1
}
var.sel.t = apply(sel.t,2,mean)*100
which(var.sel.t == 100)


p.matrix = matrix(NA,nrow=920,ncol=2)
r=1
for(c in covariates){
  p = t.test(dat_pr_comb[,c]~dat_pr_comb$Autism_NT)$p.value
  p.matrix[r,1] = c
  p.matrix[r,2] = p
  r = r + 1
}
length(which(p.matrix[,2] < 0.05))
ind = order(p.matrix[which(p.matrix[,2] < 0.05),2])

new.matrix = p.matrix[which(p.matrix[,2] < 0.05),]

sig_features=new.matrix[ind,1]

dat_pr_comb[,sig_features]

###Using Random Forests
library(randomForest)
library(caret)
K = 10
n = nrow(X)
folds = createFolds(male_pr_dat$Autism_NT, k = 10, list = FALSE)
rf.acc.m = numeric(K)
top.var.list.rf = list()
sens.rf = numeric(K)
spec.rf = numeric(K)

sel.t = matrix(0, nrow=K, ncol=ncol(male_pr_dat[,-c(1,2)]))
colnames(sel.t) = names(male_pr_dat[,-c(1,2)])
covariates = names(male_pr_dat[,-c(1,2)])[sapply(male_pr_dat[,-c(1,2)], is.numeric)]
#auc.m = numeric(K)
#acc.gbm = numeric(K)
set.seed(6041)
for(k in 1:K){
  itr = which(folds!=k)
  
  p.matrix = matrix(NA,nrow=920,ncol=2)
  r=1
  for(c in covariates){
    p = t.test(X[itr,c]~y[itr])$p.value
    p.matrix[r,1] = c
    p.matrix[r,2] = p
    r = r + 1
  }
  isel = which(p.matrix[,2] < 0.05)
  
  #vol.out = volcano(X.male[itr,], y.male[itr])
  #isel = which(vol.out$p.values < 0.1)
  #isel = order(vol.out$p.values)[1:10]
  #top.var.list.rf[k] = list(isel)
  df.rf = data.frame(y,X[,isel])
  rf.mo = randomForest(y~., data=df.rf[itr,])
  rf.pred = predict(rf.mo, newdata= df.rf[-itr,], type='class')
  #preds.glm = as.factor(as.numeric(glm.pred>.5))
  tb.rf = table(rf.pred, y[-itr])
  acc.rf = sum(diag(tb.rf))/sum(tb.rf)
  rf.acc.m[k] = acc.rf
  sens.rf[k] = tb.rf[2,2]/sum(tb.rf[,2])
  spec.rf[k] = tb.rf[1,1]/sum(tb.rf[,1])
  #auc.m[k] = roc(y.merged[-itr],lasso.pred)$auc
}
mean(rf.acc.m)
mean(sens.rf)
mean(spec.rf)


###Using Random Forests on whole data

X.full =dat_pr_comb[,-c(1,2)]
y.full = dat_pr_comb$Autism_NT
library(randomForest)
library(caret)
K = 10
n = nrow(dat_pr_comb)
folds = createFolds(dat_pr_comb$Autism_NT, k = 10, list = FALSE)
rf.acc.m = numeric(K)
top.var.list.rf = list()
sens.rf = numeric(K)
spec.rf = numeric(K)

sel.t = matrix(0, nrow=K, ncol=ncol(dat_pr_comb[,-c(1,2)]))
colnames(sel.t) = names(dat_pr_comb[,-c(1,2)])
covariates = names(dat_pr_comb[,-c(1,2)])[sapply(dat_pr_comb[,-c(1,2)], is.numeric)]
#auc.m = numeric(K)
#acc.gbm = numeric(K)
set.seed(6041)
for(k in 1:K){
  itr = which(folds!=k)
  
  p.matrix = matrix(NA,nrow=920,ncol=2)
  r=1
  for(c in covariates){
    p = t.test(X.full[itr,c]~y.full[itr])$p.value
    p.matrix[r,1] = c
    p.matrix[r,2] = p
    r = r + 1
  }
  isel = which(p.matrix[,2] < 0.05)
  
  #vol.out = volcano(X.male[itr,], y.male[itr])
  #isel = which(vol.out$p.values < 0.1)
  #isel = order(vol.out$p.values)[1:10]
  #top.var.list.rf[k] = list(isel)
  df.rf = data.frame(y.full,X.full[,isel])
  rf.mo = randomForest(y.full~., data=df.rf[itr,])
  rf.pred = predict(rf.mo, newdata= df.rf[-itr,], type='class')
  #preds.glm = as.factor(as.numeric(glm.pred>.5))
  tb.rf = table(rf.pred, y.full[-itr])
  acc.rf = sum(diag(tb.rf))/sum(tb.rf)
  rf.acc.m[k] = acc.rf
  sens.rf[k] = tb.rf[2,2]/sum(tb.rf[,2])
  spec.rf[k] = tb.rf[1,1]/sum(tb.rf[,1])
  #auc.m[k] = roc(y.merged[-itr],lasso.pred)$auc
}
mean(rf.acc.m)
mean(sens.rf)
mean(spec.rf)

##simple glm

mod.fwd = glm(Autism_NT~P05067 + P05121 + P08833 + P09486 + P12277 + P13521 + Q13201 + Q14766 + P01137 + P40197 + Q01995 + Q96HR3, data=dat_pr_comb, family = 'binomial')
pred =predict(mod.fwd, type="response")
preds = as.factor(as.numeric(pred>.5))

tb.fwd = table(preds, dat_pr_comb$Autism_NT)
acc.fwd = sum(diag(tb.fwd))/sum(tb.fwd)

pr_data = data.frame(case_control = dat_pr_comb$Autism_NT, x.pr)
names(pr_data)
x.pr = dat_pr_comb[,sig_features]

mod.fwd = glm(case_control~., data=pr_data, family = 'binomial')
pred =predict(mod.fwd, type="response")
preds = as.factor(as.numeric(pred>.5))
tb.fwd = table(preds, dat_pr_comb$Autism_NT)
acc.fwd = sum(diag(tb.fwd))/sum(tb.fwd)


####CV glm
par(pty='s')
library(glmnet)
library(pROC)
#xm = as.matrix(X.merged)
K = 10
n = nrow(pr_data)
folds = createFolds(pr_data$case_control, k = K, list = FALSE)
glm.acc.tar = numeric(K)
sens.prot = numeric(K)
spec.prot = numeric(K)
top.var.glm.tar = list()
auc.m = numeric(K)
#covariates = names(X.merged)[sapply(X.merged, is.numeric)]
#acc.gbm = numeric(K)
set.seed(6041)
for(k in 1:K){
  itr = which(folds!=k)
  #df.glm = data.frame(y.merged,X.merged[,isel])
  gmo = glm(case_control~., data=pr_data, subset = itr, family='binomial')
  glm.pred = predict(gmo, newdata= pr_data[-itr,], type='response')
  preds.glm = as.factor(as.numeric(glm.pred>.5))
  tb.glm = table(preds.glm, pr_data$case_control[-itr])
  acc.glm = sum(diag(tb.glm))/sum(tb.glm)
  glm.acc.tar[k] = acc.glm
  sens.prot[k] = tb.glm[2,2]/sum(tb.glm[,2])
  spec.prot[k] = tb.glm[1,1]/sum(tb.glm[,1])
  if (auc.m[1]==0){
    plot(roc(pr_data$case_control[-itr],glm.pred), col='blue', cex.lab= 1.5, ylim=c(0,1))
  }
  else{
    plot(roc(pr_data$case_control[-itr],glm.pred), add=TRUE, col=k, cex.lab= 1.5, ylim=c(0,1))
  }
  auc.m[k] = roc(pr_data$case_control[-itr],glm.pred)$auc
  #plot(roc(pr_data$case_control[-itr],glm.pred))
}
mean(glm.acc.tar) #0.5666909
mean(sens.prot) #0.5521368
mean(spec.prot) #0.5810541
mean(auc.m) #0.5976277


###
pca.scaled = prcomp(male_pr_dat[,-c(1,2)],scale=TRUE)

summary(pca.scaled)[,'PC1']
length(pca.scaled$rotation[,1])

names(summary(pca.scaled))
dim(summary(pca.scaled))
plot(pca.scaled)

PC1 = pca.scaled$x[,'PC1']
PC2 = pca.scaled$x[,'PC2']
PC3 = pca.scaled$x[,'PC3']

df.pca = data.frame(Autism_NT=male_pr_dat$Autism_NT, pc1 = PC1, pc2 = PC2, pc3 = PC3)

set.seed(6041)
K = 10
n = nrow(df.pca)
folds = createFolds(df.pca$Autism_NT, k = 10, list = FALSE)
accuracy.glm = numeric(K)
sens.glm = numeric(K)
spec.glm = numeric(K)
#sel.vol = matrix(0, nrow=K, ncol=ncol(X.merged))
#colnames(sel.vol) = names(X.merged)
for(k in 1:K){
  itr = which(folds!=k)
  glm.mod = glm(Autism_NT~., data = df.pca[itr,], family = 'binomial')
  pred = predict(glm.mod, newdata =df.pca[-itr,])
  pred.class = as.factor(as.numeric(pred>.5))
  
  cms = confusionMatrix(pred.class,df.pca$Autism_NT[-itr])
  accuracy.glm[k] = cms$overall[1]
  tb.vol = cms$table
  #acc.vol = sum(diag(tb.vol))/sum(tb.vol)
  sens.glm[k] = tb.vol[2,2]/sum(tb.vol[,2])
  spec.glm[k] = tb.vol[1,1]/sum(tb.vol[,1])
}
mean(accuracy.glm)
mean(spec.glm)
mean(sens.glm)


##PLS-DA

library(mixOmics)

X = male_pr_dat[,-c(1,2)]
y = male_pr_dat$Autism_NT

dat_pr_comb

X = dat_pr_comb[,-c(1,2)]
y = dat_pr_comb$Autism_NT

plsda_result <- plsda(X, y, ncomp = 2)


plotIndiv(plsda_result, comp = 1:2, group = y, ellipse = TRUE, legend = TRUE)
plotVar(plsda_result, comp = 1:2)

predict_Y <- predict(plsda_result, X)$class
confusion_matrix <- table(predicted = predict_Y$max.dist[,'comp2'], actual = y)
print(confusion_matrix)
predict_Y$centroids.dist[,'comp1']

set.seed(123)
cv <- perf(plsda_result, validation = "Mfold", folds = 10, progressBar = TRUE, nrepeat = 10)
print(cv$error.rate)

plot(cv, sd=TRUE, xlab = "Number of Components", ylab = "Error Rate")
