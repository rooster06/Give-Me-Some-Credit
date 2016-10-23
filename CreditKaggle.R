#---------------------------------------------------------------
# Give Me Some Credit-Kaggle
# Author: Prabhat R.
#---------------------------------------------------------------

rm(list = ls())

# install packages ---------------------------------------------- 
#install.packages('dplyr')
#install.packages('magrittr')
#install.packages('ggplot2')
#install.packages('Amelia')
#install.packages('ROSE')
#install.packages('ROCR')
#install.packages('cvAUC')
#install.packages('randomForest')
#install.packages('effects')
#install.packages("rgl")
#install.packages('foreach')
#install.packages('pROC')

library(dplyr)
library(magrittr)
library(ggplot2)
library(lattice)
library(Amelia)
library(caret)
library(ROSE)
library(ROCR)
library(cvAUC)
library(xgboost)
library(DiagrammeR)

# load the data --------------------------------------------------
trainDat<-read.csv('training.csv',header = TRUE)
# head(trainDat)

testDat<-read.csv('test.csv',header = TRUE)
# head(testDat)

# drop the row id column 
trainDat<-trainDat[,-1]
testDat<-testDat[,-1]


# Missing Data ---------------------------------------------------------
jpeg(file="missing2.jpeg")
missmap(trainDat, main = "Missing values vs observed")
dev.off()

# Data distributions & imputation --------------------------------------

# 1. age imputation ----------------------------------------------------
ggplot(data = trainDat,aes(age))+geom_histogram(col='red',fill='red')+
  labs(title='Histogram of Age')
qqnorm(trainDat$age, main = "Age")
qqline(trainDat$age)
sum(is.na(trainDat$age))
summary(trainDat$age)

# 2. RevolvingUtilizationOfUnsecuredLines imputation -----------------------
hist(trainDat$RevolvingUtilizationOfUnsecuredLines,col='steelblue')
summary(trainDat$RevolvingUtilizationOfUnsecuredLines)
boxplot(trainDat$RevolvingUtilizationOfUnsecuredLines)
# ratio should be between 0 and 1. 

# number of missing values
sum(is.na(trainDat$RevolvingUtilizationOfUnsecuredLines))

# outliers
sum(trainDat$RevolvingUtilizationOfUnsecuredLines>1)

# not a normal distribution 
qqnorm(trainDat$RevolvingUtilizationOfUnsecuredLines[trainDat$RevolvingUtilizationOfUnsecuredLines<=1])
qqline(trainDat$RevolvingUtilizationOfUnsecuredLines)

# replace the abnormal values with median

trainDat$RevolvingUtilizationOfUnsecuredLines[trainDat$RevolvingUtilizationOfUnsecuredLines>1]=0.15

ggplot(data = trainDat,aes(RevolvingUtilizationOfUnsecuredLines))+geom_histogram(col='red',fill='red')+
  labs(title='Histogram of RevolvingUtilizationOfUnsecuredLines')

boxplot(trainDat$RevolvingUtilizationOfUnsecuredLines)

# 3. NumberOfTime30.59DaysPastDueNotWorse imputations -------------------------
summary(trainDat$NumberOfTime30.59DaysPastDueNotWorse)

# number of missing values 
sum(is.na(trainDat$NumberOfTime30.59DaysPastDueNotWorse))

# distribution 
ggplot(data = trainDat,aes(NumberOfTime30.59DaysPastDueNotWorse))+geom_histogram(col='black',fill='red')+
  labs(title='Histogram Num of time past 30-59days')

table(trainDat$NumberOfTime30.59DaysPastDueNotWorse)
# shows a few people with 96 and 98 times which is quite absurd

# mean.impu<- mean(trainDat$NumberOfTime30.59DaysPastDueNotWorse[trainDat$NumberOfTime30.59DaysPastDueNotWorse<96])

trainDat$NumberOfTime30.59DaysPastDueNotWorse[trainDat$NumberOfTime30.59DaysPastDueNotWorse>=96]<-0

ggplot(data = trainDat,aes(NumberOfTime30.59DaysPastDueNotWorse))+geom_histogram(col='black',fill='red')+
  labs(title='Histogram Num of time past 30-59days')

# 4. DebtRatio imputations ---------------------------------------------------------

summary(trainDat$DebtRatio)

boxplot(trainDat$DebtRatio)
sum(is.na(trainDat$DebtRatio))

# debt ratio greater than 100k seems weird 
# lets remove them 
trainDat<-trainDat[-which(trainDat$DebtRatio>100000),]
summary(trainDat$DebtRatio)

boxplot(trainDat$DebtRatio)

# 5. MonthlyIncome imputations ---------------------------------------------
sum(is.na(trainDat$MonthlyIncome))

summary(trainDat$MonthlyIncome)

ggplot(data = trainDat,aes(MonthlyIncome))+geom_histogram(col='black',fill='red')+
  labs(title='Histogram of MonthlyIncome')
# lets do a median imputation for the monthly income
trainDat$MonthlyIncome[is.na(trainDat$MonthlyIncome)]<-median(trainDat$MonthlyIncome,na.rm = TRUE)
boxplot(trainDat$MonthlyIncome)



# remove the richie rich, itll be a levrage point and we dont need 
# that for the model fitting
trainDat<-trainDat[-which(trainDat$MonthlyIncome>300000),]
trainDat
ggplot(data = trainDat,aes(MonthlyIncome))+geom_histogram(col='black',fill='red')+
  labs(title='Histogram of MonthlyIncome')



# 6. NumberOfOpenCreditLinesAndLoans imputations -----------------------------------------

# no missing values! yay!
sum(is.na(trainDat$NumberOfOpenCreditLinesAndLoans))

summary(trainDat$NumberOfOpenCreditLinesAndLoans)

ggplot(data = trainDat,aes(NumberOfOpenCreditLinesAndLoans))+geom_histogram(col='red',fill='green')+
  labs(title='Histogram of num of open credit lines')

boxplot(trainDat$NumberOfOpenCreditLinesAndLoans)
# seems like this variable needs no imputations

# 7. NumberOfTimes90DaysLate imputations -------------------------------------

# no missing values! yay!
sum(is.na(trainDat$NumberOfTimes90DaysLate))
summary(trainDat$NumberOfTimes90DaysLate)

ggplot(data = trainDat,aes(NumberOfTimes90DaysLate))+geom_histogram(col='red',fill='green')+
  labs(title='Histogram of num of open credit lines')

boxplot(trainDat$NumberOfTimes90DaysLate)

# trainDat$SeriousDlqin2yrs[trainDat$NumberOfTimes90DaysLate,1]

table(trainDat$NumberOfTimes90DaysLate)
# NO ONE can default over 90 so many times thats silly must be some typo
# impute zero coz median seems to be zero anyway and the mean wouldnt make sense

trainDat$NumberOfTimes90DaysLate[trainDat$NumberOfTimes90DaysLate>90]<-0

ggplot(data = trainDat,aes(NumberOfTimes90DaysLate))+geom_histogram(col='red',fill='green')+
  labs(title='Histogram of num of open credit lines')

# 8. NumberRealEstateLoansOrLines imputations ------------------------------
summary(trainDat$NumberRealEstateLoansOrLines)
sum(is.na(trainDat$NumberRealEstateLoansOrLines))
table(trainDat$NumberRealEstateLoansOrLines)
trainDat<-trainDat[-(which(trainDat$NumberRealEstateLoansOrLines==54)),]

ggplot(data = trainDat,aes(NumberRealEstateLoansOrLines))+geom_histogram(col='red',fill='green')+
  labs(title='Histogram of num of real estate loans')

# 9. NumberOfTime60.89DaysPastDueNotWorse imputations ----------------------

sum(is.na(trainDat$NumberOfTime60.89DaysPastDueNotWorse))

ggplot(data = trainDat,aes(NumberOfTime60.89DaysPastDueNotWorse))+geom_histogram(col='red',fill='green')+
  labs(title='Histogram of num of times 60-89 days')
table(trainDat$NumberOfTime60.89DaysPastDueNotWorse)

summary(trainDat$NumberOfTime60.89DaysPastDueNotWorse)
# same as above push the absurd values to zero 
trainDat$NumberOfTime60.89DaysPastDueNotWorse[trainDat$NumberOfTime60.89DaysPastDueNotWorse>90]<-0

ggplot(data = trainDat,aes(NumberOfTime60.89DaysPastDueNotWorse))+geom_histogram(col='red',fill='green')+
  labs(title='Histogram of num of times 60-89 days')

# 10. NumberOfDependents imputations -----------------------------------

# some missing values
sum(is.na(trainDat$NumberOfDependents))

summary(trainDat$NumberOfDependents)

# lets just make them 0 the missing vals

trainDat$NumberOfDependents[is.na(trainDat$NumberOfDependents)]<-0

# Data Balance testing -------------------------------------------------- 

prop.table(table(trainDat$SeriousDlqin2yrs))
barplot(prop.table(table(trainDat$SeriousDlqin2yrs)),col = 'steelblue')
# serious data imbalance
# addressing the data imbalance by downsampling the 0 class in serious
# dlwin2yrs 

sum(trainDat$SeriousDlqin2yrs==1)
newtrainDat<-trainDat[trainDat$SeriousDlqin2yrs==1,]
DownsampleDat<-trainDat[trainDat$SeriousDlqin2yrs==0,]
downsam<-sample(1:139948,11000)

nDat<-rbind(newtrainDat,DownsampleDat[downsam,])
nDat<-nDat[sample(nrow(nDat)),]
rownames(nDat)<-NULL

set.seed(36)
trainIndex <- createDataPartition(nDat$SeriousDlqin2yrs, p = .8, 
                                  list = FALSE, 
                                  times = 1)
ntrain<-nDat[trainIndex,]
ntest<-nDat[-trainIndex,]

# testdata imputations ------------------------------------------------

testDat$MonthlyIncome<- median(trainDat$MonthlyIncome,na.rm = TRUE)
testDat$NumberOfDependents[is.na(testDat$NumberOfDependents)] <- 0

ntrain.gbm<-ntrain
ntrain$SeriousDlqin2yrs<-as.factor(ntrain$SeriousDlqin2yrs)

# Data viz ------------------------------------------------------------
library(reshape2)

# melting the data frame
feature.names<-names(nDat)[-1]

vizDat<- melt(nDat,id.vars = 'SeriousDlqin2yrs'
              ,measure.vars = feature.names, variable.name = "Feature"
              ,value.name = "Value")

# conditional box plots for each feature on the response variable
p <- ggplot(data = vizDat, aes(x=Feature, y=Value)) + 
             geom_boxplot(aes(fill=SeriousDlqin2yrs))
p <- p + facet_wrap( ~ Feature, scales="free")
p + ggtitle("Conditional Distributions of each variable")

# lets look at the data in the first two principal components

nDat.s<- scale(nDat[,-1],center=TRUE, scale=TRUE)
Dat.pc<-prcomp(nDat.s)
summary(Dat.pc)
# pr1 and pr2 only explain 36% of the variance but still 
# lets look at the distribution of for fun
plot(Dat.pc$x[,1:2],col=as.factor(nDat[,1]))

# nope! no clear distinction

# lets try a 3d scatter plot since the first three prcomps explain
# 48.64% of the variance
library(car)
library(rgl)
scatter3d(x = Dat.pc$x[,1], y = Dat.pc$x[,2], z = Dat.pc$x[,3]
          , groups = as.factor(nDat[,1]),
          grid = FALSE, surface = FALSE)


# Viz of varaibles

# scatter plots 
source("~/Desktop/linearStats/trainAcc/SPM_Panel.R")
uva.pairs(cbind(ntrain[,-1],ntrain[,1]))
# lets look at pair plots after log trainsforming all the predictor
LnDat<-log(ntrain[,-1]+0.001)
LnDat<-cbind(LnDat,ntrain[,1])
uva.pairs(LnDat)
# log transform dint really help! 




################
# lets build some models!!
################



# logistic regression -------------------------------------------------
library(effects)

# Main effect models 
logi.model<-glm(SeriousDlqin2yrs~.,data = ntrain,family = binomial)
summary(logi.model)
plot(allEffects(logi.model))

# ROC and AUROC
pred.logi.model<-predict(logi.model,ntest[,-1],type='response')
pr <- prediction(pred.logi.model, ntest$SeriousDlqin2yrs)
prf0 <- performance(pr, measure = "tpr", x.measure = "fpr")

# full two interaction model
inter.logi<-glm(SeriousDlqin2yrs~(.)^2,data = ntrain,family = binomial)
summary(inter.logi)

anova(logi.model,inter.logi,test = 'Chisq')
# avnova tells us that we need to use the interaction terms, and the pvalue
# is very very significant

# from the correlation and with intution some of the interactions that seem 
# important are as follows:
# RevolvingUtilizationOfUnsecuredLines:NumberOfTime30.59DaysPastDueNotWorse ***
# RevolvingUtilizationOfUnsecuredLines:MonthlyIncome                        *  
# RevolvingUtilizationOfUnsecuredLines:NumberOfTimes90DaysLate              ***
# NumberOfTime30.59DaysPastDueNotWorse:NumberOfTime60.89DaysPastDueNotWorse ***
# NumberOfOpenCreditLinesAndLoans:NumberRealEstateLoansOrLines              ***
# NumberOfTimes90DaysLate:NumberOfTime60.89DaysPastDueNotWorse              ***
# NumberOfOpenCreditLinesAndLoans:NumberRealEstateLoansOrLines              ***
# NumberOfTimes90DaysLate:NumberOfTime60.89DaysPastDueNotWorse              ***
# age:number of dependents
# DebtRatio:NumberRealEstateLoansOrLines                                    ** 
 
# step-wise selection 
step.logi<-step(inter.logi, trace = 0)
summary(step.logi)
# thats a lot of interactions selected by the step regression
anova(step.logi,inter.logi,test = 'Chisq')
# step selected model is better than the complete interaction model

# ROC and AUROC FOR THE STEP-WISE MODEL
pred.logi.model<-predict(step.logi,ntest[,-1],type='response')
out<-cvAUC(step.logi,ntest$SeriousDlqin2yrs)
plot(out$perf)
out$cvAUC
pr <- prediction(pred.logi.model, ntest$SeriousDlqin2yrs)
prf1 <- performance(pr, measure = "tpr", x.measure = "fpr")

# vizuvalizing a few interactions----------------
# lets look at a few interactions 
inter.logi.mod<-glm(SeriousDlqin2yrs~RevolvingUtilizationOfUnsecuredLines + 
                   age + NumberOfTime30.59DaysPastDueNotWorse + DebtRatio + 
                   MonthlyIncome + NumberOfOpenCreditLinesAndLoans + NumberOfTimes90DaysLate + 
                   NumberRealEstateLoansOrLines + NumberOfTime60.89DaysPastDueNotWorse + 
                   NumberOfDependents+ RevolvingUtilizationOfUnsecuredLines:age, data=ntrain,
                 family=binomial)

summary(inter.logi.mod)
plot(Effect(focal.predictors = c("RevolvingUtilizationOfUnsecuredLines","NumberOfTime30.59DaysPastDueNotWorse")
            ,inter.logi.mod,type='response'),multiline = FALSE)
# nothing interesting
inter.logi.mod1<- glm(SeriousDlqin2yrs~RevolvingUtilizationOfUnsecuredLines + 
                        age + NumberOfTime30.59DaysPastDueNotWorse + DebtRatio + 
                        MonthlyIncome + NumberOfOpenCreditLinesAndLoans + NumberOfTimes90DaysLate + 
                        NumberRealEstateLoansOrLines + NumberOfTime60.89DaysPastDueNotWorse + 
                        NumberOfDependents+MonthlyIncome:NumberRealEstateLoansOrLines, data=ntrain,
                      family=binomial)
plot(Effect(focal.predictors = c("MonthlyIncome","NumberRealEstateLoansOrLines")
            ,inter.logi.mod1),type='response')
# while at low number of loans the prob of default dec with inc. monthly income 
# but at high number of real estate loans the probability of default inc with inc 
# monthly income(more like saturated all around really).
inter.logi.mod1<- glm(SeriousDlqin2yrs~RevolvingUtilizationOfUnsecuredLines + 
                        age + NumberOfTime30.59DaysPastDueNotWorse + DebtRatio + 
                        MonthlyIncome + NumberOfOpenCreditLinesAndLoans + NumberOfTimes90DaysLate + 
                        NumberRealEstateLoansOrLines + NumberOfTime60.89DaysPastDueNotWorse + 
                        NumberOfDependents+age:NumberOfTime60.89DaysPastDueNotWorse, data=ntrain,
                      family=binomial)
plot(Effect(focal.predictors = c("age","MonthlyIncome")
            ,inter.logi.mod1),type='response')
# increasing age default rate between 60 to 89 inc.
inter.logi.mod1<- glm(SeriousDlqin2yrs~RevolvingUtilizationOfUnsecuredLines + 
                        age + NumberOfTime30.59DaysPastDueNotWorse + DebtRatio + 
                        MonthlyIncome + NumberOfOpenCreditLinesAndLoans + NumberOfTimes90DaysLate + 
                        NumberRealEstateLoansOrLines + NumberOfTime60.89DaysPastDueNotWorse + 
                        NumberOfDependents, data=ntrain,
                      family=binomial)

plot(Effect(focal.predictors = c("MonthlyIncome","NumberOfTime60.89DaysPastDueNotWorse")
            ,inter.logi.mod1),type='response')
# # #
plot(Effect(focal.predictors = c("NumberOfOpenCreditLinesAndLoans","NumberRealEstateLoansOrLines")
            ,inter.logi.mod1),type='response')
# # #
plot(Effect(focal.predictors = c("age","NumberOfTime60.89DaysPastDueNotWorse")
            ,inter.logi.mod1),type='response')
###
plot(Effect(focal.predictors = c("NumberOfDependents","MonthlyIncome")
            ,inter.logi.mod1),type='response')

p <- ggplot(nDat, aes(MonthlyIncome,NumberOfDependents ))
p + geom_point()

###


# random forest -------------------------------------------------------
library(randomForest)

names(ntrain)<-c('SeriousDlqin2yrs','F1','F2','F3','F4','F5'
                     ,'F6','F7','F8','F9','F10')
names(ntest)<-c('SeriousDlqin2yrs','F1','F2','F3','F4','F5'
                  ,'F6','F7','F8','F9','F10')
names(testDat)<-c('SeriousDlqin2yrs','F1','F2','F3','F4','F5'
                  ,'F6','F7','F8','F9','F10')

a<-proc.time()
credit.forest<-randomForest(SeriousDlqin2yrs~.,
                            data = ntrain,mtry=2,
                            importance=TRUE,
                            ntree=5000)
proc.time()-a

varImpPlot(credit.forest)

pred.forest<-predict(credit.forest,newdata = ntest[,-1],'prob')
output<-pred.forest[,2]
pr <- prediction(output, ntest$SeriousDlqin2yrs)
prf2 <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

# trying to parallelize the forest 
install.packages('doMC')
library(doMC)
library(foreach)
registerDoMC(2)
b<-proc.time()
credit.forestpar<-foreach(ntree=rep(1250, 4)
                       ,.combine=combine
                       ,.multicombine = TRUE
                       ,.packages='randomForest') %dopar% randomForest(SeriousDlqin2yrs~.
      ,data = ntrain
      ,mtry=3
      ,importance=TRUE
      ,ntree=ntree)

proc.time()-b
# some reason it doesnt really help

# gbm -----------------------------------------------------------------

names(ntrain.gbm)<-c('SeriousDlqin2yrs','F1','F2','F3','F4','F5'
                     ,'F6','F7','F8','F9','F10')

library(caret)

gbmGrid<-expand.grid(interaction.depth=c(1,2,3,4,5)
                     ,shrinkage=c(0.01,0.001,0.0001)
                     ,n.trees=(1:50)*100
                     ,n.minobsinnode = 10)

fitControl <- trainControl(method = "cv"
                           ,number = 10
                           ,classProbs = TRUE
                           ,summaryFunction = twoClassSummary)
ntrain.gbm1<-ntrain.gbm
ntrain.gbm$SeriousDlqin2yrs[ntrain.gbm$SeriousDlqin2yrs=='1']<-'Yes'
ntrain.gbm$SeriousDlqin2yrs[ntrain.gbm$SeriousDlqin2yrs=='0']<-'No'
ntrain.gbm$SeriousDlqin2yrs<-as.factor(ntrain.gbm$SeriousDlqin2yrs)
str(ntrain.gbm$SeriousDlqin2yrs)
set.seed(123) 

gbm.forest <- train(SeriousDlqin2yrs~.
                    ,data = ntrain.gbm
                    ,method = "gbm"
                    ,trControl = fitControl
                    ,verbose = TRUE
                    ,tuneGrid = gbmGrid
                    ## Specify which metric to optimize
                    , metric = 'ROC'
                    )

summary(gbm.forest)
plot(gbm.forest, metric='ROC',plotType='level')
ggplot(gbm.forest)
(gbm.forest$bestTune)

# fitting the best selected gbm
library(pROC)
library(gbm)
gbm.credit<-gbm(SeriousDlqin2yrs~.    
                
                ,data = ntrain.gbm
                ,distribution = 'bernoulli'
                ,n.trees = 3500
                ,interaction.depth = 5
                ,shrinkage = 0.01
                ,n.minobsinnode = 10
                ,verbose = TRUE
                ,cv.folds = 5
                ,class.stratify.cv = TRUE
                ,n.cores = 2 
                ,train.fraction = 0.8
                
)

summary(gbm.credit)
# best.iter<-gbm.perf(gbm.credit, method = 'cv')

f.predict <- predict.gbm(gbm.credit,ntest[,-1],2100, type = 'response')
pr <- prediction(f.predict, ntest$SeriousDlqin2yrs)
prf3 <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

# gbm with varmonotone ------------------------------------------------

# grid search varmonotone
gbmGrid<-expand.grid(interaction.depth=c(1,2,3,4,5)
                     ,shrinkage=0.01
                     ,n.trees=(1:50)*80
                     ,n.minobsinnode = 10)

fitControl <- trainControl(method = "cv"
                           ,number = 10
                           ,classProbs = TRUE
                           ,summaryFunction = twoClassSummary)

set.seed(123) 


gbm.forest.var <- train(SeriousDlqin2yrs~.
                    ,data = ntrain.gbm
                    ,method = "gbm"
                    ,trControl = fitControl
                    ,verbose = TRUE
                    ,tuneGrid = gbmGrid
                    ,var.monotone = c(0,0,1,1,0,0,1,0,1,0)
                    ## Specify which metric to optimize
                    ,metric = 'ROC'
                    )
summary(gbm.forest)
plot(gbm.forest)
gbm.forest.var$bestTune

# fitting the best gbm selected from the grid search 
gbm.relation.credit<-gbm.fit(x = ntrain[,-1],y = as.numeric(ntrain[,1])-1
              ,n.trees = 3000,interaction.depth = 3
              ,var.monotone = c(0,0,1,1,0,0,0,0,0,0)
              ,shrinkage = 0.01
              ,n.minobsinnode = 10
              , nTrain =nrow(ntrain)*0.8)

pred.boo <- predict(gbm.relation.credit,ntest[,-1],1000)
pr <- prediction(pred.boo, ntest$SeriousDlqin2yrs)
prf4 <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
plot(out.monoboost$perf)



# # gbm with adaboost ---------------------------------------------------
# gbm.ada.credit<-gbm.fit(x = ntrain[,-1],y = as.numeric(ntrain[,1])-1
#               ,n.trees = 500,interaction.depth = 3
#               ,distribution = 'adaboost' 
#               , nTrain =nrow(nTrain)*0.8)
# summary(gbm.ada.credit)
# ada.predict <- predict(gbm.ada.credit,ntest,best.iter)
# out.monoada<-cvAUC(ada.predict,ntest$SeriousDlqin2yrs)
# plot(out.monoada$perf)
# 
# 
# # gbm with adaboost and varmonotone -----------------------------------
# gbm.ada.relation.credit<-gbm.fit(x = ntrain[,-1],y = as.numeric(ntrain[,1])-1
#               ,n.trees = 500,interaction.depth = 3
#               ,var.monotone = c(0,0,1,1,0,0,0,0,0,0)
#               , distribution = 'adaboost'
#               , nTrain =nrow(ntrain)*0.8)
# summary(gbm.ada.relation.credit)

# xgboost -------------------------------------------------------------

dtrain <- xgb.DMatrix(data = as.matrix(ntrain[,-1])
                      , label = as.numeric(ntrain$SeriousDlqin2yrs)-1)

dtest<- xgb.DMatrix(data = as.matrix(ntest[,-1])
                    , label = as.numeric(ntest$SeriousDlqin2yrs)-1)

watchlist <- list(train=dtrain, test=dtest)

bst <- xgb.train(data=dtrain, max.depth=3
                 , eta=0.01, nthread = 2, nround=2000
                 , watchlist=watchlist, eval.metric = "error"
                 , eval.metric = "logloss"
                 
                 , objective = "binary:logistic")
print(xgb.importance(model = bst))
xgb.plot.importance(importance_matrix = xgb.importance(model = bst))
pred.xg<-predict(bst,dtest)
pr <- prediction(pred.xg, ntest$SeriousDlqin2yrs)
prf5 <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

ftest<- xgb.DMatrix(data = as.matrix(testDat[,-1])
                    , label = rep(0,nrow(testDat)))



# kaggle entries ------------------------------------------------------

# step wise selection logistic model
head(testDat)
pred.logi.model<-predict(step.logi,newdata=testDat[,-1],type='response')
write.csv(cbind(1:101503,pred.logi.model),file = 'logi_entry.csv',row.names = F)

# main effects logistic model 
pred.logi.model<-predict(logi.model,newdata=testDat[,-1],type='response')
write.csv(cbind(1:101503,pred.logi.model),file = 'logi_mainEff_entry.csv',row.names = F)

# random forest 
pred.forest<-predict(credit.forest,newdata = testDat[,-1],'prob')
output<-pred.forest[,2]
write.csv(cbind(1:101503,output)
          ,file = 'random_forest_entry.csv'
          ,row.names = F)

# gbm 
gbm.mod.pred <- predict.gbm(gbm.credit,testDat[,-1],2100, type = 'response')
write.csv(cbind(1:101503,gbm.mod.pred),file = 'gbm_entry.csv',row.names = F)

# gbm with variable relationship
gbm.rel.pred <- predict.gbm(gbm.relation.credit,testDat[,-1],2100, type = 'response')
write.csv(cbind(1:101503,gbm.rel.pred),file = 'gbm_rel_entry.csv',row.names = F)

# xgboost
pred.xg<-predict(bst,ftest)
write.csv(cbind(1:101503,pred.xg),file = 'xgboost_entry.csv',row.names = F)

# ROC curves of all models
plot( prf0) #black
plot(prf1, add = TRUE,col=2 ) #red
plot(prf2, add = TRUE, col=3) #green
plot(prf3, add = TRUE, col=4) #blue
plot(prf4, add = TRUE, col=5) #cyan
plot(prf5, add = TRUE, col=6) #purple

