### Predict the Success of Bank Telemarketing

# Data preprocess & transformation
## install & load necessary packages
library(gtools)
library(dplyr)
library(RWeka)

# load the training data set
allset <- read.csv("//hd.ad.syr.edu/02/f83e50/Documents/Downloads/IST 707/Project/Bank-all-0408.csv")
str(allset)

# calculate the info gain of each variables
infoGain <- sort(format(InfoGainAttributeEval(y~.,data = allset),scientific = FALSE))
infoGain
# find out the variables with low info gain

## Data Preprocessing and Preparation
# remove the variables with low info gain: day,default,education
allset <- subset(allset, select = -c(day,default,education,marital,loan,campaign))
# For the variable "Age", we turned it into "age group" which is categorical.
summary(allset$age)
allset$ageGroup <- cut(allset$age, c(17,24,34,44,54,64,95))
summary(allset$ageGroup)
allset <- subset(allset, select = -c(age))

# For the variable "Balance", we decided to discrete it by 10 percentiles.
summary(allset$balance)
allset$balanceGroup <- quantcut(allset$balance, q=10,scientific = FALSE)
summary(allset$balanceGroup)
allset <- subset(allset, select = -c(balance))

# For the variable "Duration", it was discarded since our intention was to have a realistic predictive model.
allset <- subset(allset, select = -c(duration))

# For the variable "Pdays", we discreted it into 6 categories
# "not previously contacted", "contacted in last 1 month","contacted in last 3 months"
# "contacted in last 6 months","contacted in last 1 year","contacted 1 year before".
summary(allset$pdays)
allset$pdaysGroup <- cut(allset$pdays, c(-2,0,30,90,180,365,871))
summary(allset$pdaysGroup)
allset <- subset(allset, select = -c(pdays))

# For the variable "Previous", we discreted it into 4 categories, 
# "contacted 0 times", "contacted 1-3 times","contacted 3-10 times","contacted more than 10 times"
summary(allset$previous)
allset$previousGroup <- cut(allset$previous, c(-1,0,3,10,275))
summary(allset$previousGroup)
allset <- subset(allset, select = -c(previous))

# For the variable "Contact" and "Poutcome", remove them.
allset <- subset(allset, select = -c(contact, poutcome))
str(allset)

# rename the columns
colnames(allset)[c(5:8)]<- c("age","balance","pdays","previous")
colnames(allset)

# label the factors for reading
allset$age <- factor(allset$age,labels = c("19-24","25-34","35-44","45-54","55-64","65+"))
allset$pdays <- factor(allset$pdays, labels = c("not contacted","contacted in last 1 month","contacted in last 1-3 months","contacted in last 3-6 months","contacted in last 6 months to 1 year","contacted 1 year before"))
allset$previous <- factor(allset$previous, labels = c("contacted 0 times","contacted 1-3 times","contacted 3-10 times","contacted more than 10 times"))
allset$balance <- factor(allset$balance,labels = c("[-8020,0]","(0,22]","(22,131]","(131,272]","(272,448]"," (448,701]","(701,1303]","(1303,1860] ","(1860,3570] ","(3570,102000]"))
summary(allset)
str(allset)

sample <- sample.int(n=nrow(allset),size = floor(0.66*nrow(allset)),replace = FALSE)
trainset <- allset[sample,]
testset <- allset[-sample,]
str(trainset)
str(testset)
write.csv(trainset,file = "//hd.ad.syr.edu/02/f83e50/Documents/Downloads/IST 707/Project/Bank-trainset-0408.csv",row.names = FALSE)
write.csv(testset,file = "//hd.ad.syr.edu/02/f83e50/Documents/Downloads/IST 707/Project/Bank-testset-0408.csv",row.names = FALSE)




CLASSIFICATION MODELS
UPLOAD DATA
trainset <- read.csv("C:/Users/Spencer/Desktop/IST707/Final Project/BankTrainset.csv")
testset <- read.csv("C:/Users/Spencer/Desktop/IST707/Final Project/BankTestset.csv")
Load Necessary Packages
library(kernlab)
library(randomForest)
library(class)
library(RWeka)
SVM MODEL
#Load Package for building the model
library(kernlab)

#Build model (3min)
SVM_M1 <- ksvm(y~., data = trainset, na.action = na.omit, kernel="rbfdot",C=5,cross=3)
SVM_M1

#The accuracy is 88.1531%
print(SVM_M1)

#Precision for model 1 is 0.8009709, Recall is 0.2360515, F Measure is 0.3646409
SVM_PT1 <- predict(SVM_M1, trainset[,-4])

SVM_PRECISION1 <- precision(data = SVM_PT1, reference = Fact, relevant = "yes")
SVM_RECALL1 <- recall(data = SVM_PT1, reference = Fact, relevant = "yes")
SVM_F1 <- F_meas(data = SVM_PT1, reference = Fact, relevant = "yes", beta = 1, na.rm = TRUE)

#Build model 2 (3min)
SVM_M2 <- ksvm(y~., data = trainset, na.action = na.omit, kernel="rbfdot",C=5,cross=10)

#The accuracy of model 2 is 88.1966%
print(SVM_M2)

#Precision for model 2 is 0.8009709, Recall is 0.2360515, F Measure is 0.3646409
library(caret)
SVM_PT2 <- predict(SVM_M2, trainset[,-4])

SVM_PRECISION2 <- precision(data = SVM_PT2, reference = Fact, relevant = "yes")
SVM_RECALL2 <- recall(data = SVM_PT2, reference = Fact, relevant = "yes")
SVM_F2 <- F_meas(data = SVM_PT2, reference = Fact, relevant = "yes", beta = 1, na.rm = TRUE)

#Build model 3 (3min)
SVM_M3 <- ksvm(y~., data = trainset, na.action = na.omit, kernel="rbfdot",C=3,cross=10)

#The accuracy of model 3 is 88.3843%
print(SVM_M3)

#Precision for model 3 is 0.775, Recall is 0.1951359, F Measure is 0.3117714
SVM_PT3 <- predict(SVM_M3, trainset[,-4])

SVM_PRECISION3 <- precision(data = SVM_PT3, reference = Fact, relevant = "yes")
SVM_RECALL3 <- recall(data = SVM_PT3, reference = Fact, relevant = "yes")
SVM_F3 <- F_meas(data = SVM_PT3, reference = Fact, relevant = "yes", beta = 1, na.rm = TRUE)

#Build model 4 (3min)
SVM_M4 <- ksvm(y~., data = trainset, na.action = na.omit, kernel="polydot",C=3,cross=10)

#The accuracy of model 4 is 88.2201%
print(SVM_M4)

#Precision for model 4 is 0.5031646, Recall is 0.04549356, F Measure is 0.08344267
SVM_PT4 <- predict(SVM_M4, trainset[,-4])

SVM_PRECISION4 <- precision(data = SVM_PT4, reference = Fact, relevant = "yes")
SVM_RECALL4 <- recall(data = SVM_PT4, reference = Fact, relevant = "yes")
SVM_F4 <- F_meas(data = SVM_PT4, reference = Fact, relevant = "yes", beta = 1, na.rm = TRUE)

#Build model 5 (3min)
SVM_M5 <- ksvm(y~., data = trainset, na.action = na.omit, kernel="polydot",C=5,cross=10)

#The accuracy of model 5 is 88.1464%
print(SVM_M5)

#Precision for model 5 is 0.5031646, Recall is 0.04549356, F Measure is 0.08344267
SVM_PT5 <- predict(SVM_M5, trainset[,-4])

SVM_PRECISION5 <- precision(data = SVM_PT5, reference = Fact, relevant = "yes")
SVM_RECALL5 <- recall(data = SVM_PT5, reference = Fact, relevant = "yes")
SVM_F5 <- F_meas(data = SVM_PT5, reference = Fact, relevant = "yes", beta = 1, na.rm = TRUE)

#Build model 6 (3min)
SVM_M6 <- ksvm(y~., data = trainset, na.action = na.omit, kernel="vanilladot",C=5,cross=10)

#The accuracy of model 6 is 88.2%
print(SVM_M6)

#Precision for model 6 is 0.5031646, Recall is 0.04549356, F Measure is 0.08344267
SVM_PT6 <- predict(SVM_M6, trainset[,-4])

SVM_PRECISION6 <- precision(data = SVM_PT6, reference = Fact, relevant = "yes")
SVM_RECALL6 <- recall(data = SVM_PT6, reference = Fact, relevant = "yes")
SVM_F6 <- F_meas(data = SVM_PT6, reference = Fact, relevant = "yes", beta = 1, na.rm = TRUE)

#Build model 7 (3min)
SVM_M7 <- ksvm(y~., data = trainset, na.action = na.omit, kernel="vanilladot",C=1,cross=10)

#The accuracy of model 7 is 88.2235%
print(SVM_M7)

#Precision for model 7 is 0.5031646, Recall is 0.04549356, F Measure is 0.08344267
SVM_PT7 <- predict(SVM_M7, trainset[,-4])

SVM_PRECISION7 <- precision(data = SVM_PT7, reference = Fact, relevant = "yes")
SVM_RECALL7 <- recall(data = SVM_PT7, reference = Fact, relevant = "yes")
SVM_F7 <- F_meas(data = SVM_PT7, reference = Fact, relevant = "yes", beta = 1, na.rm = TRUE)

#Build model 8 (3min)
SVM_M8 <- ksvm(y~., data = trainset, na.action = na.omit, kernel="tanhdot",C=3,cross=10)

#The accuracy of model 8 is 79.4698%
print(SVM_M8)

#Precision for model 8 is 0.1135647, Recall is 0.1133047, F Measure is 0.1134345
SVM_PT8 <- predict(SVM_M8, trainset[,-4])

SVM_PRECISION8 <- precision(data = SVM_PT8, reference = Fact, relevant = "yes")
SVM_RECALL8 <- recall(data = SVM_PT8, reference = Fact, relevant = "yes")
SVM_F8 <- F_meas(data = SVM_PT8, reference = Fact, relevant = "yes", beta = 1, na.rm = TRUE)

# Build model 9 (3min)
SVM_M9 <- ksvm(y~., data = trainset, na.action = na.omit, kernel="laplacedot",C=3,cross=10)

#The accuracy of model 9 is 88.3709%
print(SVM_M9)

#Precision for model 9 is 0.8836478, Recall is 0.2412017, F Measure is 0.3789616
SVM_PT9 <- predict(SVM_M9, trainset[,-4])

SVM_PRECISION9 <- precision(data = SVM_PT9, reference = Fact, relevant = "yes")
SVM_RECALL9 <- recall(data = SVM_PT9, reference = Fact, relevant = "yes")
SVM_F9 <- F_meas(data = SVM_PT9, reference = Fact, relevant = "yes", beta = 1, na.rm = TRUE)

#Build model 10 (3min)
SVM_M10 <- ksvm(y~., data = trainset, na.action = na.omit, kernel="laplacedot",C=5,cross=10)

#The accuracy of model 10 is 88.3709%
print(SVM_M10)

#Precision for model 10 is 0.9203476, Recall is 0.3636624, F Measure is 0.521329
SVM_PT10 <- predict(SVM_M10, trainset[,-4])

SVM_PRECISION10 <- precision(data = SVM_PT10, reference = Fact, relevant = "yes")
SVM_RECALL10 <- recall(data = SVM_PT10, reference = Fact, relevant = "yes")
SVM_F10 <- F_meas(data = SVM_PT10, reference = Fact, relevant = "yes", beta = 1, na.rm = TRUE)

#Build model 11 (3min)
SVM_M11 <- ksvm(y~., data = trainset, na.action = na.omit, kernel="besseldot",C=5,cross=10)

#The accuracy of model 11 is 88.3374%
print(SVM_M11)

#Precision for model 11 is 0.7240185, Recall is 0.1793991, F Measure is 0.2875487
SVM_PT11 <- predict(SVM_M11, trainset[,-4])

SVM_PRECISION11 <- precision(data = SVM_PT11, reference = Fact, relevant = "yes")
SVM_RECALL11 <- recall(data = SVM_PT11, reference = Fact, relevant = "yes")
SVM_F11 <- F_meas(data = SVM_PT11, reference = Fact, relevant = "yes", beta = 1, na.rm = TRUE)


#Build model 12 (3min)
SVM_M12 <- ksvm(y~., data = trainset, na.action = na.omit, kernel="besseldot",C=3,cross=10)

#The accuracy of model 12 is 88.4111%
print(SVM_M12)
1-0.115889
#Precision for model 12 is 0.6853333, Recall is 0.1470672, F Measure is 0.2421673
SVM_PT12 <- predict(SVM_M12, trainset[,-4])

SVM_PRECISION12 <- precision(data = SVM_PT12, reference = Fact, relevant = "yes")
SVM_RECALL12 <- recall(data = SVM_PT12, reference = Fact, relevant = "yes")
SVM_F12 <- F_meas(data = SVM_PT12, reference = Fact, relevant = "yes", beta = 1, na.rm = TRUE)
RANDOM FOREST MODEL
#Load Package for building the model
library(randomForest)

#Build model(1min)
RF_M500 <- randomForest(y~., data = trainset, ntree = 500, na.action=na.fail)

RF_M100 <- randomForest(y~., data = trainset, ntree = 100, na.action=na.fail)

#The accuracy for 500 trees is 88.35%
#print(RF_M500)

#The accuracy for 100 trees is 88.25%
#print(RF_M100)

#Precision for 500 trees is 0.9526227, Recall is 0.3221745, F Measure is 0.4815052
newtrain <- data.frame(y=NA,trainset[,-4])
Fact <- trainset$y

RF_P500 <- predict(RF_M500,newtrain)
RF_PRECISION500 <- precision(data = RF_P500, reference = Fact, relevant = "yes")
RF_RECALL500 <- recall(data = RF_P500, reference = Fact, relevant = "yes")
RF_F500 <- F_meas(data = RF_P500, reference = Fact, relevant = "yes", beta = 1, na.rm = TRUE)

#Precision for 100 trees is 0.9468641, Recall is 0.3110157, F Measure is 0.4682317
RF_P100 <- predict(RF_M100,newtrain)
RF_PRECISION100 <- precision(data = RF_P100, reference = Fact, relevant = "yes")
RF_RECALL100 <- recall(data = RF_P100, reference = Fact, relevant = "yes")
RF_F100 <- F_meas(data = RF_P100, reference = Fact, relevant = "yes", beta = 1, na.rm = TRUE)

kNN MODEL
#Load Package for Building the Model
library(class)

#Process Data for Building the Model
kNNtrain <- data.frame(lapply(trainset, function(x) as.numeric(x)))

#Build model(1min)
kNN_M3 <- knn.cv(train = kNNtrain, kNNtrain$y, k = 3, l = 2, prob = F, use.all = T) 

kNN_M5 <- knn.cv(train = kNNtrain, kNNtrain$y, k = 5, l = 4, prob = F, use.all = T)

#The accuracy for k=3 is 91.39716%
CM_M3 = as.matrix(table(Actual = kNNtrain$y, Predicted = kNN_M3))
#sum(diag(CM_M3))/length(kNNtrain$y)

#The accuracy for k=5 is 87.65039%
CM_M5 = as.matrix(table(Actual = kNNtrain$y, Predicted = kNN_M5))
#sum(diag(CM_M5))/length(kNNtrain$y)

#Precision for k = 3 is 0.9398551, Recall is 0.6958155, F Measure is 0.7996301
kNN_PRECISION3 <- precision(data = factor(kNN_M3), reference = factor(as.numeric(Fact)), relevant = "2")
kNN_RECALL3 <- recall(data = factor(kNN_M3), reference = factor(as.numeric(Fact)), relevant = "2")
kNN_F3 <- F_meas(data = factor(kNN_M3), reference = factor(as.numeric(Fact)), relevant = "2")

#Precision for k = 5 is 0.9708589, Recall is 0.6519053, F Measure is 0.780037
kNN_PRECISION5 <- precision(data = factor(kNN_M5), reference = factor(as.numeric(Fact)), relevant = "2")
kNN_RECALL5 <- recall(data = factor(kNN_M5), reference = factor(as.numeric(Fact)), relevant = "2")
kNN_F5 <- F_meas(data = factor(kNN_M5), reference = factor(as.numeric(Fact)), relevant = "2")

DECISION TREE
#Load Package for Building the Model
library(RWeka)

#Build model(1min)
DT_M1 <- J48(y~., data = trainset, control=Weka_control(U=FALSE, M=2, C=0.5))

#The accuracy for model 1 is 88.1698 %
DT_E1 <- evaluate_Weka_classifier(DT_M1, numFolds = 3, seed = 1, class = TRUE)

#Precision for model 1 is 0.7394541, Recall is 0.255794, F Measure is 0.380102
DT_PT1 <- predict(DT_M1, trainset[,-4])
DT_PRECISION1 <- precision(data = DT_PT1, reference = Fact, relevant = "yes")
DT_RECALL1 <- recall(data = DT_PT1, reference = Fact, relevant = "yes")
DT_F1 <- F_meas(data = DT_PT1, reference = Fact, relevant = "yes", beta = 1, na.rm = TRUE)

#Build model 2 (1min)
DT_M2 <- J48(y~., data = trainset, control=Weka_control(U=FALSE, M=2, C=0.3))

#The accuracy of model 2 is 88.515  %
DT_E2 <- evaluate_Weka_classifier(DT_M2, numFolds = 3, seed = 1, class = TRUE)
DT_E2

#Precision for model 2 is 0.6506623, Recall is 0.1124464, F Measure is 0.1917541
DT_PT2 <- predict(DT_M2, trainset[,-4])
DT_PRECISION2 <- precision(data = DT_PT2, reference = Fact, relevant = "yes")
DT_RECALL2 <- recall(data = DT_PT2, reference = Fact, relevant = "yes")
DT_F2 <- F_meas(data = DT_PT2, reference = Fact, relevant = "yes", beta = 1, na.rm = TRUE)
NAIVE BAYES
#Build function for Naive Bayes Model
NB <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")

#Build Model with discretization
NB_M1 <- NB(y~., data=trainset, control=Weka_control(D=TRUE))

#The accuracy of model 1 is 87.2616 %
NB_E1 <- evaluate_Weka_classifier(NB_M1, numFolds = 3, seed = 1, class = TRUE)

#Precision for model 1 is 0.4413432, Recall is 0.2895565, F Measure is 0.349689
NB_P1 <- predict (NB_M1, trainset[,-4])

NB_PRECISION1 <- precision(data = NB_P1, reference = Fact, relevant = "yes")
NB_RECALL1 <- recall(data = NB_P1, reference = Fact, relevant = "yes")
NB_F1 <- F_meas(data = NB_P1, reference = Fact, relevant = "yes", beta = 1, na.rm = TRUE)



ASSOCIATION RULES
train<- read.csv('//Mac/Home/Desktop/SU/2019spring/IST707/project/Bank-trainset-0408.csv')
test <- read.csv('//Mac/Home/Desktop/SU/2019spring/IST707/project/Bank-testset-0408.csv')
test1 <- test[-1,]
ar <- rbind(train,test1)
library(plyr)
library(dplyr)
library(arules)
library("arulesViz")

good <- apriori(ar,parameter = list(supp=0.026,conf=0.01,minlen=2,maxlen=4),appearance = list(rhs="y=yes",default="lhs"))
g_supp <- sort(good,by="support",decreasing = TRUE)
g_supp <- head(g_supp)
inspect(g_supp)
g_conf <- sort(good,by="confidence",decreasing = TRUE)
g_conf <- head(g_conf)
inspect(g_conf)
g_lift <- sort(good,by="lift",decreasing = TRUE)
g_lift <- head(g_lift)
inspect(g_lift)

bad <- apriori(ar,parameter = list(supp=0.25,conf=0.7,minlen=2),appearance = list(rhs="y=no",default="lhs"))
b_supp <- sort(bad,by="support",decreasing = TRUE)
b_supp <- head(b_supp)
inspect(b_supp)
b_conf <- sort(bad,by="confidence",decreasing = TRUE)
b_conf <- head(b_conf)
inspect(b_conf)
b_lift <- sort(bad,by="lift",decreasing = TRUE)
b_lift <- head(b_lift)
inspect(b_lift)

EVALUATION
#decision tree
DT_M1 <- J48(y~., data = train, control=Weka_control(U=FALSE, M=2, C=0.5))
DT_PTT1 <- predict(DT_M1, test[,-4])
DT_TPRECISION1 <- precision(data = DT_PTT1, reference = Fact, relevant = "yes")
DT_TRECALL1 <- recall(data = DT_PTT1, reference = Fact, relevant = "yes")
DT_TF1 <- F_meas(data = DT_PTT1, reference = Fact, relevant = "yes", beta = 1, na.rm = TRUE)
DT_TPRECISION1
DT_TRECALL1
DT_TF1
confusionMatrix(DT_PTT1, Fact)
DT_ACCURACY <-(13294+299)/(13294+1495+284+299)
DT_ACCURACY


#naive bayes
NB <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")
NB_M1 <- NB(y~., data=train, control=Weka_control(D=TRUE))
NB_PT1 <- predict (NB_M1, test[,-4])
NB_TPRECISION1 <- precision(data = NB_PT1, reference = Fact, relevant = "yes")
NB_TRECALL1 <- recall(data = NB_PT1, reference = Fact, relevant = "yes")
NB_TF1 <- F_meas(data = NB_PT1, reference = Fact, relevant = "yes", beta = 1, na.rm = TRUE)
NB_TPRECISION1
NB_TRECALL1
NB_TF1
confusionMatrix(NB_PT1, Fact)
DT_ACCURACY <-(12905+471)/(12905+1323+673+471)
DT_ACCURACY


#KNN
library(class)
kNNtrain <- data.frame(lapply(train, function(x) as.numeric(x)))
KNNtest <- data.frame(lapply(test, function(x) as.numeric(x)))
cl <- kNNtrain$y
Tpred3 <- knn(kNNtrain,KNNtest,cl,k = 3, l = 2, prob = F, use.all = T)
kNN_TPRECISION3 <- precision(data = factor(Tpred3), reference = factor(as.numeric(Fact)), relevant = "2")
kNN_TRECALL3 <- recall(data = factor(Tpred3), reference = factor(as.numeric(Fact)), relevant = "2")
kNN_TF3 <- F_meas(data = factor(Tpred3), reference = factor(as.numeric(Fact)), relevant = "2")
kNN_TPRECISION3
kNN_TRECALL3
kNN_TF3
confusionMatrix(factor(Tpred3), factor(as.numeric(Fact)))
KNN3_ACCURACY <-(13360+628)/(13360+628+34+291)
KNN3_ACCURACY

Tpred5 <- knn(kNNtrain,KNNtest,cl,k = 5, l = 4, prob = F, use.all = T)
kNN_TPRECISION5 <- precision(data = factor(Tpred5), reference = factor(as.numeric(Fact)), relevant = "2")
kNN_TRECALL5 <- recall(data = factor(Tpred5), reference = factor(as.numeric(Fact)), relevant = "2")
kNN_TF5 <- F_meas(data = factor(Tpred5), reference = factor(as.numeric(Fact)), relevant = "2")
kNN_TPRECISION5
kNN_TRECALL5
kNN_TF5
confusionMatrix(factor(Tpred5), factor(as.numeric(Fact)))
KNN5_ACCURACY <-(13146+306)/(13146+306+189+4)
KNN5_ACCURACY


Random Forest

#Random forest
library(randomForest)

RF_M500 <- randomForest(y~., data = train, ntree = 500, na.action=na.fail)
RF_M100 <- randomForest(y~., data = train, ntree = 100, na.action=na.fail)
newtrain <- data.frame(y=NA,test[,-4])

#Precision for 500 trees is 0.4922179, Recall is 0.1410256, F Measure is 0.2192374
RF_TP500 <- predict(RF_M500,newtrain)
RF_TPRECISION500 <- precision(data = RF_TP500, reference = Fact, relevant = "yes")
RF_TRECALL500 <- recall(data = RF_TP500, reference = Fact, relevant = "yes")
RF_TF500 <- F_meas(data = RF_TP500, reference = Fact, relevant = "yes", beta = 1, na.rm = TRUE)
confusionMatrix(RF_TP500, Fact)
KNN5_ACCURACY <-(13317+252)/(13317+252+261+1541)
KNN5_ACCURACY#0.8828

#Precision for 100 trees is 0.4841897, Recall is 0.1365663, F Measure is 0.2130435
RF_TP100 <- predict(RF_M100,newtrain)
RF_TPRECISION100 <- precision(data = RF_TP100, reference = Fact, relevant = "yes")
RF_TRECALL100 <- recall(data = RF_TP100, reference = Fact, relevant = "yes")
RF_TF100 <- F_meas(data = RF_TP100, reference = Fact, relevant = "yes", beta = 1, na.rm = TRUE)
confusionMatrix(RF_TP100, Fact)
KNN5_ACCURACY <-(13317+245)/(13317+245+261+1549)
KNN5_ACCURACY

#svm
library(kernlab)

SVM_M12 <- ksvm(y~., data = train, na.action = na.omit, kernel="besseldot",C=2,cross=10)
SVM_PTT12 <- predict(SVM_M12, test[,-4])

SVM_TPRECISION12 <- precision(data = SVM_PTT12, reference = Fact, relevant = "yes")
SVM_TRECALL12 <- recall(data = SVM_PTT12, reference = Fact, relevant = "yes")
SVM_TF12 <- F_meas(data = SVM_PTT12, reference = Fact, relevant = "yes", beta = 1, na.rm = TRUE)

SVM_TPRECISION12
SVM_TRECALL12
SVM_TF12
confusionMatrix(SVM_PTT12, Fact)
KNN5_ACCURACY <-(13407+187)/(13407+187+1607+171)
KNN5_ACCURACY

###KNN for all data
finalFact <- ar$y
KNNfinal <- data.frame(lapply(ar, function(x) as.numeric(x)))
KNN_finalpred <- knn(kNNtrain,KNNfinal,cl,k = 5, l = 4, prob = F, use.all = T)
write.csv(KNN_finalpred, file="//mac/Home/Desktop/SU/2019spring/IST707/knn-pred.csv", row.names=FALSE)
kNN_FPRECISION5 <- precision(data = factor(KNN_finalpred), reference = factor(as.numeric(finalFact)), relevant = "2")
kNN_FRECALL5 <- recall(data = factor(KNN_finalpred), reference = factor(as.numeric(finalFact)), relevant = "2")
kNN_FF5 <- F_meas(data = factor(KNN_finalpred), reference = factor(as.numeric(finalFact)), relevant = "2")
kNN_FPRECISION5
kNN_FRECALL5
kNN_FF5
confusionMatrix(factor(KNN_finalpred), factor(as.numeric(finalFact)))

