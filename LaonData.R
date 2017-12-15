setwd("F:/Topgear_R_Assignment")
LoanData <- read.csv("F:/Topgear_R_Assignment/Data.csv",na.strings = c("","NA"," "),header = TRUE,comment.char = "")

##Check for Data and Basic Summary
View(LoanData)
str(LoanData)
summary(LoanData)

## Loan Status Distribution
prop.table(table(LoanData$Loan_Status))
paste(prop.table(table(LoanData$Loan_Status))*100,"%")


## Check for missing value

apply(LoanData, 2, function(x) sum(is.na(x)))
colSums(is.na(LoanData))*100/nrow(LoanData)

## Missing Value Treatment 

LoanData[is.na(LoanData$Married),"Married"] <- "No"
LoanData[is.na(LoanData$Gender),"Gender"] <- "Male"
LoanData[is.na(LoanData$Credit_History),"Credit_History"] <- "1"
LoanData[is.na(LoanData$Dependents) & (LoanData$Married == "Yes"),"Dependents"] <- 1
LoanData[is.na(LoanData$Dependents),"Dependents"] <- 0

LoanData[is.na(LoanData$Education),"Education"] <- "Graduate"

LoanData[is.na(LoanData$Self_Employed),"Self_Employed"] <- "No"

LoanData$Loan_Amount_Term <- as.factor(LoanData$Loan_Amount_Term)
LoanData$Credit_History <- as.factor(LoanData$Credit_History)

## Missing Value For Laon Amount Basis Property_Area and Loan Tenure

## Missing value for Loan Tenrue basis Loan Amount and Property_area

is.na(LoanData$Gender)

summary(LoanData$Credit_History)
LoanData[is.na(LoanData$Loan_Amount_Term) & LoanData$Property_Area == "Semiurban","LoanAmount"]
LoanData[is.na(LoanData$Loan_Amount_Term) & LoanData$Property_Area == "Urban" & LoanData$LoanAmount < 100,"Loan_Amount_Term"] <-240
LoanData[is.na(LoanData$Loan_Amount_Term) & LoanData$Property_Area == "Urban","Loan_Amount_Term"] <- 84
LoanData[is.na(LoanData$Loan_Amount_Term) & LoanData$Property_Area == "Rural" & LoanData$LoanAmount > 160,"Loan_Amount_Term"] <-300

LoanData[is.na(LoanData$Loan_Amount_Term) & LoanData$Property_Area == "Semiurban" & LoanData$LoanAmount < 160,"Loan_Amount_Term"] <-84
LoanData[is.na(LoanData$Loan_Amount_Term) & LoanData$Property_Area == "Semiurban" & LoanData$LoanAmount > 160,"Loan_Amount_Term"] <-180

LoanData[is.na(LoanData$LoanAmount) & LoanData$Property_Area == "Semiurban","Loan_Amount_Term"]

LoanData[is.na(LoanData$LoanAmount) & LoanData$Property_Area == "Rural","LoanAmount"] <- 133
LoanData[is.na(LoanData$LoanAmount) & LoanData$Property_Area == "Urban" & LoanData$Loan_Amount_Term == "360","LoanAmount"] <- 125
LoanData[is.na(LoanData$LoanAmount) & LoanData$Property_Area == "Urban" & LoanData$Loan_Amount_Term == "180","LoanAmount"] <- 120
LoanData[is.na(LoanData$LoanAmount) & LoanData$Property_Area == "Semiurban" & LoanData$Loan_Amount_Term == "360","LoanAmount"] <- 128
LoanData[is.na(LoanData$LoanAmount) & LoanData$Property_Area == "Semiurban" & LoanData$Loan_Amount_Term == "240","LoanAmount"] <- 205

aggregate(x=LoanData$LoanAmount,by =list(LoanData$Loan_Amount_Term,LoanData$Property_Area),FUN = "mean")

aggregate(x=LoanData$LoanAmount,by =list(LoanData$Property_Area,LoanData$Loan_Amount_Term),FUN = "median",na.rm = TRUE)

median(LoanData$LoanAmount,na.rm = TRUE)

summary(LoanData$ApplicantIncome)
summary(LoanData$CoapplicantIncome)
summary(LoanData$LoanAmount)

LoanData <- read.csv("F:/Topgear_R_Assignment/Data1.csv",na.strings = c("","NA"," "),header = TRUE,comment.char = "")


## Data Visualization to check for any relationship b/t laon status and other independent variable
library(ggplot2)


ggplot(LoanData,aes(LoanData$Loan_Status,..count..))  + geom_bar(aes(fill=LoanData$Gender))

ggplot(LoanData,aes(LoanData$Loan_Status,..count..))  + geom_bar(aes(fill=LoanData$Married))

ggplot(LoanData,aes(LoanData$Loan_Status,..count..))  + geom_bar(aes(fill=LoanData$Dependents))

ggplot(LoanData,aes(LoanData$Loan_Status,..count..))  + geom_bar(aes(fill=LoanData$Education))

ggplot(LoanData,aes(LoanData$Loan_Status,..count..))  + geom_bar(aes(fill=LoanData$Credit_History))

ggplot(LoanData,aes(LoanData$Loan_Status,..count..))  + geom_bar(aes(fill=LoanData$Property_Area))

ggplot(LoanData,aes(LoanData$Loan_Status,..count..))  + geom_bar(aes(fill=LoanData$Self_Employed))

gdata<-aggregate(x= LoanData$ApplicantIncome, by = list(LoanData$Loan_Status), FUN= mean,na.rm=TRUE)
print(gdata)

barplot(gdata$x,legend = gdata$Group.1,col=c("red","Blue"),main=colnames(gdata$Group.1),names.arg=c("N(Not Approved)","Y(Approved)"))

gdata<-aggregate(x= LoanData$CoapplicantIncome, by = list(LoanData$Loan_Status), FUN= mean,na.rm=TRUE)
print(gdata)

barplot(gdata$x,legend = gdata$Group.1,col=c("red","Blue"),main=colnames(gdata$Group.1),names.arg=c("N(Not Approved)","Y(Approved)"))

gdata<-aggregate(x= LoanData$LoanAmount, by = list(LoanData$Loan_Status), FUN= mean,na.rm=TRUE)
print(gdata)

barplot(gdata$x,legend = gdata$Group.1,col=c("red","Blue"),main=colnames(gdata$Group.1),names.arg=c("N(Not Approved)","Y(Approved)"))

## Creattion Of New Variable - Fearture Engineering

LoanData$TotalIncome = LoanData$ApplicantIncome + LoanData$CoapplicantIncome
  
gdata<-aggregate(x= LoanData$TotalIncome, by = list(LoanData$Loan_Status), FUN= mean,na.rm=TRUE)
print(gdata)


## Check for outliers in case of continous variable.

barplot(gdata$x,legend = gdata$Group.1,col=c("red","Blue"),main=colnames(gdata$Group.1),names.arg=c("N(Not Approved)","Y(Approved)"))

boxplot(LoanData$ApplicantIncome)
boxplot(LoanData$CoapplicantIncome)
boxplot(LoanData$LoanAmount)
boxplot(LoanData$TotalIncome)

LoanData$Has_Dependent = ifelse(LoanData$Dependents== 0,"No","Yes")

LoanData$LogTotalInc = log10(LoanData$TotalIncome)
boxplot(LoanData$LogTotalInc)


## Spliting Data into Test and Training
require(caTools)

set.seed(151)
sample1=sample.split(LoanData$Loan_Status,SplitRatio = 0.7)
traindata=subset(LoanData,sample1==TRUE)
testdata=subset(LoanData,sample1==FALSE)

## Check the distibution of Dependent variable in Test and Split data , it should be almost same.
prop.table(table(LoanData$Loan_Status))

prop.table(table(traindata$Loan_Status))

prop.table(table(testdata$Loan_Status))

######################safely loading a package###########
a<-require(aod)

#### Building Logistic Regression Model ####
model1<-glm(Loan_Status ~ Married+
              Credit_History+Property_Area+
              LogTotalInc,
            data=traindata,family = binomial(link="logit"))
summary(model1)

anova(model1, test="Chisq")


exp(coef(model1))
exp(cbind(OR = coef(model1), confint(model1)))   ## odds ratios and 95% CI

fitted.testresult2<- predict(model1,newdata=testdata,type="response")
fitted.testresult2<-ifelse(fitted.testresult2 > 0.6,"Y","N")
misclassificationerror2<-mean(fitted.testresult2!=testdata$Loan_Status)
print(paste("Accuracy : ",1-misclassificationerror2))

## Checking Model Performance ####
library(ROCR)

p<- predict(model1,newdata=testdata,type="response")
pr<-prediction(p,testdata$Loan_Status)
prf<-performance(pr,measure = "tpr",x.measure = "fpr")
plot(prf)

auc<-performance(pr,measure="auc")
auc<-auc@y.values[[1]]
auc


## Building a Tree Base model Via Random Forest #####

library(randomForest)
?randomForest
## Calling syntax to build the Random Forest
RF <- randomForest(as.factor(Loan_Status) ~ Married+Credit_History+Property_Area+LogTotalInc+Dependents+Self_Employed+Gender, data = traindata, 
                   ntree=500, mtry = 3, nodesize = 20,
                   importance=TRUE)
print(RF)
str(traindata)
plot(RF, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest Train Data")

traindata$predict.class <- predict(RF, traindata, type="class")
traindata$predict.score <- predict(RF, traindata, type="prob")
head(traindata)


## CHecking model performcae ##

library(ROCR)
pred <- prediction(traindata$predict.score[,2], traindata$Loan_Status)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

library(ineq)
gini = ineq(traindata$predict.score[,2], type="Gini")

with(traindata, table(Loan_Status, predict.class))
auc
KS
gini

## Scoring syntax
testdata$predict.class <- predict(RF, testdata, type="class")
testdata$predict.score <- predict(RF, testdata, type="prob")
head(testdata)
with(testdata, table(Loan_Status, predict.class))
