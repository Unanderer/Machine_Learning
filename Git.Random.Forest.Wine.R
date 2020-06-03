
#############################################################################
#
#   Random Forests
#   01/21/2019
#   Hunter Martin
#
#############################################################################

# removed for privacy reasons
setwd( "PATH/TO/DATA" )
# Reads data
data<-read.csv(file="winequality.csv", header=TRUE, sep=",")
#Extracts ID fields
data<-data[,-1]
# Sets seed
set.seed(123)
install.packages("tree")
install.packages("randomForest")
install.packages("gbm")
installed.packages("Metrics")
library(tree)
#library(ISLR)
library(randomForest) # Random Forests
library(mlbench) # for the dataset
library(gbm) # Boosted trees
library(Metrics)
library(caret)

split<-createDataPartition(data$residual.sugar,p=.5,list = FALSE)

train<-data[split,]
test<-data[-split,]


for(i in 1:12){
  bag.wine=randomForest(residual.sugar~.,mtry=i,train,importance=TRUE)
}

boost_1<-gbm(residual.sugar~.,data=train,distribution="gaussian",n.trees=1300) #Gradient boosted trees, dist=bernoulie for classification
pred.boost<-predict.gbm(boost_1,newdata=test,n.trees=1300)
pred.boost[pred.boost<0]<-0
error.boost<-rmsle(pred.boost,test$residual.sugar)
error.boost

boost_2<-gbm(residual.sugar~.,data=train,distribution="tdist",n.trees=1300) #Gradient boosted trees, dist=bernoulie for classification
pred.boost2<-predict.gbm(boost_1,newdata=test,n.trees=1300)
pred.boost2[pred.boost<0]<-0
error.boost2<-rmsle(pred.boost,test$residual.sugar)
error.boost2


split2<-createDataPartition(data$type,p=.5,list=FALSE)
train2<-data[split2,]
test2<-data[-split2,]

# Transforms outcome into factor, necessary for classification
# or else it will assume it is a regression tree
data_factor=ifelse(data$type==1,"White","Red")
# appends factor to data
data<-data.frame(data,data_factor)

train_type<-createDataPartition(data$type,p=.5,list=FALSE
                                )
# Builds a tree with entire dataset
treedata=tree(data_factor~. -type,data)
summary(treedata)
# plots
plot(treedata)
text(treedata,pretty=0)

#Now we split into training and test
# top half training bottom half test
train=data[1:1473,]
test=data[1474:2946,]

# get the tree based on training set
traintree=tree(Enrolled ~.-Result,train)
# Use it to predict test set
predtree=predict(traintree,test,type="class")
# Confusion table and accuracy
conf<-table(predtree,test$Enrolled)
Accuracy<-(conf[1,1]+conf[2,2])/sum(conf)
Accuracy
