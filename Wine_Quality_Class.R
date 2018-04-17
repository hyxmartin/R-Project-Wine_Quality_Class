# -------------------------------
# Author: Yuxiang Hu
# Date: 5/17/2017
# -------------------------------

library(e1071)
library(stringr)
library(ROCR)
library(class)
library(ggplot2)

#utility function for import from csv file
import.csv <- function(filename){
  return(read.csv(filename,sep=",",header=TRUE))
}

#utility function for export to csv file
write.csv <- function(ob,filename){
  write.table(ob,filename,quote=FALSE,sep=",",row.names=FALSE)
}

#print contigency table
print_cont_table<-function(pred,cutoff=0.5){
  print(pred)
  pred.bi<-ifelse(pred[,1]>cutoff,1,0)
  print(pred.bi)
  pred.bi.df<-data.frame(true=pred[,2],pred=pred.bi)
  print(pred.bi.df)
  tbl<-table(pred.bi.df)
  print(tbl)
  n11=ifelse(inherits(tryCatch(tbl['1','1'], error=function(e) e), "error"),0,tbl['1','1']) 
  n10=ifelse(inherits(tryCatch(tbl['1','0'], error=function(e) e), "error"),0,tbl['1','0'])
  n01=ifelse(inherits(tryCatch(tbl['0','1'], error=function(e) e), "error"),0,tbl['0','1'])
  n00=ifelse(inherits(tryCatch(tbl['0','0'], error=function(e) e), "error"),0,tbl['0','0'])
  
  Pos<-n11+n10
  Neg<-n01+n00
  PPos<-n11+n01
  PNeg<-n10+n00
  
  nn<-sum(tbl)
  cat('          | PPos  PNeg  |Sums\n')
  cat('----------------------------\n')
  cat('actual pos|',n11,'\t',n10,'|',Pos,'\n')
  cat('actual neg|',n01,'\t',n00,'|',Neg,'\n')
  cat('----------------------------\n')
  cat('Sums      |',PPos,'\t',PNeg,'|',nn,'\n')
  
  
}


#PART 1

#default regression
get_pred_default<-function(train,test){
  nf<-ncol(train)  
  colnames(train)[nf]<-'output' 
  colnames(test)[nf]<-'output'  # The last column is the output
  pred <- rep(as.numeric(names(which.max(table(train[,nf])))), nrow(test))  # Default is the majority of the ouput
  true_output<-test$output  # True Output
  pred.df<-data.frame(pred,true_output) # return a data frame containing prediction and the true output from the last column
  return(pred.df)
}

#logistic regression
get_pred_logit<-function(train,test){
  nf<-ncol(train)  
  colnames(train)[nf]<-'output' 
  colnames(test)[nf]<-'output'  # The last column is the output
  my.model<-glm(output~.,data=train,family=binomial)  # Use glm to get logistic Regression Model
  pred<-predict(my.model,test,type='response')  # Prediciton
  true_output<-test$output  # True Output
  pred.df<-data.frame(pred,true_output) # return a data frame containing prediction and the true output from the last column
  return(pred.df)
}

#naieve bayes 
get_pred_nb<-function(train,test){
  nf<-ncol(train) 
  colnames(train)[nf]<-'output' 
  colnames(test)[nf]<-'output'  # The last column is the output
  my.model<-naiveBayes(output~.,data=train)  # Use naiveBayes to get naieve bayes Model
  pred<-predict(my.model,test,type='raw')[,'1']  # Prediciton
  true_output<-test$output   # True Output
  pred.df<-data.frame(pred,true_output)
  return(pred.df)  
}


#SVM using default linear kernal
get_pred_svm<-function(train,test){
  nf<-ncol(train) 
  colnames(train)[nf]<-'output' 
  colnames(test)[nf]<-'output'  # The last column is the output
  my.model<-svm(as.factor(output)~.,data=train,probability=TRUE)  # Use svm to get supported vector machine Model
  pred<-attr(predict(my.model,test,probability = TRUE),'prob')[,'1']  # Prediciton 
  true_output<-test$output   # True Output 
  pred.df<-data.frame(pred,true_output)
  return(pred.df)  
}

#KNN
get_pred_knn<-function(train,test,k){
  nf<-ncol(train) 
  colnames(train)[nf]<-'output' 
  colnames(test)[nf]<-'output'  # The last column is the output
  train.data<-train[,-nf] 
  test.data<-test[,-nf]  # knn finds nearest neighbor by looking only at features
  class<-as.factor(train[,nf])
  my.model<-knn(train.data,test.data,class,k=k,prob=TRUE)  # Use knn to get k Nearest Neighbor Model
  prob<-attr(my.model,"prob")  # the proportion of the votes for the winning class
  pred<-ifelse(my.model=='1',prob,1-prob)  # Prediciton  
  true_output<-test$output   # True Output 
  pred.df<-data.frame(pred,true_output)
  return(pred.df)  
}

#PART 2
#your implementation of do_cv_class goes here

get_folds <- function(nn, k) {
  index <- seq(1, nn)
  rand.index <- sample(index, nn)  # randomize
  group <- seq_along(rand.index)%%k  # assign group
  chunk <- split(rand.index, group)  # split by group
  return(chunk)
}

do_cv_class <- function(df, k, model) {
  nn <- nrow(df)
  nf <- ncol(df)
  folds <- get_folds(nn, k)
  for (ii in 1:length(folds)) {
    test.index <- folds[[ii]]
    train.data <- df[-test.index, ]
    test.data <- df[test.index, ]
    if (grepl("nn", model)) {
      k.nn <- as.integer(sub("^\\D*(\\d+).*$", "\\1", model))  # parse the K nearest neighbor from model name
      pred <- get_pred_knn(train.data, test.data, k.nn)  # run with addional parameter for knn model predictor
    } else if (model == "nb") {
      pred <- get_pred_nb(train.data, test.data)      
    } else if (model == "svm") {
      pred <- get_pred_svm(train.data, test.data)      
    } else if (model == "logreg") {
      pred <- get_pred_logit(train.data, test.data)      
    } else if (model == "default") {
      pred <- get_pred_default(train.data, test.data)      
    }
    if (ii == 1) {
      df.output <- data.frame(pred, ii)
    } else {
      df.output <- rbind(df.output, data.frame(pred, ii))
    }
  }
  return(df.output)
}

#PART 3

#input prediction file the first column of which is prediction value
#the 2nd column is true label (0/1)
#cutoff is a numeric value, default is 0.5
get_metrics<-function(pred,cutoff=0.5){
 ### your implementation goes here
  pred$pred <- ifelse(pred$pred >= cutoff, 1, 0)
  tp <- length(with(pred, which(pred == 1 & true_output == 1)))  # True Positive
  fp <- length(with(pred, which(pred == 1 & true_output == 0)))  # False Positive
  tn <- length(with(pred, which(pred == 0 & true_output == 0)))  # True Negative
  fn <- length(with(pred, which(pred == 0 & true_output == 1)))  # False Negative
  tpr <- tp / (tp + fn)
  fpr <- fp / (tn + fp)
  acc <- (tp + tn)/ (tp + tn + fn + fp)
  precision <- tp / (tp + fp)
  recall <- tpr
  df <- data.frame(tpr, fpr, acc, precision, recall)
  return(df)
}

#PART 4

####################
####import data#####
####################

my.data<-import.csv('wine.csv')
#R import symbolic attribute as factor which is integer reprsentation of symbolic values
#as.character will convert factor to string
my.data$type=as.character(my.data$type) 
#encode class into 0/1 for easier handling by classification algorithm in R
my.data$type=ifelse(my.data$type=='high',1,0)


#test cases for do_cv_class and get_metrics functions

# (a)

cutoff <- 0.5  # choose cutoff 0.5
k.nn <- 30  # max knn
for (ii in 1:k.nn) {
  pred <- do_cv_class(my.data, 10, paste(ii, "nn", sep = ""))  # use do_cv_classm, needs to concat number nn: 1nn, 2nn, ..., 12nn
  nn <- nrow(pred)
  pred$pred <- ifelse(pred$pred >= cutoff, 1, 0)
  cv_error <- length(with(pred, which(pred != true_output))) / nn
  if (ii == 1) {
    knn.output <- data.frame(ii, cv_error)
  } else {
    knn.output <- rbind(knn.output, data.frame(ii, cv_error))
  }
}
ggplot(data = knn.output, aes(x= ii , y= cv_error)) + 
  geom_line(colour="red", linetype="dashed", size=1.5) + 
  geom_point(colour="red", size=4, shape=21, fill="white") +
  xlab("k") + ylab("cross validation error") +
  ggtitle("cross validation error for each k used in knn classifier")

cat('--------------------\n')
cat('According to the learning curve,\n')
cat('K = 5 gives the best generation\n')
cat('when K < 5, the classifier is overfitting\n')
cat('when K > 5, the classifier is underfitting\n')
cat('--------------------\n')

# (b)
cat('--------------------\n')
cat('Default classifier\n')
cat('-------------------\n')
tmp<-do_cv_class(my.data, 10, 'default')
# print_cont_table(tmp[,1:2])
print(get_metrics(tmp[,1:2]))

cat('--------------------\n')
cat('logistic regression \n')
cat('-------------------\n')
tmp<-do_cv_class(my.data, 10, 'logreg')
# print_cont_table(tmp[,1:2])
print(get_metrics(tmp[,1:2]))

cat('--------------------\n')
cat('naieve Bayes\n')
cat('--------------------\n')
tmp<-do_cv_class(my.data,10,'nb')
# print_cont_table(tmp[,1:2])
print(get_metrics(tmp[,1:2]))

cat('--------------------\n')
cat('svm\n')
cat('--------------------\n')
tmp<-do_cv_class(my.data,10,'svm')
# print_cont_table(tmp[,1:2])
print(get_metrics(tmp[,1:2]))

cat('--------------------\n')
cat('According to data,\n')
cat('Accuracy in Default classifier is 0.5645412 \n')
cat('Accuracy in Logistric Regression is 0.7947123 \n')
cat('Accuracy in naieve Bayes is 0.8320373 \n')
cat('Accuracy in SVM is 0.8429238 \n')
cat('As a result, SVM generates the best accuracy \n')
cat('--------------------\n')

# (c)
cat('--------------------\n')
cat('knn\n')
cat('--------------------\n')
tmp<-do_cv_class(my.data,10,'5nn')
print_cont_table(tmp[,1:2])
print(get_metrics(tmp[,1:2]))

cat('Accuracy in 5knn is 0.8211509 \n')
cat("Finally, SVM gets the highest accuracy 0.8429238. SVM works the best on this data. \n")

