# TRAINING USING SVM::Support Vector Machines"
rm(list=ls()) #clears workspace
setwd('~/MLfinalproject/data/')
load('weighted_smallsub.RData')
ls()

# Parameters: http://www.inside-r.org/node/57517
# Tutorial: http://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Classification/SVM

# SVM: http://stackoverflow.com/questions/7782501/how-to-interpret-predict-result-of-svm-in-r
# Introduction to statistical learning: http://www-bcf.usc.edu/~gareth/ISL/ISLR%20First%20Printing.pdf
# Predict: http://www.mathworks.com/help/stats/compactclassificationsvm.predict.html


#install.packages("caret")
library(caret)
#install.packages("e1071")
library(e1071)

##### PERFORM FIT
fitsvm<-list()
system.time(fitsvm[[1]]<-svm(x=datatrain[train0,], y=as.factor(ytrain[train0])))
#   user  system elapsed 
#436.036   7.616 443.664
save.image("weighted_svm.RData")
system.time(fitsvm[[2]]<-svm(x=datatrain[train1,], y=as.factor(ytrain[train1])))
#user  system elapsed 
#1531.416   21.651 1553.130
save.image("weighted_svm.RData")
#SVM for large data
#http://lmb.informatik.uni-freiburg.de/papers/download/fe_za_bu_GFKL07.pdf
system.time(fitsvm[[3]]<-svm(x=datatrain, y=as.factor(ytrain)))
#user  system elapsed 
#6470.774   57.854 6528.991
save.image("weighted_svm.RData")

##### PERFORM PREDICT
for(x in c(1:8)) {
  cat(paste0('system.time(predsvm[['
             ,x
             ,']]<-predict(object=fitsvm[[3]], newdata=datatest[small[['
             ,x
             ,']],], type="class"))'
             ,'\n'
  ))
  flush.console()
}
for(x in seq(4,8,1)) {
  cat(paste0('system.time(predsvm[['
             ,x+5
             ,']]<-predict(object=fitsvm[[2]], newdata=datatest[small[['
             ,x
             ,']],], type="class"))'
             ,'\n'
  ))
  flush.console()
}
for(x in seq(4,8,1)) {
  cat(paste0('system.time(predsvm[['
             ,x+10
             ,']]<-predict(object=fitsvm[[1]], newdata=datatest[small[['
             ,x
             ,']],], type="class"))'
             ,'\n'
  ))
  flush.console()
}

predsvm<-list()
system.time(predsvm[[1]]<-predict(object=fitsvm[[3]], newdata=datatest[small[[1]],], type="class"))
6.115   1.124   7.240
system.time(predsvm[[2]]<-predict(object=fitsvm[[3]], newdata=datatest[small[[2]],], type="class"))
7.448   0.788   8.236
system.time(predsvm[[3]]<-predict(object=fitsvm[[3]], newdata=datatest[small[[3]],], type="class"))
14.865   0.720  15.585
system.time(predsvm[[4]]<-predict(object=fitsvm[[3]], newdata=datatest[small[[4]],], type="class"))
9.808   0.832  10.640
system.time(predsvm[[5]]<-predict(object=fitsvm[[3]], newdata=datatest[small[[5]],], type="class"))
9.925   0.516  10.442
system.time(predsvm[[6]]<-predict(object=fitsvm[[3]], newdata=datatest[small[[6]],], type="class"))
9.868   0.724  10.592
system.time(predsvm[[7]]<-predict(object=fitsvm[[3]], newdata=datatest[small[[7]],], type="class"))
9.740   0.508  10.248
system.time(predsvm[[8]]<-predict(object=fitsvm[[3]], newdata=datatest[small[[8]],], type="class"))
9.732   0.720  10.452
system.time(predsvm[[9]]<-predict(object=fitsvm[[2]], newdata=datatest[small[[4]],], type="class"))
4.909   0.164   5.073
system.time(predsvm[[10]]<-predict(object=fitsvm[[2]], newdata=datatest[small[[5]],], type="class"))
4.924   0.012   4.936
system.time(predsvm[[11]]<-predict(object=fitsvm[[2]], newdata=datatest[small[[6]],], type="class"))
4.940   0.332   5.273
system.time(predsvm[[12]]<-predict(object=fitsvm[[2]], newdata=datatest[small[[7]],], type="class"))
4.898   0.104   5.002
system.time(predsvm[[13]]<-predict(object=fitsvm[[2]], newdata=datatest[small[[8]],], type="class"))
4.816   0.000   4.816

system.time(predsvm[[14]]<-predict(object=fitsvm[[1]], newdata=datatest[small[[4]],], type="class"))
2.952   0.004   2.956
system.time(predsvm[[15]]<-predict(object=fitsvm[[1]], newdata=datatest[small[[5]],], type="class"))
2.969   0.000   2.970
system.time(predsvm[[16]]<-predict(object=fitsvm[[1]], newdata=datatest[small[[6]],], type="class"))
2.949   0.004   2.954
system.time(predsvm[[17]]<-predict(object=fitsvm[[1]], newdata=datatest[small[[7]],], type="class"))
2.923   0.072   2.995
system.time(predsvm[[18]]<-predict(object=fitsvm[[1]], newdata=datatest[small[[8]],], type="class"))
2.918   0.076   2.994

table(pred, ytest, dnn=list('predicted','actual'))
accuracy<-1-(sum(abs(as.numeric(as.character(ytest[smalltest])) - 
                       as.numeric(as.character(pred))))/length(ytest[smalltest]))
##########################################
system.time(fit<-svm(x=datatrain
         , y=as.factor(ytrain)
         , scale=TRUE
         , type = NULL
         , kernel = "radial"
         , degree = 3
         , gamma = if(is.vector(datatrain)) 1 else 1/ncol(datatrain)
         , coef0 = 0
         , cost = 1
         , nu = 0.5
         , class.weights = NULL
         , cachesize = 40
         , tolerance = 0.001
         , epsilon = 0.1
         , shrinking = TRUE
         , cross = 0
         , probability = FALSE
         , fitted = TRUE
         , seed = 1L
         ))
#Defaults
#    user   system  elapsed 
#1323.354    5.066 1327.324 


system.time(pred<-predict(fit
                          , newdata=datatest))
table(pred, ytest, dnn=list('predicted','actual'))
#smalltest=300, actual
#predicted   0   1
#        0 232  28
#        1   5  35

accuracy<-1-(sum(abs(as.numeric(as.character(ytest[smalltest])) - 
                       as.numeric(as.character(pred))))/length(ytest[smalltest]))
#300: accuracy: 0.89

#TUNE
#http://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Classification/SVM
tuned <- tune.svm(Sex~., data = trainset, gamma = 10^(-6:-1), cost = 10^(1:2))
summary(tuned)