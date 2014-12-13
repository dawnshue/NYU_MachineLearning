# TRAINING USING SVM::Support Vector Machines"
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
system.time(fitsvm[[2]]<-svm(x=datatrain[train1,], y=as.factor(ytrain[train1])))
#user  system elapsed 
#1531.416   21.651 1553.130
system.time(fitsvm[[3]]<-svm(x=datatrain, y=as.factor(ytrain)))
#user  system elapsed 
#
save.image("weighted_svm.RData")

##### PERFORM PREDICT
predsvm<-list()
for(x in c(1:8)) {
  cat(paste0('system.time(predsvm[['
             ,x
             ,']]<-predict(object=fitsvm[[3]], newdata=datatest[small[['
             ,x
             ,']],], type="class"))'
  ))
  flush.console()
}
for(x in c(1:8)) {
  system.time(predsvm[[x]]<-predict(object=fitsvm[[3]], newdata=datatest[small[[x]],], type="class", threshold=0.05))
  flush.console()
}




pcnt<-9
for(x in seq(4,8,1)) {
  for(f in c(1:2)) {
    system.time(predsvm[[pcnt]]<-predict(object=fitsvm[[f]]
                                         , newdata=datatest[small[[x]],]
    ))
    pcnt<-pcnt+1
    flush.console()
  }
}


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