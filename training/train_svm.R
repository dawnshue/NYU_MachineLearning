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
system.time(fitsvm[[1]]<-svm(x=datatrain[train0,]
                             , y=ytrain[train0]
                             , scale=FALSE
                             , type="C-classification"
                             , kernel="linear"
                             , probability=TRUE))
presvm<-list()
system.time(predsvm[[1]]<-predict(object=fitsvm[[1]]
                                  , newdata=datatest[small[[1]],]
                                  , probability=TRUE
                                  ))
system.time(predsvm[[1]]<-predict(object=fitsvm[[1]]
                                  , newdata=datatest[small[[1]],]
                                  , probability=FALSE
                                  ))

system.time(fitsvm[[2]]<-svm(x=datatrain[train0,]
                             , y=as.factor(ytrain[train0])
                             , scale=FALSE
                             , type="C-classification"
                             , kernel="linear"
                             , probability=FALSE))


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
system.time(predsvm[[1]]<-predict(object=fitsvm[[3]], newdata=datatest[small[[1]],]))
#6.115   1.124   7.240
system.time(predsvm[[2]]<-predict(object=fitsvm[[3]], newdata=datatest[small[[2]],], type="class"))
#7.448   0.788   8.236
system.time(predsvm[[3]]<-predict(object=fitsvm[[3]], newdata=datatest[small[[3]],], type="class"))
#14.865   0.720  15.585
ptimes<-c(6.115, 7.448, 9.808, 14.865)/60
samples<-c(100,250,500,1000)
plot(x=sample,y=ptimes,type="l"
     ,xlab="# test cases"
     ,ylab="time (min)")

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
svm_acc<-c(0,0,0)
svm_fp<-c(0,0,0)
svm_fn<-c(0,0,0)
nb_prec<-c(0,0,0)
nb_rec<-c(0,0,0)
for(x in seq(4,8,1)) {
  svm_acc[3]<-svm_acc[3]+
    1-(sum(abs(ytest[small[[x]]] - as.numeric(as.character(predsvm[[x]]))))/length(ytest[small[[x]]]))
  svm_fp[3]<-svm_fp[3]+
    table(predsvm[[x]], ytest[small[[x]]], dnn=list('predicted','actual'))[2,1]/length(ytest[small[[x]]])
  svm_fn[3]<-svm_fn[3]+
    table(predsvm[[x]], ytest[small[[x]]], dnn=list('predicted','actual'))[1,2]/length(ytest[small[[x]]])
  tp<-table(predsvm[[x]], ytest[small[[x]]], dnn=list('predicted','actual'))[2,2]
  fp<-table(predsvm[[x]], ytest[small[[x]]], dnn=list('predicted','actual'))[2,1]
  nb_prec[3]<-nb_prec[3]+tp/(fp+tp)
  tt<-table(predsvm[[x]], ytest[small[[x]]], dnn=list('predicted','actual'))[1,2]+
    table(predsvm[[x]], ytest[small[[x]]], dnn=list('predicted','actual'))[2,2]
  nb_rec[3]<-nb_rec[3]+tp/tt
  
  svm_acc[2]<-svm_acc[2]+
    1-(sum(abs(as.numeric(as.character(ytest[small[[x]]])) - as.numeric(as.character(predsvm[[x+5]]))))/length(ytest[small[[x]]]))
  svm_fp[2]<-svm_fp[2]+
    table(predsvm[[x+5]], ytest[small[[x]]], dnn=list('predicted','actual'))[2,1]/length(ytest[small[[x]]])
  svm_fn[2]<-svm_fn[2]+
    table(predsvm[[x+5]], ytest[small[[x]]], dnn=list('predicted','actual'))[1,2]/length(ytest[small[[x]]])
  tp<-table(predsvm[[x+5]], ytest[small[[x]]], dnn=list('predicted','actual'))[2,2]
  fp<-table(predsvm[[x+5]], ytest[small[[x]]], dnn=list('predicted','actual'))[2,1]
  nb_prec[2]<-nb_prec[2]+tp/(fp+tp)
  tt<-table(predsvm[[x+5]], ytest[small[[x]]], dnn=list('predicted','actual'))[1,2]+
    table(predsvm[[x+5]], ytest[small[[x]]], dnn=list('predicted','actual'))[2,2]
  nb_rec[2]<-nb_rec[2]+tp/tt
  
  svm_acc[1]<-svm_acc[1]+
    1-(sum(abs(as.numeric(as.character(ytest[small[[x]]])) - as.numeric(as.character(predsvm[[x+10]]))))/length(ytest[small[[x]]]))
  svm_fp[1]<-svm_fp[1]+
    table(predsvm[[x+10]], ytest[small[[x]]], dnn=list('predicted','actual'))[2,1]/length(ytest[small[[x]]])
  svm_fn[1]<-svm_fn[1]+
    table(predsvm[[x+10]], ytest[small[[x]]], dnn=list('predicted','actual'))[1,2]/length(ytest[small[[x]]])
  tp<-table(predsvm[[x+10]], ytest[small[[x]]], dnn=list('predicted','actual'))[2,2]
  fp<-table(predsvm[[x+10]], ytest[small[[x]]], dnn=list('predicted','actual'))[2,1]
  nb_prec[1]<-nb_prec[1]+tp/(fp+tp)
  tt<-table(predsvm[[x+10]], ytest[small[[x]]], dnn=list('predicted','actual'))[1,2]+
    table(predsvm[[x+10]], ytest[small[[x]]], dnn=list('predicted','actual'))[2,2]
  nb_rec[1]<-nb_rec[1]+tp/tt
}
svm_acc<-svm_acc/5
svm_fp<-svm_fp/5
svm_fn<-svm_fn/5
nb_prec<-nb_prec/5
nb_rec<-nb_rec/5
svm_acc
svm_fp
svm_fn
nb_prec
nb_rec

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