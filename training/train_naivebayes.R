#TRAINING USING Bernoulli Naive Bayes"
rm(list=ls()) #clears workspace
setwd('~/MLfinalproject/data/')
load('weighted_smallsub.RData')
ls()

#install.packages("e1071")
library(e1071)

#DEFAULT PARAMS
#FITTING
fitnb<-list()
#datatrain0
system.time(fitnb[[1]]<-naiveBayes(x=datatrain[train0,], y=as.factor(ytrain[train0])))
#82.248   4.092  86.214
#75.858   1.781  77.641 
save.image("weighted_nb2.RData")
system.time(fitnb[[2]]<-naiveBayes(x=datatrain[train1,], y=as.factor(ytrain[train1])))
#167.030   4.481 171.384
#146.080   3.039 149.123
save.image("weighted_nb2.RData")
system.time(fitnb[[3]]<-naiveBayes(x=datatrain, y=as.factor(ytrain)))
#294.883   6.883 301.594
#252.456  12.488 264.952
save.image("weighted_nb2.RData")

sizes<-c(35000, 70000, 142433)
times<-c(82.248, 167.030, 294.883)
plot(x=sizes,y=times)

##### PERFORM PREDICT
for(x in c(1:8)) {
  cat(paste0('system.time(prednb[['
             ,x
             ,']]<-predict(object=fitnb[[3]], newdata=datatest[small[['
             ,x
             ,']],], type="raw", threshold=0.001))'
             ,'\n'
  ))
  flush.console()
}
for(x in seq(4,8,1)) {
  cat(paste0('system.time(prednb[['
             ,x+5
             ,']]<-predict(object=fitnb[[2]], newdata=datatest[small[['
             ,x
             ,']],], type="raw", threshold=0.001))'
             ,'\n'
  ))
  flush.console()
}
for(x in seq(4,8,1)) {
  cat(paste0('system.time(prednb[['
             ,x+10
             ,']]<-predict(object=fitnb[[1]], newdata=datatest[small[['
             ,x
             ,']],], type="raw", threshold=0.001))'
             ,'\n'
  ))
  flush.console()
}


#PREDICTING
prednb<-list()
system.time(prednb[[1]]<-predict(object=fitnb[[3]], newdata=datatest[small[[1]],], type="class", threshold=0.001))
#33.053   0.542  33.270
system.time(prednb[[2]]<-predict(object=fitnb[[3]], newdata=datatest[small[[2]],], type="class", threshold=0.001))
#82.095   1.079  82.390
system.time(prednb[[3]]<-predict(object=fitnb[[3]], newdata=datatest[small[[3]],], type="class", threshold=0.001))
#335.145   3.435 335.402

system.time(prednb[[4]]<-predict(object=fitnb[[3]], newdata=datatest[small[[4]],], type="class", threshold=0.001))
#168.915   1.735 169.046
system.time(prednb[[5]]<-predict(object=fitnb[[3]], newdata=datatest[small[[5]],], type="class", threshold=0.001))
#165.378   1.698 165.510
system.time(prednb[[6]]<-predict(object=fitnb[[3]], newdata=datatest[small[[6]],], type="class", threshold=0.001))
#167.071   2.023 167.487
system.time(prednb[[7]]<-predict(object=fitnb[[3]], newdata=datatest[small[[7]],], type="class", threshold=0.001))
#166.828   1.925 167.161
system.time(prednb[[8]]<-predict(object=fitnb[[3]], newdata=datatest[small[[8]],], type="class", threshold=0.001))
#165.426   1.784 165.618

system.time(prednb[[9]]<-predict(object=fitnb[[2]], newdata=datatest[small[[4]],], type="class", threshold=0.001))
#163.682   1.955 164.071
system.time(prednb[[10]]<-predict(object=fitnb[[2]], newdata=datatest[small[[5]],], type="class", threshold=0.001))
#164.012   1.657 164.100
system.time(prednb[[11]]<-predict(object=fitnb[[2]], newdata=datatest[small[[6]],], type="class", threshold=0.001))
#162.962   1.738 163.117
system.time(prednb[[12]]<-predict(object=fitnb[[2]], newdata=datatest[small[[7]],], type="class", threshold=0.001))
#164.526   1.574 164.514
system.time(prednb[[13]]<-predict(object=fitnb[[2]], newdata=datatest[small[[8]],], type="class", threshold=0.001))
#163.231   1.615 163.257

system.time(prednb[[14]]<-predict(object=fitnb[[1]], newdata=datatest[small[[4]],], type="class", threshold=0.001))
#164.975   1.637 165.013
system.time(prednb[[15]]<-predict(object=fitnb[[1]], newdata=datatest[small[[5]],], type="class", threshold=0.001))
#164.854   1.755 165.011
system.time(prednb[[16]]<-predict(object=fitnb[[1]], newdata=datatest[small[[6]],], type="class", threshold=0.001))
#166.835   1.813 167.056
system.time(prednb[[17]]<-predict(object=fitnb[[1]], newdata=datatest[small[[7]],], type="class", threshold=0.001))
#165.561   1.881 165.864
system.time(prednb[[18]]<-predict(object=fitnb[[1]], newdata=datatest[small[[8]],], type="class", threshold=0.001))
#165.310   1.431 165.159

save(pdata, pevents, prednb, ptest, small, ytest, file='weighted_nb2.RData')

#########################
setwd('~/TESTWORK/MLfinalproject/data/')
load('weighted_nb.RData')
predtimes<-c(33.053, 82.095, 168.915, 335.145)
samples<-c(100,250,500,1000)
plot(x=sample,y=ptimes/60,type="l"
     ,xlab="# test cases"
     ,ylab="time (min)")
#Less Data
#table(prednb[[4:8]], ytest[small[[4:8]]], dnn=list('predicted','actual'))
#table(prednb[[9:13]], ytest[small[[4:8]]], dnn=list('predicted','actual'))
#table(prednb[[14:18]], ytest[small[[8]]], dnn=list('predicted','actual'))[2,1]
#More Data
nb_acc<-c(0,0,0)
nb_fp<-c(0,0,0)
nb_fn<-c(0,0,0)
nb_prec<-c(0,0,0)
nb_rec<-c(0,0,0)
for(x in seq(4,8,1)) {
  nb_acc[3]<-nb_acc[3]+1-(sum(abs(as.numeric(as.character(ytest[small[[x]]])) - as.numeric(as.character(prednb[[x]]))))/length(ytest[small[[x]]]))
  nb_fp[3]<-nb_fp[3]+
    table(prednb[[x]], ytest[small[[x]]], dnn=list('predicted','actual'))[2,1]/length(ytest[small[[x]]])
  nb_fn[3]<-nb_fn[3]+
    table(prednb[[x]], ytest[small[[x]]], dnn=list('predicted','actual'))[1,2]/length(ytest[small[[x]]])
  tp<-table(prednb[[x]], ytest[small[[x]]], dnn=list('predicted','actual'))[2,2]
  fp<-table(prednb[[x]], ytest[small[[x]]], dnn=list('predicted','actual'))[2,1]
  nb_prec[3]<-nb_prec[3]+tp/(fp+tp)
  tt<-table(prednb[[x]], ytest[small[[x]]], dnn=list('predicted','actual'))[1,2]+
    table(prednb[[x]], ytest[small[[x]]], dnn=list('predicted','actual'))[2,2]
  nb_rec[3]<-nb_rec[3]+tp/tt
  
  nb_acc[2]<-nb_acc[2]+
    1-(sum(abs(as.numeric(as.character(ytest[small[[x]]])) - as.numeric(as.character(prednb[[x+5]]))))/length(ytest[small[[x]]]))
  nb_fp[2]<-nb_fp[2]+
    table(prednb[[x+5]], ytest[small[[x]]], dnn=list('predicted','actual'))[2,1]/length(ytest[small[[x]]])
  nb_fn[2]<-nb_fn[2]+
    table(prednb[[x+5]], ytest[small[[x]]], dnn=list('predicted','actual'))[1,2]/length(ytest[small[[x]]])
  tp<-table(prednb[[x+5]], ytest[small[[x]]], dnn=list('predicted','actual'))[2,2]
  fp<-table(prednb[[x+5]], ytest[small[[x]]], dnn=list('predicted','actual'))[2,1]
  nb_prec[2]<-nb_prec[2]+tp/(fp+tp)
  tt<-table(prednb[[x+5]], ytest[small[[x]]], dnn=list('predicted','actual'))[1,2]+
    table(prednb[[x+5]], ytest[small[[x]]], dnn=list('predicted','actual'))[2,2]
  nb_rec[2]<-nb_rec[2]+tp/tt
  
  nb_acc[1]<-nb_acc[1]+
    1-(sum(abs(as.numeric(as.character(ytest[small[[x]]])) - as.numeric(as.character(prednb[[x+10]]))))/length(ytest[small[[x]]]))
  nb_fp[1]<-nb_fp[1]+
    table(prednb[[x+10]], ytest[small[[x]]], dnn=list('predicted','actual'))[2,1]/length(ytest[small[[x]]])
  nb_fn[1]<-nb_fn[1]+
    table(prednb[[x+10]], ytest[small[[x]]], dnn=list('predicted','actual'))[1,2]/length(ytest[small[[x]]])
  tp<-table(prednb[[x+10]], ytest[small[[x]]], dnn=list('predicted','actual'))[2,2]
  fp<-table(prednb[[x+10]], ytest[small[[x]]], dnn=list('predicted','actual'))[2,1]
  nb_prec[1]<-nb_prec[1]+tp/(fp+tp)
  tt<-table(prednb[[x+10]], ytest[small[[x]]], dnn=list('predicted','actual'))[1,2]+
    table(prednb[[x+10]], ytest[small[[x]]], dnn=list('predicted','actual'))[2,2]
  nb_rec[1]<-nb_rec[1]+tp/tt
}
nb_acc<-nb_acc/5
nb_fp<-nb_fp/5
nb_fn<-nb_fn/5
nb_prec<-nb_prec/5
nb_rec<-nb_rec/5
nb_acc
nb_fp
nb_fn
nb_prec
nb_rec

ROCsdat <- data.frame(cutpoint = c(-Inf, 5, 7, 9, Inf), TPR = c(0, 0.56, 0.78, 0.91, 1), FPR = c(0, 0.01, 0.19, 0.58, 1)) 





#TUNING
system.time(fit<-naiveBayes(x=datatrain, y=as.factor(ytrain)
                            #, laplace = 0 #0 disables laplace smoothing
))

# Data streaming: https://github.com/jwijffels/RMOA
# http://www-users.cs.york.ac.uk/~jc/teaching/arin/R_practical/