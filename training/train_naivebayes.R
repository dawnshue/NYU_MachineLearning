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
#user  system elapsed
#82.248   4.092  86.214
#datatrain1
system.time(fitnb[[2]]<-naiveBayes(x=datatrain[train1,], y=as.factor(ytrain[train1])))
#user  system elapsed 
#167.030   4.481 171.384
#datatrain
system.time(fitnb[[3]]<-naiveBayes(x=datatrain, y=as.factor(ytrain)))
#user  system elapsed 
#294.883   6.883 301.594
save.image("weighted_nb.RData")

sizes<-c(35000, 70000, 142433)
times<-c(82.248, 167.030, 294.883)
plot(x=sizes,y=times)


#PREDICTING
#Different sizes (datatrain)
for(x in c(1:8)) {
  print(paste0('System.time(prednb[[',x,']]<-predict(object=fitnb[[3]], newdata=datatest[small[[',x,']],], type="class", threshold=0.05))'
               ))
  flush.console()
}

prednb<-list()
system.time(prednb[[1]]<-predict(object=fitnb[[3]], newdata=datatest[small[[1]],], type="class", threshold=0.05))
#33.053   0.542  33.270
system.time(prednb[[2]]<-predict(object=fitnb[[3]], newdata=datatest[small[[2]],], type="class", threshold=0.05))
#82.095   1.079  82.390
system.time(prednb[[3]]<-predict(object=fitnb[[3]], newdata=datatest[small[[3]],], type="class", threshold=0.05))
#335.145   3.435 335.402

predtimes<-c(33.053, 82.095, 168.915, 335.145)
samples<-c(100,250,500,1000)
plot(x=sample,y=ptimes/60,type="l"
     ,xlab="# test cases"
     ,ylab="time (min)")

system.time(prednb[[4]]<-predict(object=fitnb[[3]], newdata=datatest[small[[4]],], type="class", threshold=0.05))
#168.915   1.735 169.046
system.time(prednb[[5]]<-predict(object=fitnb[[3]], newdata=datatest[small[[5]],], type="class", threshold=0.05))
#165.378   1.698 165.510
system.time(prednb[[6]]<-predict(object=fitnb[[3]], newdata=datatest[small[[6]],], type="class", threshold=0.05))
#167.071   2.023 167.487
system.time(prednb[[7]]<-predict(object=fitnb[[3]], newdata=datatest[small[[7]],], type="class", threshold=0.05))
#166.828   1.925 167.161
system.time(prednb[[8]]<-predict(object=fitnb[[3]], newdata=datatest[small[[8]],], type="class", threshold=0.05))
#165.426   1.784 165.618

system.time(prednb[[9]]<-predict(object=fitnb[[2]], newdata=datatest[small[[4]],], type="class", threshold=0.05))
#163.682   1.955 164.071
system.time(prednb[[10]]<-predict(object=fitnb[[2]], newdata=datatest[small[[5]],], type="class", threshold=0.05))
#164.012   1.657 164.100
system.time(prednb[[11]]<-predict(object=fitnb[[2]], newdata=datatest[small[[6]],], type="class", threshold=0.05))
#162.962   1.738 163.117
system.time(prednb[[12]]<-predict(object=fitnb[[2]], newdata=datatest[small[[7]],], type="class", threshold=0.05))
#164.526   1.574 164.514
system.time(prednb[[13]]<-predict(object=fitnb[[2]], newdata=datatest[small[[8]],], type="class", threshold=0.05))
#163.231   1.615 163.257

system.time(prednb[[14]]<-predict(object=fitnb[[1]], newdata=datatest[small[[4]],], type="class", threshold=0.05))
#164.975   1.637 165.013
system.time(prednb[[15]]<-predict(object=fitnb[[1]], newdata=datatest[small[[5]],], type="class", threshold=0.05))
#164.854   1.755 165.011
system.time(prednb[[16]]<-predict(object=fitnb[[1]], newdata=datatest[small[[6]],], type="class", threshold=0.05))
#166.835   1.813 167.056
system.time(prednb[[17]]<-predict(object=fitnb[[1]], newdata=datatest[small[[7]],], type="class", threshold=0.05))
#165.561   1.881 165.864
system.time(prednb[[18]]<-predict(object=fitnb[[1]], newdata=datatest[small[[8]],], type="class", threshold=0.05))
#165.310   1.431 165.159

rm(datatest, datatrain, fitnb, x)
save.image("weighted_nb.RData")

#########################
setwd('~/_Main/School/CafeMom/MLfinalproject/data/')
load('weighted_nb.RData')

#Less Data
table(prednb[[4:8]], ytest[small[[4:8]]], dnn=list('predicted','actual'))
table(prednb[[9:13]], ytest[small[[4:8]]], dnn=list('predicted','actual'))
table(prednb[[18]], ytest[small[[8]]], dnn=list('predicted','actual'))[2,1]
#More Data
nb_acc<-c(0,0,0)
nb_fp<-c(0,0,0)
nb_fn<-c(0,0,0)
for(x in seq(4,8,1)) {
  nb_acc[1]<-nb_acc[1]+
    1-(sum(abs(as.numeric(as.character(ytest[small[[x]]])) - as.numeric(as.character(prednb[[x]]))))/length(ytest[small[[x]]]))
  nb_fp[1]<-nb_fp[1]+
    table(prednb[[x]], ytest[small[[x]]], dnn=list('predicted','actual'))[2,1]/length(ytest[small[[x]]])
  nb_fn[1]<-nb_fn[1]+
    table(prednb[[x]], ytest[small[[x]]], dnn=list('predicted','actual'))[1,2]/length(ytest[small[[x]]])
  nb_acc[2]<-nb_acc[2]+
    1-(sum(abs(as.numeric(as.character(ytest[small[[x]]])) - as.numeric(as.character(prednb[[x+5]]))))/length(ytest[small[[x]]]))
  nb_fp[2]<-nb_fp[2]+
    table(prednb[[x+5]], ytest[small[[x]]], dnn=list('predicted','actual'))[2,1]/length(ytest[small[[x]]])
  nb_fn[2]<-nb_fn[2]+
    table(prednb[[x+5]], ytest[small[[x]]], dnn=list('predicted','actual'))[1,2]/length(ytest[small[[x]]])
  nb_acc[3]<-nb_acc[3]+
    1-(sum(abs(as.numeric(as.character(ytest[small[[x]]])) - as.numeric(as.character(prednb[[x+10]]))))/length(ytest[small[[x]]]))
  nb_fp[3]<-nb_fp[3]+
    table(prednb[[x+10]], ytest[small[[x]]], dnn=list('predicted','actual'))[2,1]/length(ytest[small[[x]]])
  nb_fn[3]<-nb_fn[3]+
    table(prednb[[x+10]], ytest[small[[x]]], dnn=list('predicted','actual'))[1,2]/length(ytest[small[[x]]])
}
nb_acc<-nb_acc/5
nb_fp<-nb_fp/5
nb_fn<-nb_fn/5
nb_acc
nb_fp
nb_fn


ROCsdat <- data.frame(cutpoint = c(-Inf, 5, 7, 9, Inf), TPR = c(0, 0.56, 0.78, 0.91, 1), FPR = c(0, 0.01, 0.19, 0.58, 1)) 

1-(sum(abs(as.numeric(as.character(ytest[small100])) - as.numeric(as.character(prednb100))))/length(ytest[small100]))


#TUNING:
#Test Laplace: increases shrinkage
system.time(fitnb[[4]]<-naiveBayes(x=datatrain[train0,]
                                   , y=as.factor(ytrain[train0])
                                   , laplace=1))

summary(fit)

system.time(pred<-predict(object=fit
                              , newdata=datatest[smalltest]
                              , type="class"
                          ))
#test = 300
#user  system elapsed 
#68.671   0.554  68.557

table(pred, ytest[smalltest], dnn=list('predicted','actual'))
actual
#test = 300
#         actual
#predicted   0   1
#         0 236  54
#         1   1   9

accuracy<-1-(sum(abs(as.numeric(as.character(ytest[smalltest])) - 
                       as.numeric(as.character(pred))))/length(ytest[smalltest]))
#test = 300, accuracy = 0.8166667



#TUNING
system.time(fit<-naiveBayes(x=datatrain, y=as.factor(ytrain)
                            #, laplace = 0 #0 disables laplace smoothing
))

# Data streaming: https://github.com/jwijffels/RMOA
# http://www-users.cs.york.ac.uk/~jc/teaching/arin/R_practical/