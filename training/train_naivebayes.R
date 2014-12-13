#TRAINING USING Bernoulli Naive Bayes"
setwd('~/MLfinalproject/data/')
load('weighted_smallsub.RData')
ls()

# Parameter info: http://www.inside-r.org/packages/cran/e1071/docs/naiveBayes
# Data streaming: https://github.com/jwijffels/RMOA
# http://www-users.cs.york.ac.uk/~jc/teaching/arin/R_practical/

#Installation for RMOA
#sudo apt-get install texlive-full
install.packages("texlive")
install.packages("devtools")
library(devtools)
install.packages("ff")
install.packages("rJava")
install_github("jwijffels/RMOA", subdir="RMOAjars/pkg")
install_github("jwijffels/RMOA", subdir="RMOA/pkg")


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

#PREDICTING
#Different sizes (datatrain)
for(x in c(1:8)) {
  print(paste0('System.time(prednb[[',x,']]<-predict(object=fitnb[[3]], newdata=datatest[small[[',x,']],], type="class", threshold=0.05))'
               ))
  flush.console()
}

prednb<-list()
system.time(prednb[[1]]<-predict(object=fitnb[[3]], newdata=datatest[small[[1]],], type="class", threshold=0.05))
system.time(prednb[[2]]<-predict(object=fitnb[[3]], newdata=datatest[small[[2]],], type="class", threshold=0.05))
system.time(prednb[[3]]<-predict(object=fitnb[[3]], newdata=datatest[small[[3]],], type="class", threshold=0.05))

system.time(prednb[[4]]<-predict(object=fitnb[[3]], newdata=datatest[small[[4]],], type="class", threshold=0.05))
system.time(prednb[[5]]<-predict(object=fitnb[[3]], newdata=datatest[small[[5]],], type="class", threshold=0.05))
system.time(prednb[[6]]<-predict(object=fitnb[[3]], newdata=datatest[small[[6]],], type="class", threshold=0.05))
system.time(prednb[[7]]<-predict(object=fitnb[[3]], newdata=datatest[small[[7]],], type="class", threshold=0.05))
system.time(prednb[[8]]<-predict(object=fitnb[[3]], newdata=datatest[small[[8]],], type="class", threshold=0.05))

system.time(prednb[[9]]<-predict(object=fitnb[[2]], newdata=datatest[small[[4]],], type="class", threshold=0.05))
system.time(prednb[[10]]<-predict(object=fitnb[[2]], newdata=datatest[small[[5]],], type="class", threshold=0.05))
system.time(prednb[[11]]<-predict(object=fitnb[[2]], newdata=datatest[small[[6]],], type="class", threshold=0.05))
system.time(prednb[[12]]<-predict(object=fitnb[[2]], newdata=datatest[small[[7]],], type="class", threshold=0.05))
system.time(prednb[[13]]<-predict(object=fitnb[[2]], newdata=datatest[small[[8]],], type="class", threshold=0.05))

system.time(prednb[[14]]<-predict(object=fitnb[[1]], newdata=datatest[small[[4]],], type="class", threshold=0.05))
system.time(prednb[[15]]<-predict(object=fitnb[[1]], newdata=datatest[small[[5]],], type="class", threshold=0.05))
system.time(prednb[[16]]<-predict(object=fitnb[[1]], newdata=datatest[small[[6]],], type="class", threshold=0.05))
system.time(prednb[[17]]<-predict(object=fitnb[[1]], newdata=datatest[small[[7]],], type="class", threshold=0.05))
system.time(prednb[[18]]<-predict(object=fitnb[[1]], newdata=datatest[small[[8]],], type="class", threshold=0.05))


#small100
#   user  system elapsed 
#31.448   0.362  31.497
#small250

#small1000

#small500

#small2

#small3

#small4

#small5


for(p in prednb) {
  
}
table(prednb100, ytest[small100], dnn=list('predicted','actual'))

1-(sum(abs(as.numeric(as.character(ytest[small100])) - as.numeric(as.character(prednb100))))/length(ytest[small100]))





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