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
#datatrain0
system.time(fitnb0<-naiveBayes(x=datatrain[train0,], y=as.factor(ytrain[train0])))
#user  system elapsed
#82.248   4.092  86.214
#datatrain1
system.time(fitnb1<-naiveBayes(x=datatrain[train1,], y=as.factor(ytrain[train1])))
#user  system elapsed 
#167.030   4.481 171.384
#datatrain
system.time(fitnb<-naiveBayes(x=datatrain, y=as.factor(ytrain)))


#PREDICTING
#Different sizes (datatrain)
#small100
system.time(prednb100<-predict(object=fitnb, newdata=datatest[small100,], type="class", threshold=0.05))

table(prednb100, ytest[small100], dnn=list('predicted','actual'))

1-(sum(abs(as.numeric(as.character(ytest[small100])) - as.numeric(as.character(prednb100))))/length(ytest[small100]))

#small250
system.time(prednb250<-predict(object=fitnb, newdata=datatest[small250,], type="class", threshold=0.05))

table(prednb250, ytest[small250], dnn=list('predicted','actual'))

1-(sum(abs(as.numeric(as.character(ytest[small250])) - as.numeric(as.character(prednb250))))/length(ytest[small250]))

#small500
system.time(prednb500<-predict(object=fitnb, newdata=datatest[small500,], type="class", threshold=0.05))

table(prednb100, ytest[small100], dnn=list('predicted','actual'))

1-(sum(abs(as.numeric(as.character(ytest[small100])) - as.numeric(as.character(prednb100))))/length(ytest[small100]))

#small1000
system.time(prednb1000<-predict(object=fitnb, newdata=datatest[small1000,], type="class", threshold=0.05))


#Different model sizes
system.time(prednb500a<-predict(object=fitnb, newdata=datatest[small500,], type="class", threshold=0.05))


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