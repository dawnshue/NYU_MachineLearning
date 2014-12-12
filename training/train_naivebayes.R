#TRAINING USING Bernoulli Naive Bayes"
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

system.time(fit<-naiveBayes(x=datatrain
                         , y=as.factor(ytrain)
                         #, laplace = 0 #0 disables laplace smoothing
))

summary(fit)

system.time(pred<-predict(object=fit
                              , newdata=datatest
                              , type="class"
                          ))

table(pred, ytest, dnn=list('predicted','actual'))