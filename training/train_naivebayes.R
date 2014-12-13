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
#p = 0.01
#user     system  elapsed
#76.556   2.358  78.782

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