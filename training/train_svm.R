# TRAINING USING SVM::Support Vector Machines"
# SVM: http://stackoverflow.com/questions/7782501/how-to-interpret-predict-result-of-svm-in-r
# Wikibook: http://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Classification/SVM
# http://www.inside-r.org/node/57517
# Introduction to statistical learning: http://www-bcf.usc.edu/~gareth/ISL/ISLR%20First%20Printing.pdf
# Predict: http://www.mathworks.com/help/stats/compactclassificationsvm.predict.html


alldata<-data.frame(cbind(y, traindata))
alldata$y

#install.packages("caret")
library(caret)
#install.packages("e1071")
library(e1071)

fit<-svm(x=traindata
         , y=y
         , gamma=10)
pred<-predict(fit
              , x=testdata)
