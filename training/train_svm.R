# TRAINING USING SVM::Support Vector Machines"
# Parameters: http://www.inside-r.org/node/57517
# Tutorial: http://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Classification/SVM

# SVM: http://stackoverflow.com/questions/7782501/how-to-interpret-predict-result-of-svm-in-r
# Introduction to statistical learning: http://www-bcf.usc.edu/~gareth/ISL/ISLR%20First%20Printing.pdf
# Predict: http://www.mathworks.com/help/stats/compactclassificationsvm.predict.html


#install.packages("caret")
library(caret)
#install.packages("e1071")
library(e1071)

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