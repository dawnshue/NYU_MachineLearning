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

system.time(pred<-predict(fit
              , x=datatest))

table(pred, ytest, dnn=list('predicted','actual'))
accuracy<-1-(sum(abs(as.numeric(as.character(ytest)) - 
                       as.numeric(as.character(pred))))/length(ytest))
