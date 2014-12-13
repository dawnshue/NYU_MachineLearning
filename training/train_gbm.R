#TRAINING USING GBM::Gradient Boosted Model"
#Video tutorial: http://vimeo.com/71992876

#install.packages("gbm")
library(gbm)

system.time(fit<-gbm.fit(x=datatrain
                         , y=ytrain
                         , distribution="bernoulli"
                         , verbose = TRUE #prints preliminary output
))
#user  system elapsed 
#657.627   3.792 660.302

system.time(fit<-gbm.fit(x=datatrain
                         , y=ytrain
                         , distribution="bernoulli"
                         , n.trees = 100
                         , shrinkage = 1
                         , interaction.depth = 1
                         , n.minobsinnode = 10
                         , keep.data = FALSE
                         #, nTrain = round(dim(datatrain)[1] * 0.8) #data used for training
                         , verbose = FALSE #prints preliminary output
))

summary(fit)

#Obtains optimal number of trees based on cv
system.time(newtrees<-gbm.perf(fit))
#newtrees = 100
#user  system elapsed 
#0.686   1.955   1.297

system.time(predgbm<-predict.gbm(object=fit
                              , newdata=datatest[smalltest,]
                              , n.trees=newtrees
                              , type="response"))

predgbm[which(predgbm>=0.9)]<-1
predgbm[which(predgbm<0.9)]<-0
table(predgbm, ytest[smalltest], dnn=list('predicted','actual'))
accuracy<-1-(sum(abs(as.numeric(as.character(ytest[smalltest])) - 
                       as.numeric(as.character(predgbm))))/length(ytest[smalltest]))
#test = 300, accuracy: 0.79

