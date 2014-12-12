#TRAINING USING GBM::Gradient Boosted Model"
#Video tutorial: http://vimeo.com/71992876

#install.packages("gbm")
library(gbm)

system.time(fit<-gbm.fit(x=datatrain
                         , y=ytrain
                         , distribution="bernoulli"
                         , verbose = TRUE #prints preliminary output
))

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

system.time(pred<-predict.gbm(object=fit
                              , newdata=testdata
                              , n.trees=newtrees
                              , type="response"))