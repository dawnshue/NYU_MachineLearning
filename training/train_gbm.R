#TRAINING USING GBM::Gradient Boosted Model"
#Video tutorial: http://vimeo.com/71992876

#install.packages("gbm")
library(gbm)

fit<-gbm.fit(x=traindata
             , y=y
             , distribution="bernoulli"
             , n.trees = 1000
             , shrinkage = 0.1
             , interaction.depth = 3
             , n.minobsinnode = 10
             , nTrain = round(dim(traindata)[1] * 0.8)
             , verbose = TRUE #prints preliminary output
             )

summary(fit)
newtrees<-gbm.perf(fit) #optimal number of trees based on cv

pred<-predict.gbm(object=fit
        , newdata=testdata
        , n.trees=newtrees
        , type="response")
