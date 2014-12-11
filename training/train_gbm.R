#TRAINING USING GBM::Gradient Boosted Model"
#Video tutorial: http://vimeo.com/71992876

#install.packages("glmnet")
fit<-gbm.fit(x=dataset2
             , y=y
             , distribution="bernoulli"
             , n.trees = 1000
             , shrinkage = 0.01
             , interaction.depth = 3
             , n.minobsinnode = 10
             , ntrain = round(end_trn * 0.8)
             , verbose = TRUE)
pred<-predict(object=fit
        , newx=testdataset
        , type="response")
