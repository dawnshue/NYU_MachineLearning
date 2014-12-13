#TRAINING USING GBM::Gradient Boosted Model"
setwd('~/MLfinalproject/data/')
load('weighted_smallsub.RData')
ls()

#install.packages("gbm")
library(gbm)

#Video tutorial: http://vimeo.com/71992876
#GENERATING MODEL
fitgbm<-list()
system.time(fitgbm[[1]]<-gbm.fit(x=datatrain[train0,], y=ytrain[train0], distribution="bernoulli", verbose = FALSE))
#
save.image("weighted_gbm.RData")
system.time(fitgbm[[2]]<-gbm.fit(x=datatrain[train1,], y=ytrain[train1], distribution="bernoulli", verbose = FALSE))
#
save.image("weighted_gbm.RData")
system.time(fitgbm[[3]]<-gbm.fit(x=datatrain, y=ytrain, distribution="bernoulli", verbose = FALSE))
#
save.image("weighted_gbm.RData")

##### PERFORM PREDICT
predglm<-list()
for(x in c(1:8)) {
  cat(paste0('system.time(predglm[['
             ,x
             ,']]<-predict(object=fitglm[[3]], newx=datatest[small[['
             ,x
             ,']],], type="response", s=0.05))'
             ,'\n'
  ))
  flush.console()
}
for(x in seq(4,8,1)) {
  cat(paste0('system.time(predglm[['
             ,x+5
             ,']]<-predict(object=fitglm[[3]], newx=datatest[small[['
             ,x
             ,']],], type="response", s=0.05))'
             ,'\n'
  ))
  flush.console()
}
for(x in seq(4,8,1)) {
  cat(paste0('system.time(predglm[['
             ,x+10
             ,']]<-predict(object=fitglm[[3]], newx=datatest[small[['
             ,x
             ,']],], type="response", s=0.05))'
             ,'\n'
  ))
  flush.console()
}




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

