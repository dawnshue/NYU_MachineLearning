#TRAINING USING GBM::Gradient Boosted Model"
rm(list=ls()) #clears workspace
setwd('~/MLfinalproject/data/')
load('weighted_smallsub.RData')
ls()

#install.packages("gbm")
library(gbm)

#Video tutorial: http://vimeo.com/71992876
#GENERATING MODEL
fitgbm<-list()
system.time(fitgbm[[1]]<-gbm.fit(x=datatrain[train0,], y=ytrain[train0]
                                 , distribution="bernoulli"
                                 , n.trees = 100
                                 , shrinkage = 1
                                 , interaction.depth = 1
                                 , n.minobsinnode = 10
                                 , keep.data = FALSE
                                 , verbose = FALSE))
#569.411   4.867 573.688
save.image("weighted_gbm.RData")
system.time(fitgbm[[2]]<-gbm.fit(x=datatrain[train1,], y=ytrain[train1]
                                 , n.trees = 100
                                 , shrinkage = 1
                                 , interaction.depth = 1
                                 , n.minobsinnode = 10
                                 , keep.data = FALSE
                                 , distribution="bernoulli", verbose = FALSE))
#1145.877   10.190 1154.894
save.image("weighted_gbm.RData")
system.time(fitgbm[[3]]<-gbm.fit(x=datatrain, y=ytrain
                                 , n.trees = 100
                                 , shrinkage = 1
                                 , interaction.depth = 1
                                 , n.minobsinnode = 10
                                 , keep.data = FALSE
                                 , distribution="bernoulli", verbose = FALSE))
#
save.image("weighted_gbm.RData")

##### PERFORM PREDICT
for(x in c(1:8)) {
  cat(paste0('system.time(predgbm[['
             ,x
             ,']]<-predict.gbm(object=fitgbm[[3]], newdata=datatest[small[['
             ,x
             ,']],], n.trees=newtrees, type="response"))'
             ,'\n'
  ))
  flush.console()
}
for(x in seq(4,8,1)) {
  cat(paste0('system.time(predgbm[['
             ,x+5
             ,']]<-predict.gbm(object=fitgbm[[2]], newdata=datatest[small[['
             ,x
             ,']],], n.trees=newtrees, type="response"))'
             ,'\n'
  ))
  flush.console()
}
for(x in seq(4,8,1)) {
  cat(paste0('system.time(predgbm[['
             ,x+10
             ,']]<-predict.gbm(object=fitgbm[[1]], newdata=datatest[small[['
             ,x
             ,']],], n.trees=newtrees, type="response"))'
             ,'\n'
  ))
  flush.console()
}

predgbm<-list()
system.time(newtrees<-gbm.perf(fitgbm[[1]], plot.it=FALSE, oobag.curve=FALSE
                               ,overlay=FALSE, method="cv"))
newtrees<-10
system.time(predgbm[[1]]<-predict.gbm(object=fitgbm[[2]], newdata=datatest[small[[1]],], n.trees=newtrees, type="response"))
0.062   0.000   0.062
system.time(predgbm[[2]]<-predict.gbm(object=fitgbm[[2]], newdata=datatest[small[[2]],], n.trees=newtrees, type="response"))
0.119   0.000   0.120
system.time(predgbm[[3]]<-predict.gbm(object=fitgbm[[2]], newdata=datatest[small[[3]],], n.trees=newtrees, type="response"))
0.412   0.036   0.447
system.time(predgbm[[4]]<-predict.gbm(object=fitgbm[[1]], newdata=datatest[small[[4]],], n.trees=newtrees, type="response"))
0.216   0.000   0.216
system.time(predgbm[[5]]<-predict.gbm(object=fitgbm[[1]], newdata=datatest[small[[5]],], n.trees=newtrees, type="response"))
0.216   0.000   0.216
system.time(predgbm[[6]]<-predict.gbm(object=fitgbm[[1]], newdata=datatest[small[[6]],], n.trees=newtrees, type="response"))
0.218   0.000   0.218
system.time(predgbm[[7]]<-predict.gbm(object=fitgbm[[1]], newdata=datatest[small[[7]],], n.trees=newtrees, type="response"))
0.219   0.000   0.220
system.time(predgbm[[8]]<-predict.gbm(object=fitgbm[[1]], newdata=datatest[small[[8]],], n.trees=newtrees, type="response"))
0.22    0.00    0.22

system.time(predgbm[[9]]<-predict.gbm(object=fitgbm[[2]], newdata=datatest[small[[4]],], n.trees=newtrees, type="response"))
0.221   0.000   0.220
system.time(predgbm[[10]]<-predict.gbm(object=fitgbm[[2]], newdata=datatest[small[[5]],], n.trees=newtrees, type="response"))
0.220   0.000   0.219
system.time(predgbm[[11]]<-predict.gbm(object=fitgbm[[2]], newdata=datatest[small[[6]],], n.trees=newtrees, type="response"))
0.22    0.00    0.22
system.time(predgbm[[12]]<-predict.gbm(object=fitgbm[[2]], newdata=datatest[small[[7]],], n.trees=newtrees, type="response"))
0.22    0.00    0.22
system.time(predgbm[[13]]<-predict.gbm(object=fitgbm[[2]], newdata=datatest[small[[8]],], n.trees=newtrees, type="response"))
0.221   0.000   0.221

system.time(predgbm[[14]]<-predict.gbm(object=fitgbm[[3]], newdata=datatest[small[[4]],], n.trees=newtrees, type="response"))
system.time(predgbm[[15]]<-predict.gbm(object=fitgbm[[3]], newdata=datatest[small[[5]],], n.trees=newtrees, type="response"))
system.time(predgbm[[16]]<-predict.gbm(object=fitgbm[[3]], newdata=datatest[small[[6]],], n.trees=newtrees, type="response"))
system.time(predgbm[[17]]<-predict.gbm(object=fitgbm[[3]], newdata=datatest[small[[7]],], n.trees=newtrees, type="response"))
system.time(predgbm[[18]]<-predict.gbm(object=fitgbm[[3]], newdata=datatest[small[[8]],], n.trees=newtrees, type="response"))

rm(datatrain, x, fitgbm)
save.image("weighted_gbm.RData")

pred<-predgbm[[1]]
pred[which(pred>=0.5)]<-1
pred[which(pred<0.5)]<-0
table(pred, ytest[small[[1]]], dnn=list('predicted','actual'))
############################################
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

