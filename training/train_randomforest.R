#TRAIN USING RANDOM FOREST OR RANDOM FERNS
rm(list=ls()) #clears workspace
setwd('~/MLfinalproject/data/')
load('weighted_smallsub.RData')
ls()

#Random Forest
#install.packages("bigrf")
library(bigrf)
fitrf<-list()
system('mkdir rf_cache')
#http://cran.r-project.org/web/packages/bigrf/bigrf.pdf
tempdata<-data.frame(datatrain)
#tempdata[]<-as.factor(tempdata)
system.time(fitrf[[1]]<-bigrfc(x=tempdata[train0,], y=as.factor(ytrain[train0])
                               , ntrees=100
                               , cachepath='rf_cache'
                               , trace=0
                               ))
save.image('weighted_brf.RData')
system.time(fitrf[[2]]<-bigrfc(x=tempdata[train1,], y=as.factor(ytrain[train1])
                               , ntrees=100
                               , cachepath='rf_cache'
                               , trace=0
))
save.image('weighted_brf.RData')
system.time(fitrf[[3]]<-bigrfc(x=tempdata, y=as.factor(ytrain)
                               , ntrees=100
                               , cachepath='rf_cache'
                               , trace=0
))
save.image('weighted_brf.RData')


#install.packages("randomForest")
library(randomForest)
#http://www.inside-r.org/packages/cran/randomForest/docs/randomForest

bigrfc(x, y, ntrees = 50L, varselect = NULL, varnlevels = NULL,
       nsplitvar = round(sqrt(ifelse(is.null(varselect), ncol(x),
                                     length(varselect)))), maxeslevels = 11L, nrandsplit = 1023L,
       maxndsize = 1L, yclasswts = NULL, printerrfreq = 10L,
       printclserr = TRUE, cachepath = tempdir(), trace = 0L)

system.time(fitrf[[1]]<-randomForest(x=datatrain[train0,], y=ytrain[train0]
                                     , ntrees=100
                                     , keep.forest=FALSE
                                     , keep.inbag=FALSE
                                     ))
save.image('weighted_rf.RData')
system.time(fitrf[[2]]<-randomForest(x=datatrain[train1,], y=ytrain[train1]
                                     , ntrees=100
                                     , keep.forest=FALSE
                                     , keep.inbag=FALSE
))
save.image('weighted_rf.RData')
system.time(fitrf[[3]]<-randomForest(x=datatrain, y=ytrain
                                     , ntrees=100
                                     , keep.forest=FALSE
                                     , keep.inbag=FALSE
))
save.image('weighted_rf.RData')

##### PERFORM PREDICT
#http://www.inside-r.org/packages/cran/randomforest/docs/predict.randomForest
for(x in c(1:8)) {
  cat(paste0('system.time(predrf[['
             ,x
             ,']]<-predict(object=fitrf[[3]], newdata=datatest[small[['
             ,x
             ,']],], n.trees=newtrees, type="response"))'
             ,'\n'
  ))
  flush.console()
}
predrf<-list()
system.time(pred<-predict(object=fitrf, newdata=datatest[smalltest,], type="response"))
predict((object, newdata, type="response",
         norm.votes=TRUE, predict.all=FALSE, proximity=FALSE, nodes=FALSE,
         cutoff, ...))
#Additional parameters: growing

#Random Ferns
#http://blog.yhathq.com/posts/comparing-random-forests-in-python-and-r.html
#http://cran.r-project.org/web/packages/rFerns/rFerns.pdf