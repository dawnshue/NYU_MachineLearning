#TRAINING USING GLMNET::BINOMIAL LOGISTIC REGRESSION"
rm(list=ls()) #clears workspace
setwd('~/MLfinalproject/data/')
load('weighted_smallsub.RData')
ls()

#install.packages("glmnet")
library(glmnet)


#GENERATING MODEL
fitglm<-list()
system.time(fitglm[[1]]<-glmnet(x=datatrain[train0,], y=ytrain[train0]))
#   user  system elapsed 
#293.033   1.298 294.006
system.time(fitglm[[2]]<-glmnet(x=datatrain[train1,], y=ytrain[train1]))
#user  system elapsed 
#469.368   1.505 470.336
system.time(fitglm[[3]]<-glmnet(x=datatrain, y=ytrain))
#user  system elapsed 
#808.587   2.070 809.759
save.image("weighted_glm.RData")

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

############
system.time(predglm[[1]]<-predict(object=fitglm[[3]], newx=datatest[small[[1]],], type="response", s=0.05))
0.034   0.000   0.034 
system.time(predglm[[2]]<-predict(object=fitglm[[3]], newx=datatest[small[[2]],], type="response", s=0.05))
0.076   0.000   0.076
system.time(predglm[[3]]<-predict(object=fitglm[[3]], newx=datatest[small[[3]],], type="response", s=0.05))
0.274   0.028   0.301
system.time(predglm[[4]]<-predict(object=fitglm[[3]], newx=datatest[small[[4]],], type="response", s=0.05))
0.161   0.012   0.173
system.time(predglm[[5]]<-predict(object=fitglm[[3]], newx=datatest[small[[5]],], type="response", s=0.05))
0.201   0.016   0.217
system.time(predglm[[6]]<-predict(object=fitglm[[3]], newx=datatest[small[[6]],], type="response", s=0.05))
0.198   0.016   0.213
system.time(predglm[[7]]<-predict(object=fitglm[[3]], newx=datatest[small[[7]],], type="response", s=0.05))
0.157   0.032   0.189
system.time(predglm[[8]]<-predict(object=fitglm[[3]], newx=datatest[small[[8]],], type="response", s=0.05))
0.166   0.028   0.194
system.time(predglm[[9]]<-predict(object=fitglm[[2]], newx=datatest[small[[4]],], type="response", s=0.05))
0.159   0.028   0.188
system.time(predglm[[10]]<-predict(object=fitglm[[2]], newx=datatest[small[[5]],], type="response", s=0.05))
0.153   0.016   0.170
system.time(predglm[[11]]<-predict(object=fitglm[[2]], newx=datatest[small[[6]],], type="response", s=0.05))
0.161   0.012   0.173
system.time(predglm[[12]]<-predict(object=fitglm[[2]], newx=datatest[small[[7]],], type="response", s=0.05))
0.154   0.016   0.170
system.time(predglm[[13]]<-predict(object=fitglm[[2]], newx=datatest[small[[8]],], type="response", s=0.05))
0.166   0.008   0.175
system.time(predglm[[14]]<-predict(object=fitglm[[1]], newx=datatest[small[[4]],], type="response", s=0.05))
0.157   0.016   0.173
system.time(predglm[[15]]<-predict(object=fitglm[[1]], newx=datatest[small[[5]],], type="response", s=0.05))
0.158   0.012   0.170
system.time(predglm[[16]]<-predict(object=fitglm[[1]], newx=datatest[small[[6]],], type="response", s=0.05))
0.157   0.012   0.169
system.time(predglm[[17]]<-predict(object=fitglm[[1]], newx=datatest[small[[7]],], type="response", s=0.05))
0.161   0.012   0.173
system.time(predglm[[18]]<-predict(object=fitglm[[1]], newx=datatest[small[[8]],], type="response", s=0.05))
0.197   0.020   0.217

rm(fitglm, x, datatrain, datatest)
save.image("weighted_glm.RData")

########################




#Fit 1: using defaults: lasso penalty, nlambda=100
system.time(fit1<-glmnet(x=datatrain, y=ytrain, family="binomial"))
#Datatrain:
#user   system  elapsed 
#1465.517    2.615 1466.737
system.time(pred<-predict(object=fit1, newx=datatest[smalltest,], type="response", s=0.05))
#300
#user  system elapsed 
#0.07    0.00    0.07
pred<-round(pred)
table(pred, ytest[smalltest], dnn=list('predicted','actual'))
#300
#actual
#predicted   0   1
#0 232  28
#1   5  35
1-(sum(abs(as.numeric(as.character(ytest[smalltest])) - 
                       as.numeric(as.character(pred))))/length(ytest[smalltest]))
#300: 0.89

#Fit 2: lasso penalty, lamdba from 10-fold cross-validation
system.time(cv2<-cv.glmnet(x=dataset2,y=y,family="binomial",nfolds=10,alpha=1))
system.time(fit2<-glmnet(x=dataset2,y=y,family="binomial",alpha=1, lambda=cv2$lambda))
#Fit 3: current version
system.time(cv3<-cv.glmnet(x=dataset2,y=y,family="binomial",nfolds=5,alpha=1,nlambda=100,pmax=1500))
system.time(fit3<-glmnet(x=dataset2,y=y,family="binomial",alpha=1, lambda=cv3$lambda))

plot(fit1)
plot(fit2)
plot(fit3)

#TESTING MODEL
head(pred)
system.time(pred<-predict(object=fit1, newx=datatest[smalltest,], type="response"))