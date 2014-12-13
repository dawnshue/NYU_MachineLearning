#TRAINING USING GLMNET::BINOMIAL LOGISTIC REGRESSION"
setwd('~/MLfinalproject/data/')
load('weighted_smallsub.RData')
ls()

#install.packages("glmnet")
library(glmnet)


#GENERATING MODEL
fitglm<-list()
system.time(fitglm[[1]]<-glmnet(x=datatrain[train0,], y=ytrain[train0]))
#   user  system elapsed 
#
system.time(fitglm[[2]]<-glmnet(x=datatrain[train1,], y=ytrain[train1]))
#user  system elapsed 
#
system.time(fitglm[[3]]<-glmnet(x=datatrain, y=ytrain))
#user  system elapsed 
#

##### PERFORM PREDICT
predglm<-list()
for(x in c(1:8)) {
  print(paste0('predsvm[[',x,']]<-predict(object=fitglm[[3]]
                                    , newdata=datatest[small[[',x']],]
                                    , type="class"
                                    , threshold=0.05
  )'
  ))
  flush.console()
}

pcnt<-9
for(x in seq(4,8,1)) {
  for(f in c(1:2)) {
    system.time(predsvm[[pcnt]]<-predict(object=fitglm[[f]]
                                         , newx=datatest[small[[x]],]
                                         , type="response", s=0.05
    ))
    pcnt<-pcnt+1
    flush.console()
  }
}





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