#TRAINING USING GLMNET::BINOMIAL LOGISTIC REGRESSION"
#install.packages("glmnet")
library(glmnet)


#GENERATING MODEL
#Fit 1: using defaults: lasso penalty, nlambda=100
system.time(fit1<-glmnet(dataset2, y, family="binomial"))
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
system.time(pred<-predict(object=fit, newx=testdataset, type="response"))
head(pred)