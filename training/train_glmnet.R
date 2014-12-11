#TRAINING USING GLMNET::BINOMIAL LOGISTIC REGRESSION"
#install.packages("glmnet")
library(glmnet)
#TEST EXAMPLE
dataset2<-matrix(abs(round(rnorm(100*20)))%%2,100,20)
y<-sample(1:2,100,replace=TRUE)
fit<-glmnet(dataset2, y, family="binomial")
testdataset<-matrix(abs(round(rnorm(100*20)))%%2,100,20)
head(dataset2)
head(testdataset)

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


#install.packages("ipred")
library(ipred)
mymodel.glm<-function(formula, data) {
  glm(formula, data, family=binomial(link=logit))
}
mypredict.glm<-function(object, newdata) {
  predict(object, newdata, type="response")
}
est2<-errorest(data=testdataset
               , model=fit2
               , model=glm
               , predict=mypredict.glm)

mycoef<-coef(fit2, s=cv2$lambda.1se)[which(abs(coef(fit2, s=cv2$lambda.1se))>0),]
mycoef






system('mkdir ~/hl')
setwd('~/hl')
system("s3cmd ls s3://cmcdf/hive_tables/hash_lookup/ | awk '{print $2}' | parallel --gnu s3cmd get {}*")
system("cat ~/hl/* | pigz -c > ~/hl.txt.gz")

system('mkdir ~/dt')
setwd('~/dt')
system ('rm *')
system("s3cmd ls s3://cmcdf/hive_tables/demdex_traits2/ | awk '{print $4}' | parallel --gnu s3cmd get {}*")
system("cat ~/dt/* > ~/dt.txt")





#hl<-read.table(file='~/hl/hl2.txt', sep='\001')
hl2<-readLines('~/hl.txt.gz')
hl2.1<-strsplit(hl2,split="\001") 
hl2.2<-lapply(hl2.1, function(x) x[1:2])
#change to rbindlist
hl2.3<-do.call('rbind',hl2.2)
hl2_4<-data.frame(hl2.3)
library('sqldf')
coefdf<-data.frame(field=names(mycoef), betaval=mycoef)

mresults<-sqldf('select field, betaval, x1 hname, x2 mname from coefdf a left join hl2_4 b on a.field = b.x2 order by betaval desc' )


dt<-readLines('~/dt.txt')
dt2<-strsplit(dt,split="\t") 
dt3<-data.frame(do.call('rbind',dt2), stringsAsFactors =FALSE)
mresults$dt<-sapply(strsplit(mresults$mname, '_'), function (x) x[1])
lapply(mresults[1:10,1], function(x) table(y, dataset2[,which(colnames(dataset2)==x)]))



dtlookup<-rbind(
  c(4,'trait')
  ,c(5,'segment')
  ,c(7,'page')
  ,c(8,'host')
  ,c(9,'utm_source')
  ,c(10,'cm_list1')
  ,c(11,'cm_refer_page')
  ,c(12,'cm_refer_host')
  ,c(13,'cm_group_id')
  ,c(14,'post')
  ,c(15,'tc_keyword')
  ,c(16,'top_level_dir')
  ,c(17,'stir_qm_title_words')
  ,c(18,'exelate')
  ,c(19,'mediamath')
  ,c(20,'regioncode')
  ,c(21,'metrocode')
  ,c(22,'citycode')
  ,c(23,'connspeed')
)

dtlookupdf<-data.frame(dtlookup)
mresults2<-sqldf("select a.*, b.X7,b.X2 from mresults a left outer join dt3 b on substr(a.mname,1,1) = '4' and a.hname = b.X1")
mresults3<-sqldf("select *, b.X2 type from mresults2 a left outer join dtlookupdf b on a.dt= b.X1")
head(mresults3, n =50)

setwd('~')
write.csv(file='modelcoefout.csv',mresults3)
options(scipen=10)
write.csv(file='s3push.csv',coefdf, row.names=F, quote=F)

system("s3cmd put s3push.csv s3://cmcdf/hive_tables/segment/hispanic/coeftable/ --force")

save.image(file='model_image_hispanic.RData', compress=TRUE)

system("s3cmd put model_image_hispanic.RData s3://cmds/model_images/ --force")


