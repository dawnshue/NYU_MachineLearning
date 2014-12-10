#TRAINING USING GLMNET::BINOMIAL LOGISTIC REGRESSION"
rm(list=ls()) #clears workspace
setwd('~/TESTWORK/MLfinalproject/data/')


#Some methods and libraries needed
system("s3cmd get -r s3://cmcdf/scripts/utils.R ~/utils.R")
source('~/utils.R')
#source('~/svn/ds/models/utils.R')
totranlist <- function(x) strsplit(readLines(x),split=",")


install.packages("glmnet")
library(glmnet)

#TEST EXAMPLE
x=matrix(rnorm(100*20),100,20)
g2=sample(1:2,100,replace=TRUE)
fit2=glmnet(x, g2, family="binomial")



#get files, do parallel read, unlist, have done this before with some split and shuf
#convpath<-'~/learn/hispanic=1/'
#paste0(convpath,list.files(path=convpath, pattern='.ans'))

inlist<-'~/events.txt'
conv<-parLapply(cl=cl, inlist, totranlist)
conv<-unlist(conv,recursive=FALSE, use.names=FALSE)

#get files, do parallel read, unlist
#nconvpath<-'~/learn/hispanic=0/'
inlist<-'~/nconv.txt.gz' #paste0(nconvpath,list.files(path=nconvpath, pattern='.ans'))
nconv<-parLapply(cl=cl, inlist, totranlist)
nconv<-unlist(nconv,recursive=FALSE, use.names=FALSE)



allusers<-unlist(list(conv,nconv), recursive=FALSE, use.names=FALSE)

#profile this
system.time(allt<-as(allusers, "transactions"))

y<-c(rep(1, length(conv)), rep(0, length(nconv)))
table(y)
allt
gc()

lsos()
dataset <- t(as(allt,'ngCMatrix'))
myrows<-which(rowSums(dataset)>2)
y<-y[myrows]
dataset2<-dataset[myrows,which(colSums(dataset) > 50 &  colSums(dataset)/nrow(dataset) < .9)]
table(colSums(dataset2))
dim(dataset2)

library(glmnet)
fit<-cv.glmnet(x=dataset2,y=y,family="binomial",nfolds=5,alpha=1,nlambda=100,pmax=1500)

str(fit)

fit2<-glmnet(x=dataset2,y=y,family="binomial",alpha=1, lambda=fit$lambda)
plot(fit2)

mycoef<-coef(fit2, s=fit$lambda.1se)[which(abs(coef(fit2, s=fit$lambda.1se))>0),]
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


