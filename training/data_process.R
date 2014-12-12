rm(list=ls()) #clears workspace

#Some methods and libraries needed
setwd('~/TESTWORK/MLfinalproject/training/')
source('utils.R')
totranlist <- function(x) strsplit(readLines(x),split=",")


#SMALL TEST EXAMPLE##
traindata<-matrix(abs(round(rnorm(100*20)))%%2,100,20)
y<-sample(0:1,100,replace=TRUE)
head(traindata)
head(testdata)
fit<-glmnet(dataset2, y, family="binomial")
#####################

setwd('~/TESTWORK/MLfinalproject/data/')


#TRAINING DATA ################################
#Unlist the features in class=1
inlist<-'train_events.txt'
conv<-parLapply(cl=cl, inlist, totranlist)
conv<-unlist(conv,recursive=FALSE, use.names=FALSE)

#Unlist the features in class=0
inlist<-'train_nonevents.txt.gz'
nconv<-parLapply(cl=cl, inlist, totranlist)
nconv<-unlist(nconv,recursive=FALSE, use.names=FALSE)

allt<-as(unlist(list(conv,nconv), recursive=FALSE, use.names=FALSE),"transactions")
allt
traindata<- t(as(allt,'ngCMatrix'))
y<-c(rep(1, length(conv)), rep(0, length(nconv)))


rm(conv)
rm(nconv)
gc()
lsos()

#TESTING DATA ################################
inlist<-'test_events.txt'
conv<-parLapply(cl=cl, inlist, totranlist)
conv<-unlist(conv,recursive=FALSE, use.names=FALSE)
inlist<-'test_nonevents.txt.gz'
nconv<-parLapply(cl=cl, inlist, totranlist)
nconv<-unlist(nconv,recursive=FALSE, use.names=FALSE)
allt<-as(unlist(list(conv,nconv), recursive=FALSE, use.names=FALSE),"transactions")
allt
testdata<- t(as(allt,'ngCMatrix'))
ytest<-c(rep(1, length(conv)), rep(0, length(nconv)))
rm(conv)
rm(nconv)
gc()
lsos()


#Filtering lines out#
dataset<-traindata
myrows<-which(rowSums(dataset)>2) #if record<=2 features
y<-y[myrows]
dataset2<-dataset[myrows,which(colSums(dataset)>50 & colSums(dataset)/nrow(dataset)<.9)]
#traindata<-dataset2
#testdata<-dataset2
#####################

#SUMMARY============================
#TRAIN DATA
table(colSums(traindata))
dim(traindata)
table(y)
#TEST DATA
table(colSums(testdata))
dim(testdata)
table(ytest)