rm(list=ls()) #clears workspace

#Some methods and libraries needed
setwd('~/MLfinalproject/training/')
source('utils.R')
totranlist <- function(x) {
  tryCatch({
    strsplit(readLines(x),split=",")
  },
  error=function(e) {
    print(e)
    stop(e)
  })
}


#SMALL TEST EXAMPLE##
traindata<-matrix(abs(round(rnorm(150*4)))%%2,150,4)
y<-sample(0:1,100,replace=TRUE)
traindata[which(iris[1]>5.5),1]<-1
traindata[which(iris[1]<=5.5),1]<-0
traindata[which(iris[2]>3.4),2]<-0
traindata[which(iris[2]<=3.4),2]<-0
traindata[which(iris[3]>2),3]<-0
traindata[which(iris[3]<=2),3]<-0
traindata[which(iris[4]>0.5),4]<-0
traindata[which(iris[4]<=0.5),4]<-0
y<-c(1:150)
y[which(iris[5]=='virginica')]<-1
y[which(iris[5]!='virginica')]<-0
head(traindata)
y
dim(traindata)
fit<-glmnet(traindata, y, family="binomial")
#####################


setwd('~/MLfinalproject/data/')
#TRAINING DATA ################################
#Unlist the features in class=1
inlist<-'~/MLfinalproject/data/train_events.txt'
conv<-parLapply(cl=cl, inlist, totranlist)
conv<-unlist(conv,recursive=FALSE, use.names=FALSE)
lconv<-length(conv)
#Unlist the features in class=0
inlist<-'~/MLfinalproject/data/train_nonevents.txt.gz'
nconv<-parLapply(cl=cl, inlist, totranlist)
nconv<-unlist(nconv,recursive=FALSE, use.names=FALSE)
lnconv<-length(nconv)
#percent events
pcnt<-lconv/lnconv
#TESTING DATA ################################
inlist<-'~/MLfinalproject/data/test_events.txt'
conv2<-parLapply(cl=cl, inlist, totranlist)
conv2<-unlist(conv2,recursive=FALSE, use.names=FALSE)
lconv2<-length(conv2)
inlist<-'~/MLfinalproject/data/test_nonevents.txt.gz'
nconv2<-parLapply(cl=cl, inlist, totranlist)
nconv2<-unlist(nconv2,recursive=FALSE, use.names=FALSE)
lnconv2<-length(nconv2)

#MERGE TRAINING & TEST DATA
#This ensures that any manipulations of data will occur for both
allt<-as(unlist(list(conv,nconv), recursive=FALSE, use.names=FALSE),"transactions")
allt
traindata<- t(as(allt,'ngCMatrix'))
trainy<-c(rep(1, length(conv)), rep(0, length(nconv)))

allt<-as(unlist(list(conv2,nconv2), recursive=FALSE, use.names=FALSE),"transactions")
allt
testdata<- t(as(allt,'ngCMatrix'))
testy<-c(rep(1, length(conv2)), rep(0, length(nconv2)))

rm(conv)
rm(nconv)
rm(conv2)
rm(nconv2)
rm(allt)
gc()
lsos()


#Filtering training data for sparse features
keeprows<-which(rowSums(traindata)>2) #if record<=2 features
keepcols<-which(colSums(traindata)>50 & colSums(traindata)/nrow(traindata)<.9)
y<-y[keeprows]
dataset<-traindata[keeprows,keepcols]
#traindata<-dataset2
#testdata<-dataset2
#####################

#SUMMARY============================
#TRAIN DATA
table(colSums(dataset))
dim(dataset)
table(trainy)
#TEST DATA
table(colSums(testdata))
dim(testdata)
table(testy)