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
#LOAD DATA ################################
#Unlist the features in class=1
inlist<-'~/MLfinalproject/data/weighted_events.txt'
conv<-parLapply(cl=cl, inlist, totranlist)
conv<-unlist(conv,recursive=FALSE, use.names=FALSE)
levents<-length(conv)
#Unlist the features in class=0
inlist<-'~/MLfinalproject/data/weighted_nonevents.txt.gz'
nconv<-parLapply(cl=cl, inlist, totranlist)
nconv<-unlist(nconv,recursive=FALSE, use.names=FALSE)
lnon<-length(nconv)
#Percent data that is events
pevents<-levents/(levents+lnon)


#MERGE DATA ################################
mdata<-1000000 #Number of rows of data to use
#This ensures that any manipulations of data will occur for both test & train data
allt<-as(unlist(list(conv[1:round((pevents)*mdata)]
                     ,nconv[1:round(1-pevents*mdata)])
                , recursive=FALSE, use.names=FALSE),"transactions")
allt
alldata<- t(as(allt,'ngCMatrix'))
ally<-c(rep(1, round((pevents)*mdata)), rep(0, round((1-pevents)*mdata)))

rm(conv)
rm(nconv)
rm(allt)
rm(inlist)
gc()
lsos()


#Filtering data of sparse features ################################
y<-testy
dataset<-testdata
#keeprows<-which(rowSums(dataset)>2) #if record<=2 features
keepcols<-which(colSums(dataset)>50)
# & colSums(dataset)/nrow(dataset)<.9
#y<-y[keeprows]
#dataset<-dataset[keeprows,keepcols]
dataset<-dataset[,keepcols]

nlines<-dim(testdata)[1]
ptest<-0.2 #Percent of data to use as test
lconv*0.2
lnconv*0.2
#####################

#SUMMARY============================
#TRAIN DATA
#table(colSums(traindata))
dim(traindata)
dim(dataset)
table(trainy)
#TEST DATA
#table(colSums(dataset_test))
dim(testdata)
dim(dataset_test)
table(testy)