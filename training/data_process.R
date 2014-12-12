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
#This ensures that any manipulations of data will occur for both test & train data
allt<-as(unlist(list(conv,nconv),recursive=FALSE, use.names=FALSE),"transactions")
allt
alldata<- t(as(allt,'ngCMatrix'))
ally<-c(rep(1, length(conv)), rep(0, length(nconv)))


rm(conv)
rm(nconv)
rm(allt)
rm(inlist)
gc()
lsos()


#Filtering data of sparse features ################################
pdata<-0.1 #percent of data to actually use
levents2<-round(pdata*levents)
lnon2<-round(pdata*lnon)
cevents<-c(1:levents)
cnon<-c(levents+1:levents+lnon)
userows<-c(sample(cevents,levents2)
           ,sample(cnon,lnon2))
y<-ally[userows]
dataset<-alldata[userows]
#keeprows<-which(rowSums(dataset)>2) #if record<=2 features
#y<-y[keeprows]
#dataset<-dataset[keeprows,]
keepcols<-which(colSums(dataset)>50)
                #& colSums(dataset)/nrow(dataset)<.9
dataset<-dataset[,keepcols]


#Partition out the testdata
ptest<-0.1 #Percent of data to use as test
levents3<-ptest*levents2
lnon3<-ptest*lnon2
rtest<-c(c(1:levents3),c(levents2+1:levents2+lnon3))
rtrain<-c(c(levents3+1:levents2),c(levents2+1+lnon3:levents2+lnon2))
ytest<-y[rtest]
datatest<-dataset[rtest,]
ytrain<-y[rtrain]
datatrain<-dataset[rtrain,]


#SUMMARY============================
#TRAIN DATA
#table(colSums(traindata))
dim(datatrain)
table(ytrain)
#TEST DATA
#table(colSums(dataset_test))
dim(datatest)
table(ytest)