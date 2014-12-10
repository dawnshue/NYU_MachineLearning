rm(list=ls()) #clears workspace

#Some methods and libraries needed
setwd('~/TESTWORK/MLfinalproject/training/')
source('utils.R')
totranlist <- function(x) strsplit(readLines(x),split=",")


setwd('~/TESTWORK/MLfinalproject/data/')


#Unlist the features in class=1
inlist<-'events.txt'
conv<-parLapply(cl=cl, inlist, totranlist)
conv<-unlist(conv,recursive=FALSE, use.names=FALSE)

#Unlist the features in class=0
inlist<-'nonevents.txt.gz'
nconv<-parLapply(cl=cl, inlist, totranlist)
nconv<-unlist(nconv,recursive=FALSE, use.names=FALSE)


allt<-as(unlist(list(conv,nconv), recursive=FALSE, use.names=FALSE),"transactions")
allt
dataset <- t(as(allt,'ngCMatrix'))
y<-c(rep(1, length(conv)), rep(0, length(nconv)))
table(y)


rm(conv)
rm(nconv)
gc()
lsos()


#Filtering lines out#
myrows<-which(rowSums(dataset)>2) #if record<=2 features
y<-y[myrows]
dataset2<-dataset[myrows,which(colSums(dataset)>50 & colSums(dataset)/nrow(dataset)<.9)]
#####################

dataset2<-dataset
table(colSums(dataset2))
dim(dataset2)