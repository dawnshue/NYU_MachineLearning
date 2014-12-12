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
datatrain<-matrix(abs(round(rnorm(150*4)))%%2,150,4)
y<-sample(0:1,100,replace=TRUE)
datatrain[which(iris[1]>5.5),1]<-1
datatrain[which(iris[1]<=5.5),1]<-0
datatrain[which(iris[2]>3.4),2]<-0
datatrain[which(iris[2]<=3.4),2]<-0
datatrain[which(iris[3]>2),3]<-0
datatrain[which(iris[3]<=2),3]<-0
datatrain[which(iris[4]>0.5),4]<-0
datatrain[which(iris[4]<=0.5),4]<-0
ytrain<-c(1:150)
ytrain[which(iris[5]=='virginica')]<-1
ytrain[which(iris[5]!='virginica')]<-0
head(datatrain)
ytrain
dim(datatrain)
datatest<-datatrain
ytest<-ytrain
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
savefile<-'~/MLfinalproject/data/weighted.RData'
save.image(file=savefile)
system(paste0('s3cmd put '
              , savefile
              , ' s3://cmcdf/hive_tables/segment/hispanic/'))


#Filtering data of sparse features ################################
pdata<-0.1 #percent of data to actually use
levents2<-round(pdata*levents)
lnon2<-round(pdata*lnon)
cevents<-seq(1,levents,1)
cnon<-seq(levents+1,levents+lnon,1)
userows<-c(sample(cevents,levents2)
           ,sample(cnon,lnon2))
y<-ally[userows]
dataset<-alldata[userows,]
rm(cevents)
rm(cnon)
rm(userows)
#keeprows<-which(rowSums(dataset)>2) #if record<=2 features
#y<-y[keeprows]
#dataset<-dataset[keeprows,]
keepcols<-which(colSums(dataset)>50)
#& colSums(dataset)/nrow(dataset)<.9
dataset<-dataset[,keepcols]
rm(keepcols)

#Partition out the testdata
ptest<-0.1 #Percent of data to use as test
levents3<-round(ptest*levents2)
lnon3<-round(ptest*lnon2)
rtest<-c(seq(1,levents3,1)
         ,seq(levents2+1,levents2+lnon3,1))
rtrain<-c(seq(levents3+1,levents2,1)
          ,seq(levents2+lnon3+1,levents2+lnon2,1))
ytest<-y[rtest]
datatest<-dataset[rtest,]
ytrain<-y[rtrain]
datatrain<-dataset[rtrain,]

rm(rtest)
rm(rtrain)
rm(dataset)
rm(y)
gc()

#SUMMARY============================
#TRAIN DATA
#table(colSums(traindata))
dim(datatrain)
table(ytrain)
#TEST DATA
#table(colSums(dataset_test))
dim(datatest)
table(ytest)

# More Clean Up
sort(sapply(ls(),function(x) object.size(get(x))),T)[1:10]/1e6
gc()
