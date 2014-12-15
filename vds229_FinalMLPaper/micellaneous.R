install.packages("vcd")
install.packages("MASS")
require(vcd)
require(MASS)
curve(dexp(x, rate=fitdistr(ftimes
                            ,"exponential")$estimate)
      , col = "red", add = TRUE)




m<-c(35000, 70000, 142433)
ftimes<-c(82.248, 167.030, 294.883)/60
plot(x=m,y=ftimes,type="l"
     ,xlab="# training examples"
     ,ylab="time (min)")
#cor(m, ftimes) #0.9966318
#lm(ftimes ~ m) #m: 3.245e-5


ptimes<-c(33.053, 82.095, 168.915, 335.145)/60
sample<-c(100,250,500,1000)
plot(x=sample,y=ptimes,type="b"
     ,xlab="# test cases"
     ,ylab="time (min)")
#cor(sample, ptimes) #0.9999622
#lm(ptimes ~ sample) #m: 0.005606


m<-c(35000, 70000, 142433)
nb_acc<-c(0.7992, 0.8036, 0.8148)-0.7992
#ave(nb_acc): 0.8058667
nb_fp<-c(0.0056,0.0036,0.0052)-0.0056
nb_fn<-c(0.1952,0.1928,0.1800)-0.1952
yax<-c(-0.02,0.02)
plot(x=m,y=nb_acc,type="b"
     ,xlab="# training examples"
     ,ylab="Change in accuracy (%)"
     ,ylim=yax
)
par(new=TRUE)
plot(x=m,y=nb_fp,type="b",axes=FALSE
     ,xlab="",ylab="",col="blue"
     ,ylim=yax
)
par(new=TRUE)
plot(x=m,y=nb_fn,type="b",axes=FALSE
     ,xlab="",ylab="",col="red"
     ,ylim=yax
)
#cor(m, nb_acc) #0.9988469
#lm(nb_acc ~ m) #m: 1.466e-7