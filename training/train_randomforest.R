#TRAIN USING RANDOM FOREST OR RANDOM FERNS

#Random Forest
#http://www.inside-r.org/packages/cran/randomForest/docs/randomForest
system.time(fitrf<-randomForest(x=datatrain, y=ytrain, family="binomial"))

system.time(pred<-predict(object=fitrf, newdata=datatest[smalltest,], type="response"))

#Additional parameters: growing

#Random Ferns
#http://blog.yhathq.com/posts/comparing-random-forests-in-python-and-r.html
#http://cran.r-project.org/web/packages/rFerns/rFerns.pdf