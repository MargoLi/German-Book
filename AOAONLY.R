book=read.csv(file.choose(),header=T)
row.names(book)=book$id
book$id=NULL
book$aoa=book$m/book$f
book$lambda=book$f/book$tof
names(book)
for(i in 66:67) book[[i]]=ifelse(is.na(book[[i]])=="TRUE",0,book[[i]])

summary(book)

book$dlogtarg=ifelse(book$logtarg>0,1,0)
table(book$dlogtarg)

names(book)
set.seed(12345)
book$train=runif(nrow(book))<0.5
table(book$train)
names(book)
for(i in 6:35) book[[i+64]]=(book[[i+30]])/(book[[i]])
for(i in 70:99) book[[i]]=ifelse(is.na(book[[i]])=="TRUE",0,book[[i]])
for(i in 2:67) book[[i]]=log(book[[i]]+1)
for(i in 70:99) book[[i]]=log(book[[i]]+1)
summary(book)
booktrain=book[book$train=="TRUE",]
names(booktrain)
booktest=book[book$train=="FALSE",]
names(booktest)


#prob
#full
probaoa=booktrain[,-6:-65]
probaoa2=probaoa[,-1]
probaoa3=probaoa2[,-8]
names(probaoa3)
probfull=glm(dlogtarg~.+r:f+r:m+r:tof+r:aoa+r:lambda+f:m+f:tof+f:aoa+f:lambda+m:tof+m:aoa+m:lambda+tof:aoa+tof:lambda+aoa:lambda,data=probaoa3,family=binomial)
summary(probfull)
#stepwise
probstep=step(probfull)
probtest=predict(probstep,data.frame(booktest),type="response")
library(pROC)
auc=roc(booktest$dlogtarg>0,probtest)
auc
#ridge
names(probaoa2)
probmatrix=probaoa2[,-7:-8]
names(probmatrix)
summary(probmatrix)
p=as.matrix(probmatrix)
library(glmnet)
summary(probaoa$dlogtarg)
probridge=glmnet(p,probaoa$dlogtarg,alpha=0,family="binomial")
probridge.cv=cv.glmnet(p,probaoa$dlogtarg,alpha=0,family="binomial")
probridge.cv$lambda.min

#lasso
problasso=glmnet(p,probaoa$dlogtarg,alpha=1,family="binomial")
problasso.cv=cv.glmnet(p,probaoa$dlogtarg,alpha=1,family="binomial")
problasso.cv$lambda.min



#RF
library(randomForest)
names(probaoa3)
probrf=randomForest(dlogtarg~.,probaoa3,mtry=12,ntree=500,importance=T)
probtest=predict(probrf,data.frame(booktest),type="response")
library(pROC)
auc=roc(booktest$dlogtarg>0,probtest)
auc
#GBM
library(gbm)
probgbm=gbm(dlogtarg~.,data=probaoa3,interaction.depth = 2,shrinkage=0.001,n.trees = 3000)

probtest=predict(probgbm,data.frame(booktest),type="response",n.trees=3000)
library(pROC)
auc=roc(booktest$dlogtarg>0,probtest)
auc

#function
trainnz=booktrain[booktrain$logtarg>0,-69]
names(trainnz)
trainnzaoa=trainnz[,-6:-65]
names(trainnzaoa)
trainnzaoa=trainnzaoa[,-8]
names(trainnzaoa)
summary(trainnzaoa)
#full
spfull=lm(logtarg~.+r:f+r:m+r:tof+r:aoa+r:lambda+f:m+f:tof+f:aoa+f:lambda+m:tof+m:aoa+m:lambda+tof:aoa+tof:lambda+aoa:lambda,trainnzaoa)
#stepwise
spstep=step(spfull)
summary(spstep)

#ridge
names(trainnzaoa)
summary(trainnzaoa)
spmatrix=trainnzaoa[,-1]
names(spmatrix)
spridge=glmnet(as.matrix(spmatrix),trainnzaoa$logtarg,alpha = 0)
spridge.cv=cv.glmnet(as.matrix(spmatrix),trainnzaoa$logtarg,alpha = 0)
spridge.cv$lambda.min

#lasso
splasso=glmnet(as.matrix(spmatrix),trainnzaoa$logtarg,alpha = 1)
splasso.cv=cv.glmnet(as.matrix(spmatrix),trainnzaoa$logtarg,alpha = 1)
splasso.cv$lambda.min

#RF Model
library(randomForest)
sprf=randomForest(x=trainnzaoa[,2:37],y=trainnz$logtarg,xtest=testnzmatrix,ntree=500,importance = T)
varImpPlot(sprf,cex=0.5)
mean((testnz$logtarg-sprf$test$predicted)^2)

sprf2=randomForest(logtarg~.,trainnzaoa,mtry=12,ntree=500,importance=T)
yhat=predict(sprf2,data.frame(testnz))
fmserf=mean((testnz$logtarg-yhat)^2)
fmserf

#GBM
library(gbm)
names(trainnzaoa)
spgbm=gbm(logtarg~.,data=trainnzaoa,interaction.depth = 2,shrinkage=0.001,n.trees = 10000)
spgbm
yhat=predict(spgbm,data.frame(testnz),n.trees=10000)
fmsegbm=mean((testnz$logtarg-yhat)^2)
fmsegbm

#MSE 
names(booktest)
testaoa=booktest[,-6:-65]
names(testaoa)
testaoa=testaoa[,-9]
testaoamatrix=testaoa[,-8]
testaoamatrix=testaoamatrix[,-1]
names(testaoamatrix)
p=as.matrix(testaoamatrix)

testnz=testaoa[testaoa$logtarg>0,]
names(testnz)
testnzmatrix=testnz[,-8]
testnzmatrix=testnzmatrix[,-1]
names(testnzmatrix)
#function mse
fhat=predict(spfull,data.frame(testnz))
summary(fhat)
fmsefull=mean((testnz$logtarg-fhat)^2)
fmsefull

fhat=predict(spstep,data.frame((testnz)))
summary(fhat)
fmsestep=mean((testnz$logtarg-fhat)^2)
fmsestep

fhat=predict(spridge,s=spridge.cv$lambda.min,newx=as.matrix(testnzmatrix))
summary(fhat)
fmseridge=mean((testnz$logtarg-fhat)^2)
fmseridge

fhat=predict(splasso,s=splasso.cv$lambda.min,newx=as.matrix(testnzmatrix))
summary(fhat)
fmselasso=mean((testnz$logtarg-fhat)^2)
fmselasso

#full-gbm model mse
testaoa$prob=predict(probfull,data.frame(testaoa),type="response")
summary(testaoa$prob)
names(testaoa)
names(testaoamatrix)
testaoa$sp=predict(spgbm,data.frame(testaoa),n.trees = 3000)
summary(testaoa$sp)
testaoa$yhat=testaoa$prob*testaoa$sp
msefull=mean((testaoa$logtarg-testaoa$yhat)^2)
msefull

#stepwise-gbm model mse
testaoa$prob=predict(probstep,data.frame(testaoa),type="response")
summary(testaoa$prob)
testaoa$yhat=testaoa$prob*testaoa$sp
msestep=mean((testaoa$logtarg-testaoa$yhat)^2)
msestep

#ridge-gbm
testaoa$prob=predict(probridge,s=probridge.cv$lambda.min,newx=as.matrix(testaoamatrix),type="response")
summary(testaoa$prob)
library(pROC)
auc=roc(booktest$dlogtarg>0,testaoa$prob)
auc
testaoa$yhat=testaoa$prob*testaoa$sp
mseridge=mean((testaoa$logtarg-testaoa$yhat)^2)
mseridge

#ridge-gbm
testaoa$prob=predict(problasso,s=problasso.cv$lambda.min,newx=as.matrix(testaoamatrix),type="response")
summary(testaoa$prob)
testaoa$yhat=testaoa$prob*testaoa$sp
mselasso=mean((testaoa$logtarg-testaoa$yhat)^2)
mselasso

#RF-gbm
testaoa$prob=predict(probrf,data.frame(testaoa),type="response")
summary(testaoa$prob)
testaoa$yhat=testaoa$prob*testaoa$sp
mserf=mean((testaoa$logtarg-testaoa$yhat)^2)
mserf

#gbm-gbm
names(testaoa)
testaoa$prob=predict(probgbm,data.frame(testaoa),type="response",n.trees=3000)
summary(testaoa$prob)
testaoa$yhat=testaoa$prob*testaoa$sp
msegbm=mean((testaoa$logtarg-testaoa$yhat)^2)
msegbm


#train whole dataset
names(book)
bookaoa=book[,-69]
names(bookaoa)
bookaoa=bookaoa[,-6:-65]
names(bookaoa)
bookprob=bookaoa[,-1]
summary(bookaoa)
names(bookprob)
bookaoamatrix=bookaoa[,-8]
names(bookaoamatrix)
bookaoamatrix=bookaoamatrix[,-1]
names(bookaoamatrix)
probwholefull=glm(dlogtarg~.+r:f+r:m+r:tof+r:aoa+r:lambda+f:m+f:tof+f:aoa+f:lambda+m:tof+m:aoa+m:lambda+tof:aoa+tof:lambda+aoa:lambda,data=bookprob,family=binomial)
probwholestep=step(probwholefull)
#probwholelasso=glmnet(as.matrix(bookaoamatrix),bookaoa$dlogtarg,alpha=1,family="binomial")
#probwholelasso.cv=cv.glmnet(as.matrix(bookaoamatrix),bookaoa$dlogtarg,alpha=1,family="binomial")
#probwholelasso.cv$lambda.min
names(bookprob)
probwholegbm=gbm(dlogtarg~.,data=bookprob,interaction.depth = 4,shrinkage=0.001,n.trees = 5000)
names(bookaoa)
bookspmatrix=bookaoa[bookaoa$logtarg>0,-8]
summary(bookspmatrix)
#spwholefull=lm(logtarg~.+r:f+r:m+r:tof+r:aoa+r:lambda+f:m+f:tof+f:aoa+f:lambda+m:tof+m:aoa+m:lambda+tof:aoa+tof:lambda+aoa:lambda,data=bookspmatrix)
#spwholestep=step(spwholefull)
summary(spwholestep)
bookspmatrix2=bookspmatrix[,-1]
#spwholelasso=glmnet(as.matrix(bookspmatrix2),bookspmatrix$logtarg,alpha=1)
#spwholelasso.cv=cv.glmnet(as.matrix(bookspmatrix2),bookspmatrix$logtarg,alpha=1)
#spwholeasso.cv$lambda.min
spwholegbm=gbm(logtarg~.,data=bookspmatrix,interaction.depth = 2,shrinkage = 0.001,n.trees = 3000)


#predict
ft=read.csv(file.choose(),header=T)
row.names(ft)=ft$id
ft$id=NULL
names(ft)
ft$aoa=ft$m/ft$f
ft$lambda=ft$f/ft$tof
names(ft)
for(i in 66:67) ft[[i]]=ifelse(is.na(ft[[i]])=="TRUE",0,ft[[i]])
summary(ft)
ft$train=is.na(ft$logtarg)=="FALSE"
ft$dlogtarg=ifelse(ft$logtarg>0,1,0)
names(ft)
for(i in 6:35) ft[[i+64]]=(ft[[i+30]])/(ft[[i]])
for(i in 70:99) ft[[i]]=ifelse(is.na(ft[[i]])=="TRUE",0,ft[[i]])
for(i in 2:67) ft[[i]]=log(ft[[i]]+1)
for(i in 70:99) ft[[i]]=log(ft[[i]]+1)
names(ft)
ftaoa=ft[ft$train=="FALSE",-6:-65]
names(ftaoa)
ftaoa=ftaoa[,-8:-9]
names(ftaoa)
ftaoamatrix=ftaoa[,-1]
names(ftaoamatrix)
summary(ftaoamatrix)

#ftaoa$prob=predict(probwholelasso,s=probwholelasso.cv$lambda.min,newx=as.matrix(ftaoamatrix),type="response")
ftaoa$prob=predict(probwholestep,data.frame(ftaoa),type="response")
summary(ftaoa$prob)
probwholegbm
ftaoa$pb=predict(probwholegbm,data.frame(ftaoa),n.trees=5000,type="response")
summary(ftaoa$pb)
ftaoa$sp=predict(spwholegbm,data.frame(ftaoa),n.trees=3000)
summary(ftaoa$sp)
ftaoa$logtarg=ftaoa$prob*ftaoa$sp
summary(ftaoa$logtarg)
summary(bookaoa$logtarg)
predict=read.csv(file.choose(),header=T)
predict$train=is.na(predict$logtarg)
table(predict$train)
predict=predict[!ft$train,]
predict$logtarg=ftaoa$logtarg
predict=predict[,1:2]
predict$logtarg=ifelse(predict$logtarg<0,0,predict$logtarg)
summary(predict$logtarg)
View(predict)
library(foreign)
write.foreign(predict,"~/Desktop/predictaoastepgbm.csv","~/Desktop/predict6.txt",package="SPSS")
