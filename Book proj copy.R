book=read.csv(file.choose(),header=T)
booktrain=read.csv(file.choose(),header=T)
View(book)
View(booktrain)
attach(booktrain)
table(missinbook=is.na(book$logtarg))
row.names(booktrain)=booktrain$id
booktrain$id=NULL
#split train & test
set.seed(12345)
train=runif(nrow(booktrain))<0.5
table(train)
#1.lazy model
fit_lazy=lm(logtarg~.,booktrain,subset=train)
summary(fit_lazy)
lazy_yhat=predict(fit_lazy,booktrain[!train,])
lazy_MSE=mean((booktrain$logtarg[!train]-lazy_yhat)^2)
lazy_MSE #0.3985349
plot(fit_lazy) #terrible
hist(booktrain$logtarg)
#transformation and interaction
summary(booktrain)
hist(booktrain$r)
str(booktrain)
for(i in 2:65) booktrain[[i]] = log(booktrain[[i]]+1)
summary(booktrain)
fit_log=lm(logtarg~.,booktrain,subset=train)
log_yhat=predict(fit_log,booktrain[!train,])
log_MSE=mean((booktrain$logtarg[!train]-log_yhat)^2)
log_MSE # 0.3798627

plot(booktrain[train,])
par(mfrow=c(8,8))
for (i in 2:65) plot(booktrain[,i],booktrain$logtarg)
plot(logtarg,r)
#stepwise backward
fit_stepwise=lm(logtarg~r + tof + Fcartoons5 + Freligion8 + Fmusic14 + Fhistory19 + 
                  Fconthist20 + Ftravelguides31 + Fhealth35 + Fvideos50 + Mmusic14 + 
                  Mhistory19 + Mtravelguides31 + Mhealth35 + Mhobby40 + Mvideos50,booktrain,subset=train)
summary(fit_stepwise)
stepwise_yhat=predict(fit_stepwise, booktrain[!train,])
stepwise_MSE=mean((booktrain$logtarg[!train]-stepwise_yhat)^2)
stepwise_MSE #0.3800574

#Ridege
library(glmnet)
fit_ridge=glmnet(log)

