names(kagglestep)
kaggleaoa=kagglestep[,-5:-64]
names(kaggleaoa)
fitlogitaoa=glm(dlogtarg~.+r:f+r:m+r:tof+r:lambda+r:AOA+f:m+f:tof+f:lambda+f:AOA+tof:lambda+tof:AOA+lambda:AOA,family = binomial,data = kaggleaoa)
fitstepaoa=step(fitlogitaoa)

names(kaoanz2)
kaoanz=ktnz2[,-99]
kaoanz2=kaoanz[,-6:-65]
fit.fullaoa=lm(logtarg~.+r:f+r:m+r:tof+r:lambda+r:AOA+f:m+f:tof+f:lambda+f:AOA+tof:lambda+tof:AOA+lambda:AOA,data=kaoanz2)
fit.fullaoastep=step(fit.fullaoa)
summary(kaoanz)
#prob
names(kaggleaoa)
kaggleaoamatrix=kaggleaoa[,-38]
names(kaggleaoamatrix)
p=as.matrix(kaggleaoamatrix)
fitpridge=glmnet(p,kaggle$dlogtarg,alpha=1,family="binomial")
fitlpridge=cv.glmnet(p,kaggle$dlogtarg,alpha=1,family="binomial")
fitlpridge$lambda.min

#function
names(ktnz4)
ktnz5=ktnz4[,-5:-64]
names(ktnz5)
x=as.matrix(ktnz5)
fitlassonz=glmnet(x,ktnz$logtarg,alpha=1)
fitlassonzcv=cv.glmnet(x,ktnz$logtarg,alpha=1)
fitlassonzcv$lambda.min

