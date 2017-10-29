book=read.csv(file.choose(),header=T)
booktrain=read.csv(file.choose(),header=T)
View(book)
View(booktrain)
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

#transformation and interaction
summary(booktrain)
hist(booktrain$Mhobby40)
str(booktrain)
