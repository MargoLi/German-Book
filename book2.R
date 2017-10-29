library(gam)
book = read.csv("teach/data/book/book.csv")
bookans = read.csv("teach/data/book/bookans.csv")
names(bookans)
names(book)
train = !is.na(book$logtarg)

book$totF = apply(book[,7:36], 1, sum)
book$aos = book$m/(book$f+1)
head(book, 2)
names(book)

summary(book)
# log the data
for(i in 3:68) book[[i]] = log(book[[i]]+1)
fit = lm(logtarg ~ ., book[,2:68])
summary(fit)
fit2 = step(fit)
yhat = predict(fit2, book[!train,])
length(yhat)
write.csv(data.frame(id=book$id[!train], logtarg=yhat), "teach/data/book/sampans.csv", row.names=F)
sqrt(mean((yhat - bookans$logtarg)^2))


