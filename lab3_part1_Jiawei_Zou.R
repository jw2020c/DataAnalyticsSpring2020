install.packages("C50")
library(C50)
data("iris")
head(iris) 
str(iris) 
table(iris$Species) 
set.seed(321)
grn <-runif(nrow(iris))
irisrand <-iris[order(grn),]
str(irisrand)
classificationmodel1 <-C5.0(irisrand[1:100,-5], irisrand[1:100,5])
classificationmodel1
summary(classificationmodel1)
prediction1 <- predict(classificationmodel1,irisrand[101:150,])
prediction1
table(irisrand[101:150,5],prediction1)

library("e1071")
classifier<-naiveBayes(iris[,1:4], iris[,5])
table(predict(classifier, iris[,-5]), iris[,5], dnn=list('predicted','actual'))
classifier$apriori
classifier$tables$Petal.Length
plot(function(x) dnorm(x, 1.462, 0.1736640), 0, 8, col="red", main="Petal length distribution for the 3 different species")
curve(dnorm(x, 4.260, 0.4699110), add=TRUE, col="blue")
curve(dnorm(x, 5.552, 0.5518947 ), add=TRUE, col = "green") 


par(mar=rep(0.2,4))
data <- matrix(rnorm(400),20)
image(1:20,1:20,t(data)[,nrow(data):1])

?heatmap
heatmap(data)

?rbinom
library(titanic)
data(Titanic)
library(caret)
library(rpart)
library(party)

base <- names(titanic_train)
base <- base[-c(1,2)]
base <- list(base)
mypaste <- function(x){
  paste(x,collapse =  "+")
}
myformula <- as.formula(paste0("Survived~",do.call(mypaste, base)))
myrpart <- rpart(myformula,titanic_train[,-1])
testy <- predict(myrpart,newx=titanic_test)

mtctree <- ctree(myformula,data=titanic_train)

apply(train,2,function(x) print(class(x)))
train <- titanic_train

for(i in c(1,2,4,5,6,8,9,)){
  train[,i] <- as.factor((train[,i])) 
}

