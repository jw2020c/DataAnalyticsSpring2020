wine_data <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep = ",")
head(wine_data)
dim(wine_data)

colnames(wine_data) <- c("Cvs", "Alcohol", 
                         "Malic_Acid", "Ash", "Alkalinity_of_Ash", 
                         "Magnesium", "Total_Phenols", "Flavanoids", "NonFlavanoid_Phenols",
                         "Proanthocyanins", "Color_Intensity", "Hue", "OD280/OD315_of_Diluted_Wine", 
                         "Proline")
head(wine_data)

heatmap(cor(wine_data),Rowv = NA, Colv = NA) 

cultivar_classes <- factor(wine_data$Cvs) 
cultivar_classes

wine_data_PCA <- prcomp(scale(wine_data[,-1]))
summary(wine_data_PCA)


library(titanic)
data(Titanic)
library(caret)
library(rpart)
library(party)

#data processing
apply(titanic_train,2,function(x) print(class(x)))
train <- titanic_train

for(i in c(3,5,7,8,9,10,11,12)){
  train[,i] <- as.factor((train[,i])) 
}
for(i in c(6,10)){
  train[,i] <- as.numeric((train[,i])) 
}
train <- train[,-c(1,4,9,11)]

#construct formula
base <- names(titanic_train)
base <- base[-c(1,2,4,9,11)]
base <- list(base)
mypaste <- function(x){
  paste(x,collapse =  "+")
}
myformula <- as.formula(paste0("Survived~",do.call(mypaste, base)))

#rpart
myrpart <- rpart(myformula,titanic_train)
plot(myrpart)
text(myrpart)

testy <- predict(myrpart,newx=titanic_test)

#tree
mtctree <- ctree(myformula,data=train)
plot(mtctree)

#RF
library(randomForest)
train <- na.omit(train)
myrf <- randomForest(myformula,data=(train))

print(myrf)
importance(myrf)
plot(myrf)
