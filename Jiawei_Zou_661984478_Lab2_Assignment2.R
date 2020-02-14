#Lab2 part1
#2a
EPI_data <- read.csv(file = "C:/Users/23721/Downloads/DAta analytics/EPI_data.csv")
attach(EPI_data)

summary(EPI)
summary(DALY)

hist(EPI)
hist(DALY)

library(dplyr)
select(sample_n(EPI_data,5),EPI,DALY)
select(sample_frac(EPI_data,0.1),EPI,DALY)

new_decs_EPI <-arrange(EPI_data,desc(EPI))
new_decs_DALY<- arrange(EPI_data,desc(DALY))

mutate(EPI_data,double_EPI=EPI*2,double_DALY=DALY*2)

EPI_data%>%summarise(average_EPI=mean(EPI,na.rm=T),average_DAIY=mean(DALY,na.rm=T))

boxplot(ENVHEALTH,ECOSYSTEM)

qqplot(ENVHEALTH,ECOSYSTEM)


#2b
#here we want to build model with EPI=a+b1*AIR_E+b2*CLIMATE
boxplot(EPI,AIR_E,CLIMATE)

lmEPI<-lm(EPI~AIR_E+CLIMATE)

lmEPI

summary(lmEPI)

cEPI<-coef(lmEPI)

newAIR_E <- seq(5,95,by=5)
newCLIMATE <- seq(5,95,by=5)

NEW <- data.frame(newAIR_E,newCLIMATE)
names(NEW) <- c("AIR_E","CLIMATE")

pred <- predict(lmEPI,newdata = NEW,interval = "prediction")
con <- predict(lmEPI,NEW,interval = "confidence")


#Lab2 part2
multi <- read.csv("C:/Users/23721/Downloads/DAta analytics/dataset_multipleRegression.csv")
abalone <- read.csv("C:/Users/23721/Downloads/DAta analytics/abalone.csv")

#multiregression
attach(multi)
mul <- lm(ROLL~UNEM+HGRAD)
summary(mul)

new <- as.data.frame(matrix(c(7,90000),1))
names(new)<-c("UNEM","HGRAD")
answer <-predict(mul,newdata=new)
print(answer)

newm <- lm(ROLL~UNEM+HGRAD+INC)
summary(newm)

INC <- c(25000)
new <- cbind(new,INC)
newanswer <-predict(mul,newdata=new)
print(newanswer)

#KNN
head(abalone)
abalone$Rings <- as.numeric(abalone$Rings)

abalone$Rings <- cut(abalone$Rings, br=c(-1,8,11,35), labels = c("young", 'adult', 'old'))
abalone$Rings <- as.factor(abalone$Rings)
summary(abalone$Rings)

#remove sex
abalone <- abalone[,-1]

#standardize here I used sdandard deviation rather maxmium
sdandard <- apply(abalone[,1:7],2,sd)
abalone[,1:7] <- abalone[,1:7]/sdandard
summary(abalone$Shucked.weight)

#get training sample
set.seed(202)
i <- sample(2, nrow(abalone), replace=T, prob=c(0.75, 0.25))
KNNtrain <- abalone[i==1,1:7]
y <- abalone[i==1,8]
KNNtest <- abalone[i==2,1:7]
testy <- abalone[i==2,8]

#get a K
k <- floor(sqrt(nrow(abalone)))+1

library(class)
pred <- knn(train = KNNtrain, test = KNNtest, cl = y, k = k)
table(pred)
table(testy)

#Kmeans
data(iris)
table(iris[,5])
km <- as.data.frame(iris)
km <- km[,-5]

kmmodel <-kmeans(km,3,nstart = 20,iter.max = 10000)$cluster
table(iris[,5],kmmodel)

