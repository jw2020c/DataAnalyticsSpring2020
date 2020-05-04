setwd("C:/Users/23721/Downloads/DAta analytics")
data <- read.csv("Traffic_Crashes_-_Vehicles.csv")
levels(data$MODEL)
apply(data,2,function(x) print(class(x)))

data$CRASH_DATE<-as.POSIXct(data$CRASH_DATE,format="%m/%d/%Y %I:%M:%S %p", tz="UTC")

data <- data[,-c(1,2,3,5,7,8,9)]

#reform the year
library(lubridate)
data$VEHICLE_YEAR <-year(data$CRASH_DATE)-as.numeric(data$VEHICLE_YEAR)
attach(data)

library(dplyr)

barplot(table(MAKE))
barplot(table(VEHICLE_DEFECT))
hist(VEHICLE_YEAR,xlim=c(0,40))

#data cleaning
data <- data[-which(data$FIRST_CONTACT_POINT=="UNKNOWN"),]
data <- data[-which(data$MANEUVER=="UNKNOWN/NA"),]
data <- data[-which(data$MODEL==""),]
data <- data[-which(data$LIC_PLATE_STATE==""),]
data <- data[-which(data$TRAVEL_DIRECTION=="UNKNOWN"),]
data <- data[-which(data$VEHICLE_USE=="UNKNOWN/NA"),]
data <- data[-which(data$VEHICLE_TYPE=="UNKNOWN/NA"),]

data <- data[,c(4:11,32)]
data <- na.omit(data)     
attach(data)
table(MAKE)[which(table(MAKE)>1000)]
brand <- names(table(MAKE)[which(table(MAKE)>1000)])[-36]
data <- data[which(data$VEHICLE_YEAR>=0),]

#EDA
barplot(table(UNIT_TYPE),main = "UNIT_TYPE")
barplot(table(VEHICLE_TYPE),main = "VEHICLE_TYPE")
barplot(table(VEHICLE_DEFECT),main = "Defect")
table(VEHICLE_DEFECT)
barplot(table(data$TRAVEL_DIRECTION),main ="Direction")
barplot(table(data$FIRST_CONTACT_POINT),main ="First contact point")
hist((data$VEHICLE_YEAR))

apply(data[,-c(1,6)],2,table)



#only left the brands have more than 1000 samples
k<- NA
for(i in 1:length(brand)){
  go <- filter(data,MAKE==brand[i])
  k <- rbind(k,go)
}
k <- k[-1,]
data <- k

#same work for model
model <- names(table(data$MODEL)[which(table(data$MODEL)>1000)])
k<- NA

#only left the brand has more than 1000 samples
for(i in 1:length(model)){
  go <- filter(data,MODEL==model[i])
  k <- rbind(k,go)
}
k <- k[-1,]
data <- k

#same work for other lines
use <- names(table(data$VEHICLE_USE)[which(table(data$VEHICLE_USE)>1000)])
k<- NA
for(i in 1:length(use)){
  go <- filter(data,VEHICLE_USE==use[i])
  k <- rbind(k,go)
}
k <- k[-1,]
data <- k

state <- names(table(data$LIC_PLATE_STATE)[which(table(data$LIC_PLATE_STATE)>1000)])
k<- NA
for(i in 1:length(state)){
  go <- filter(data,LIC_PLATE_STATE==state[i])
  k <- rbind(k,go)
}
k <- k[-1,]
data <- k

k<-NA
type <- names(table(data$VEHICLE_TYPE)[which(table(data$VEHICLE_TYPE)>1000)])
for(i in 1:length(type)){
  go <- filter(data,VEHICLE_TYPE==type[i])
  k <- rbind(k,go)
}
k <- k[-1,]
data <- k

k<-NA
type <- names(table(data$MANEUVER)[which(table(data$MANEUVER)>1000)])
for(i in 1:length(type)){
  go <- filter(data,MANEUVER==type[i])
  k <- rbind(k,go)
}
k <- k[-1,]
data <- k

#create new variable time and season
library(lubridate)
h <- hour(data$CRASH_DATE)
h[h>=20|h<=6] <- 1
h[h<20&h>6]<-"day"
h[h=="1"]<-"night"
h <- as.factor(h)
data$time <- h

m <- month(data$CRASH_DATE)
m[6>=m&m>=4]<-100
m[9>=m&m>=7]<-200
m[12>=m&m>=10]<-300
m[3>=m]<-400
m[m==100]<-"spring"
m[m=="200"]<-"summer"
m[m=="300"]<-"fall"
m[m=="400"]<-"winter"
m <- as.factor(m)
data$season <- m

#convert it to binary problem
data$VEHICLE_DEFECT<-as.character(unlist(data$VEHICLE_DEFECT))
data$VEHICLE_DEFECT[!(data$VEHICLE_DEFECT=="NONE"|data$VEHICLE_DEFECT=="UNKNOWN")]<- "YES"
data$VEHICLE_DEFECT<-as.factor((data$VEHICLE_DEFECT))
table(data$VEHICLE_DEFECT)

#save data
write.csv(data,file = "myda.csv",row.names = F)
setwd("C:/Users/23721/Downloads/DAta analytics")
data <- read.csv("myda.csv",header = T)

#plot
barplot(table(data$time))
barplot(table(data$season))


#train set and test set
mytest <- data[which(data$VEHICLE_DEFECT=="UNKNOWN"),]
mytrain <- data[-which(data$VEHICLE_DEFECT=="UNKNOWN"),]

#we test for rpart but the system crashed
library(caret)
library(rpart)
attach(data)
kk <- rpart(VEHICLE_DEFECT~UNIT_TYPE+time+season+
              VEHICLE_YEAR+VEHICLE_TYPE+VEHICLE_USE+FIRST_CONTACT_POINT,data = mytrain)

x <- mytrain[,-c(1,3,4,5,7)]
y <- mytrain[,7]
y <- as.character(y)
y <- as.factor(y)

fitControl <- trainControl(method = "cv",number=3)
m <- c(2:ncol(x))
b <- c(1,seq(10,200,by=10))
oob.error.rf <- matrix(NA,ncol=length(m),
                       nrow=length(b))
rf.treebag <- list(NA)
nn <- 1
for (j in 1:length(m)){
  grid <- expand.grid(.mtry=m[j],
                      .splitrule="info",
                      .min.node.size=5)
  for (i in 1:length(b)){
    rf.treebag[[nn]] <- train(x=x,y=y,
                              method="ranger",
                              trControl = fitControl,
                              metric="Accuracy",
                              tuneGrid=grid,
                              num.trees=b[i])
    oob.error.rf[i,j] <- rf.treebag[[nn]]$finalModel$prediction.error
    print(i)
    nn <- nn + 1
    print(nn)
  }
}
rf.final <- rf.treebag[[which.min(oob.error.rf)]]
prediction <- predict(rf.final,mytest)

glm <- train(x=x,y=y,
      method="glm",
      trControl = fitControl,
      metric="Accuracy")
co <- glm$finalModel$coefficients

prediction<-predict.glm(glm$finalModel,mytest)
mytest[,7]<-prediction
data <- rbind(mytrain,mytest)

#plot
par(mai=c(1,2.5,1,1))
barplot(co[1:11],horiz = T,las=1,cex.names=0.55)
barplot(co[11:22],horiz = T,las=1,cex.names=0.55)
barplot(co[22:33],horiz = T,las=1,cex.names=0.55)
barplot(co[33:44],horiz = T,las=1,cex.names=0.55)
barplot(co[44:55],horiz = T,las=1,cex.names=0.55)
barplot(co[55:66],horiz = T,las=1,cex.names=0.55)
#

mytrain$VEHICLE_DEFECT <- mytrain$VEHICLE_DEFECT=="YES"
make <- aggregate(mytrain$VEHICLE_DEFECT,by=list(data$MAKE),FUN=mean)
model <- aggregate(mytrain$VEHICLE_DEFECT,by=list(data$MODEL),FUN=mean)
hist(model[,2],main="model defect rate",xlab="model")
hist(make[,2],main="maker defect rate",xlab="maker")
model[which(model[,2]>0.025),]
make[which(make[,2]>0.03),]
