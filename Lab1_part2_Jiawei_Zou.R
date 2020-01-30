EPI_data <- read.csv("C:/Users/23721/Downloads/DAta analytics/EPI_data.csv")
attach(EPI_data)
qqnorm(DALY)
qqplot(EPI,DALY)
qqline(DALY)

x <- 30:95
qqplot(qt(ppoints(250),df=5),x, xlab = "Q-Q plot")
qqline(x)

qqplot(EPI,WATER_H)
qqnorm(WATER_H)
qqline(WATER_H)

qqplot(EPI,ECOSYSTEM)
qqnorm(ECOSYSTEM)

boxplot(EPI,DALY,ENVHEALTH, ECOSYSTEM,  BIODIVERSITY)
boxplot(AIR_H, WATER_H, AIR_E,WATER_E)

library(ggplot2)
qplot(EPI,DALY)
ggplot(EPI_data,aes(EPI,DALY))+geom_point()+
  geom_point(data = filter(EPI_data,DALY>75,EPI<50),col="2")
ggplot(EPI_data,aes(EPI))+geom_histogram(binwidth = 5)


mult <- read.csv("C:/Users/23721/Downloads/DAta analytics/multivariate.csv")
attach(mult)
mm <- lm(Homeowners~Immigrant)
summary(mm$coefficients)

plot(Homeowners~Immigrant)
abline(mm,col=2,lwd=3)

newImmigrantdata <- data.frame(Immigrant = c(0,  20))
mm %>% predict(newImmigrantdata)
attributes(mm)

