getwd()
setwd("C:\\Users\\aaroh\\Documents\\BA with R")
# read the dataset
mydata <- read.csv("mortality.csv",header= TRUE)
head(mydata)
mydata$City <- NULL
head(mydata)
#predict model to check for outliers
lmfit <- lm(Mortality ~ ., data=mydata)
summary(lmfit)
plot(lmfit)
#plot cooks distance to plot outliers

cook <- cooks.distance(lmfit)
plot(cook) 
abline(h = 4*mean(cook, na.rm=T), col="blue")
outliers_row <- as.numeric(names(cook)[(cook > 4*mean(cook, na.rm=T))]) 
head(mydata[outliers_row, ]) 
#remove outliers
mydata_new <-mydata[-c(31,36,58), ]
nrow(mydata_new)
#normalise dataset
hist(mydata_new$JanTemp)
hist(mydata_new$JulyTemp)
hist(mydata_new$RelHum)
hist(mydata_new$Rain)
hist(mydata_new$Education)
hist(mydata_new$PopDensity)
hist(mydata_new$NW)
hist(mydata_new$WC)
hist(mydata_new$pop)
hist(mydata_new$HHSiz)
hist(mydata_new$income)
hist(mydata_new$HCPot)
hist(mydata_new$NOxPot)
hist(mydata_new$S02Pot)
mydata_new$S02Pot <- sqrt(mydata_new$S02Pot)
mydata_new$NOxPot <- sqrt(mydata_new$NOxPot)
mydata_new$HCPot <- sqrt(mydata_new$HCPot)
mydata_new$pop <- sqrt(mydata_new$pop)
mydata_new$NW <- sqrt(mydata_new$NW)
hist(mydata_new$HCPot)
hist(mydata_new$NOxPot)
hist(mydata_new$S02Pot)
hist(mydata_new$pop)
hist(mydata_new$NW)
#perform linear regression 
new_model <- lm(Mortality~ ., data = mydata_new)
summary(new_model)
plot(new_model)
#linear regression using significant variables
fin_model <- lm(Mortality~Rain+PopDensity+NW+HCPot,data=mydata_new)
summary(fin_model)
fin_model1 <- lm(Mortality~Rain+PopDensity+NW,data=mydata_new)
summary(fin_model1)
#stepwise regression(forward)
null <- lm(Mortality ~ 1, data = mydata_new)
full <- lm(Mortality ~ ., data = mydata_new)
selectedmodel <- step(null, scope=list(lower=null, upper=full), direction="forward")
selectedmodel
fwd_model <- lm(Mortality ~ NW + Education + PopDensity + Rain + S02Pot + JanTemp + JulyTemp, data = mydata_new)
summary(fwd_model)
fwd_model1 <- lm(Mortality ~ NW + Education + PopDensity + Rain + S02Pot + JanTemp , data = mydata_new)
summary(fwd_model1)
#principal compoent analysis
pcdata <- mydata_new
pcdata$Mortality <- NULL
fit <-princomp(pcdata,cor = TRUE)
summary(fit)
loadings(fit)
plot(fit,type="lines")
mydata_new <- cbind(mydata_new,fit$scores)
head(mydata_new)
pca_model <- lm(Mortality ~ Comp.1+Comp.2+Comp.3+Comp.4+Comp.5+Comp.6+Comp.7, data=mydata_new)
summary(pca_model)
new_pcamodel <- lm(Mortality ~ Comp.1+Comp.2+Comp.3+Comp.5+Comp.6, data=mydata_new)
summary(new_pcamodel)




















