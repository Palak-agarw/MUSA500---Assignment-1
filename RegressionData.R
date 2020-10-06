install.packages("DAAG")
install.packages("car")
install.packages("MASS")
install.packages("rsq")
install.packages("dummies")
library(dummies)
library(DAAG)
library(car)  #to calculate VIF
library(MASS)
library(rsq)

#Avoid getting results in scientific notation!
options(scipen = 999)

multregdata <- read.csv("C:\\Users\\agarw\\Documents\\MUSA500\\HW 1\\RegressionData.csv")

mean(multregdata$MEDHHINC)
mean(multregdata$MEDHVAL)
mean(multregdata$PCTVACANT)
mean(multregdata$PCTBACHMOR)
mean(multregdata$PCTSINGLES)
mean(multregdata$NBELPOV100)


sd(multregdata$MEDHHINC)
sd(multregdata$MEDHVAL)
sd(multregdata$PCTVACANT)
sd(multregdata$PCTBACHMOR)
sd(multregdata$PCTSINGLES)
sd(multregdata$NBELPOV100)


hist(multregdata$MEDHHINC, breaks=50)
hist(multregdata$MEDHVAL, breaks=50)
hist(multregdata$PCTVACANT)
hist(multregdata$PCTBACHMOR)
hist(multregdata$PCTSINGLES)
hist(multregdata$NBELPOV100)

hist(log(multregdata$MEDHHINC), breaks=50)
hist(log(multregdata$MEDHVAL), breaks=50)
hist(log(multregdata$PCTVACANT+1))
hist(log(multregdata$PCTBACHMOR+1))
hist(log(multregdata$PCTSINGLES+1))
hist(log(multregdata$NBELPOV100+1))

plot(log(multregdata$MEDHVAL), multregdata$PCTVACANT)
plot(log(multregdata$MEDHVAL), multregdata$PCTSINGLES)
plot(log(multregdata$MEDHVAL), multregdata$PCTBACHMOR)
plot(log(multregdata$MEDHVAL), log(multregdata$NBELPOV100+1))

cor(log(multregdata$MEDHVAL), multregdata$PCTVACANT, method="pearson")
cor(log(multregdata$MEDHVAL), multregdata$PCTSINGLES, method="pearson")
cor(log(multregdata$MEDHVAL), multregdata$PCTBACHMOR, method="pearson")
cor(log(multregdata$MEDHVAL), log(multregdata$NBELPOV100+1), method="pearson")


fit <- lm(log(MEDHVAL) ~ PCTVACANT + PCTSINGLES + PCTBACHMOR + log(NBELPOV100+1), data=multregdata)
summary(fit)

anova(fit)

#Predicted values (y-hats)
multregdata$predvals <- fitted(fit) 
#Residuals
multregdata$resids <- residuals(fit)
hist(multregdata$resids)
#Standardized Residuals
multregdata$stdres <- rstandard(fit)

plot(multregdata$predvals, multregdata$stdres)




