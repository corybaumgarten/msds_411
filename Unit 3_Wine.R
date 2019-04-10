my.path <- '/Users/corybaumgarten/Documents/Northwestern/2017FA_PREDICT_411-DL_SEC55/Unit 3/Wine Sales Project/';
my.file <- paste(my.path,'wine.csv',sep='');
wine <- read.csv(my.file,header=TRUE);

str(wine)
summary(wine)

# load packages
library(car)
library(corrplot)
library(lessR)
library(MASS)
library(moments)

CountAll(wine)


#### TARGET ####
SummaryStats(TARGET,data=wine)
Histogram(TARGET,data=wine)
ftable(wine$TARGET)


#### FixedAcidity ####
SummaryStats(FixedAcidity,data=wine)
Histogram(FixedAcidity,data=wine)


#### VolatileAcidity ####
SummaryStats(VolatileAcidity,data=wine)
Histogram(VolatileAcidity,data=wine)


#### CitricAcid ####
SummaryStats(CitricAcid,data=wine)
Histogram(CitricAcid,data=wine)


#### ResidualSugar ####
SummaryStats(ResidualSugar,data=wine)
Histogram(ResidualSugar,data=wine)


#### Chlorides ####
SummaryStats(Chlorides,data=wine)
Histogram(Chlorides,data=wine)


#### FreeSulfurDioxide ####
SummaryStats(FreeSulfurDioxide,data=wine)
Histogram(FreeSulfurDioxide,data=wine)


#### TotalSulfurDioxide ####
SummaryStats(TotalSulfurDioxide,data=wine)
Histogram(TotalSulfurDioxide,data=wine)


#### Density ####
SummaryStats(Density,data=wine)
Histogram(Density,data=wine)


#### pH ####
SummaryStats(pH,data=wine)
Histogram(pH,data=wine)


#### Sulphates ####
SummaryStats(Sulphates,data=wine)
Histogram(Sulphates,data=wine)


#### Alcohol ####
SummaryStats(Alcohol,data=wine)
Histogram(Alcohol,data=wine)


#### LabelAppeal ####
SummaryStats(LabelAppeal,data=wine)
Histogram(LabelAppeal,data=wine)


#### AcidIndex ####
SummaryStats(AcidIndex,data=wine)
Histogram(AcidIndex,data=wine)


#### STARS ####
SummaryStats(STARS,data=wine)
Histogram(STARS,data=wine)
boxplot(wine$STARS,horizontal = TRUE)


##############################################################################
##############################################################################
##############################################################################
##############################################################################


#### Data cleaning ####

# FixedAcidity
a_FixedAcidity <- abs(wine$FixedAcidity)
s_FixedAcidity <- sqrt(a_FixedAcidity)

par(mfrow=c(1,2))
hist(wine$FixedAcidity,main = "FixedAcidity",col = "red",xlab = "FixedAcidity")
hist(s_FixedAcidity,main = "sqrt(abs(FixedAcidity))",col = "blue")
par(mfrow=c(1,1))

SummaryStats(FixedAcidity,data=wine)
SummaryStats(s_FixedAcidity)



# VolatileAcidity
a_VolatileAcidity <- abs(wine$VolatileAcidity)
s_VolatileAcidity <- sqrt(a_VolatileAcidity)

par(mfrow=c(1,2))
hist(wine$VolatileAcidity,main = "VolatileAcidity",col="red",xlab = "VolatileAcidity")
hist(s_VolatileAcidity,main = "sqrt(abs(VolatileAcidity))",col = "blue")
par(mfrow=c(1,1))

SummaryStats(VolatileAcidity,data=wine)
SummaryStats(s_VolatileAcidity)



# CitricAcid
a_CitricAcid <- abs(wine$CitricAcid)
s_CitricAcid <- sqrt(a_CitricAcid)

par(mfrow=c(1,2))
hist(wine$CitricAcid,main = "CitricAcid",col="red",xlab = "CitricAcid")
hist(s_CitricAcid,main = "sqrt(abs(CitricAcid))",col = "blue")
par(mfrow=c(1,1))

SummaryStats(CitricAcid,data=wine)
SummaryStats(s_CitricAcid)


# ResidualSugar
a_ResidualSugar <- abs(wine$ResidualSugar)
s_ResidualSugar <- sqrt(a_ResidualSugar)

par(mfrow=c(1,2))
hist(wine$ResidualSugar,main = "ResidualSugar",col = "red",xlab = "ResidualSugar")
hist(s_ResidualSugar,main = "sqrt(abs(ResidualSugar))",col = "blue")
par(mfrow=c(1,1))

SummaryStats(ResidualSugar,data = wine)
SummaryStats(s_ResidualSugar)


# Chlorides
a_Chlorides <- abs(wine$Chlorides)
s_Chlorides <- sqrt(a_Chlorides)

par(mfrow=c(1,2))
hist(wine$Chlorides,main = "Chlorides",col = "red",xlab = "Chlorides")
hist(s_Chlorides,main = "sqrt(abs(Chlorides))",col = "blue")
par(mfrow=c(1,1))

SummaryStats(Chlorides,data = wine)
SummaryStats(s_Chlorides)


# FreeSulfurDioxide
a_FreeSulfurDioxide <- abs(wine$FreeSulfurDioxide)
s_FreeSulfurDioxide <- sqrt(a_FreeSulfurDioxide)

par(mfrow=c(1,2))
hist(wine$FreeSulfurDioxide,main = "FreeSulfurDioxide",col = "red",xlab = "FreeSulfurDioxide")
hist(s_FreeSulfurDioxide,main = "sqrt(abs(FreeSulfurDioxide))",col = "blue")
par(mfrow=c(1,1))

SummaryStats(FreeSulfurDioxide,data = wine)
SummaryStats(s_FreeSulfurDioxide)


# TotalSulfurDioxide
a_TotalSulfurDioxide <- abs(wine$TotalSulfurDioxide)
s_TotalSulfurDioxide <- sqrt(a_TotalSulfurDioxide)

par(mfrow=c(1,2))
hist(wine$TotalSulfurDioxide,main = "TotalSulfurDioxide",col = "red",xlab = "TotalSulfurDioxide")
hist(s_TotalSulfurDioxide,main = "sqrt(abs(TotalSulfurDioxide))",col = "blue")
par(mfrow=c(1,1))

SummaryStats(TotalSulfurDioxide,data = wine)
SummaryStats(s_TotalSulfurDioxide)


# Density
s_Density <- sqrt(wine$Density)

par(mfrow=c(1,2))
hist(wine$Density,main = "Density",col = "red",xlab = "Density")
hist(s_Density,main = "sqrt(Density)",col = "blue")
par(mfrow=c(1,1))

SummaryStats(Density,data=wine)
SummaryStats(s_Density)


# pH


# Sulphates
a_Sulphates <- abs(wine$Sulphates)
s_Sulphates <- sqrt(a_Sulphates)

par(mfrow=c(1,2))
hist(wine$Sulphates,main = "Sulphates",col = "red",xlab = "Sulphates")
hist(s_Sulphates,main = "sqrt(abs(Sulphates))",col = "blue")
par(mfrow=c(1,1))

SummaryStats(Sulphates,data=wine)
SummaryStats(s_Sulphates)


# Alcohol
a_Alcohol <- abs(wine$Alcohol)

par(mfrow=c(1,2))
hist(wine$Alcohol,main = "Alcohol",col = "red",xlab = "Alcohol")
hist(a_Alcohol,main = "abs(Alcohol)",col = "blue")
par(mfrow=c(1,1))

SummaryStats(Alcohol,data=wine)
SummaryStats(a_Alcohol)


# LabelAppeal


# AcidIndex
s_AcidIndex <- sqrt(wine$AcidIndex)

par(mfrow=c(1,2))
hist(wine$AcidIndex,main = "AcidIndex",col = "red",xlab = "AcidIndex")
hist(s_AcidIndex,main = "sqrt(AcidIndex)",col = "blue")
par(mfrow=c(1,1))

SummaryStats(AcidIndex,data = wine)
SummaryStats(s_AcidIndex)



#### Fix missing values ####

wine.2 <- cbind.data.frame(wine,s_FixedAcidity,s_VolatileAcidity,s_CitricAcid,
 s_ResidualSugar,s_Chlorides,s_FreeSulfurDioxide,s_TotalSulfurDioxide,s_Density,
 s_Sulphates,a_Alcohol,s_AcidIndex)

str(wine.2)


keep.vars <- c('INDEX','TARGET','s_FixedAcidity','s_VolatileAcidity','s_CitricAcid',
               's_ResidualSugar','s_Chlorides','s_FreeSulfurDioxide','s_TotalSulfurDioxide',
               's_Density','pH','s_Sulphates','a_Alcohol','LabelAppeal','s_AcidIndex',
               'STARS');

# define skinny data frame
wine.3 <- wine.2[,keep.vars];

str(wine.3)
summary(wine.3)

# create data frame
wine.3 <- data.frame(wine.3)

anyNA(wine.3)


# missing values
library(mice)
missingvalues <- mice(wine.3[, !names(wine.3) %in% c("TARGET")], method="rf")
miceOutput <- complete(missingvalues)  # generate the completed data

# Check for missing values
anyNA(miceOutput)

str(miceOutput)

TARGET <- wine$TARGET

# create new data frame with complete data
mynewdata <- cbind.data.frame(miceOutput,TARGET)

str(mynewdata)
anyNA(mynewdata)



#### Model Building ####

#  Poisson distribution
#  Negative Binomial distribution
#  Zero Inflated Poisson distribution
#  Zero Inflated Negative Binomial distribution
#  Regression 


# Poisson regression model

# remove s_FixedAcidity, s_ResidualSugar

model.1 <- glm(TARGET ~ s_VolatileAcidity+s_CitricAcid+s_Chlorides+s_FreeSulfurDioxide+
 s_TotalSulfurDioxide+s_Density+pH+s_Sulphates+a_Alcohol+LabelAppeal+s_AcidIndex+
 STARS,data=mynewdata,family = 'poisson'(link = "log"))

summary(model.1)


# Negative Binomial model

# remove s_FixedAcidity, s_ResidualSugar

model.2 <- glm.nb(TARGET ~ s_VolatileAcidity+s_CitricAcid+s_Chlorides+s_FreeSulfurDioxide+
 s_TotalSulfurDioxide+s_Density+pH+s_Sulphates+a_Alcohol+LabelAppeal+s_AcidIndex+
 STARS,data=mynewdata)

summary(model.2)


#  Zero Inflated Poisson distribution

library(pscl)

# remove s_CitricAcid, s_FreeSulfurDioxide, s_ResidualSugar, s_Sulphates, s_Chlorides,
# s_FixedAcidity, pH, s_TotalSulfurDioxide, s_Density, s_VolatileAcidity

model.3 <- zeroinfl(TARGET ~ +a_Alcohol+LabelAppeal+s_AcidIndex+STARS,data=mynewdata)

summary(model.3)


#  Zero Inflated Negative Binomial distribution

# remove s_CitricAcid, s_ResidualSugar, s_FixedAcidity, s_FreeSulfurDioxide, pH,
# s_Sulphates, s_TotalSulfurDioxide, s_Chlorides, s_Density, s_VolatileAcidity

model.4 <- zeroinfl(TARGET ~ a_Alcohol+LabelAppeal+s_AcidIndex+STARS,
 data=mynewdata,dist="negbin", EM = TRUE)

summary(model.4)


#  OLS

# remove s_FixedAcidity, s_ResidualSugar

model.5 <- lm(TARGET ~ s_VolatileAcidity+s_CitricAcid+s_Chlorides+s_FreeSulfurDioxide+
 s_TotalSulfurDioxide+s_Density+pH+s_Sulphates+a_Alcohol+LabelAppeal+s_AcidIndex+
 STARS,data=mynewdata)

summary(model.5)


AIC(model.1)
AIC(model.2)
AIC(model.3)
AIC(model.4)
AIC(model.5)


# Average Square Error
mse.1 <- mean(model.1$residuals^2)
mse.2 <- mean(model.2$residuals^2)
mse.3 <- mean(model.3$residuals^2)
mse.4 <- mean(model.4$residuals^2)
mse.5 <- mean(model.5$residuals^2)

mse.1
mse.2
mse.3
mse.4
mse.5




#### write best model file to CSV ####

pred1 <- predict(model.1)
pred1 <- round(pred1,0)

pred2 <- predict(model.2)
pred2 <- round(pred2,0)

pred3 <- predict(model.3)
pred3 <- round(pred3,0)

pred4 <- predict(model.4)
pred4 <- round(pred4,0)

pred5 <- predict(model.5)
pred5 <- round(pred5,0)


wine.df <- data.frame(wine$INDEX,wine$TARGET,pred1,pred2,pred3,pred4,pred5)

write.csv(wine.df,file="wine.df.csv")






#### misc code ####

# Model 1 -- automated variable selection

mynewdata.1 <- mynewdata[,!names(mynewdata) %in% c("INDEX")]
str(mynewdata.1)


# define upper model
upper.1 <- glm(TARGET ~ .,data = mynewdata.1, family = 'poisson'(link = "log"))
summary(upper.1)


# Define the lower model as the Intercept model 
lower.1 <- glm(TARGET ~ 1,data=mynewdata.1, family = 'poisson'(link = 'log'));
summary(lower.1)


m1 <- step(lower.1,scope=formula(upper.1), direction="forward",k=2)
summary(m1)

mynewdata$pred1 <- m1$fitted.values

data.frame(mynewdata$INDEX,mynewdata$pred1)



# Model 2 -- automated variable selection

mynewdata.2 <- mynewdata[,!names(mynewdata) %in% c("INDEX")]
str(mynewdata.2)


# define upper model
upper.2 <- glm.nb(TARGET ~ .,data = mynewdata.2)
summary(upper.2)


# Define the lower model as the Intercept model 
lower.2 <- glm.nb(TARGET ~ 1,data=mynewdata.2);
summary(lower.2)


m2 <- step(lower.2,scope=formula(upper.2), direction="forward",k=2)
summary(m2)


# Model 3 -- automated variable selection

mynewdata.3 <- mynewdata[,!names(mynewdata) %in% c("INDEX")]
str(mynewdata.3)


# define upper model
upper.3 <- zeroinfl(TARGET ~ .,data = mynewdata.3)
summary(upper.3)


# Define the lower model as the Intercept model 
lower.3 <- zeroinfl(TARGET ~ 1,data=mynewdata.3);
summary(lower.3)


m3 <- step(lower.3,scope=formula(upper.3), direction="forward",k=2)
summary(m3)



# Model 4 -- automated variable selection

mynewdata.4 <- mynewdata[,!names(mynewdata) %in% c("INDEX")]
str(mynewdata.4)


# define upper model
upper.4 <- zeroinfl(TARGET ~ .,data=mynewdata,dist="negbin", EM = TRUE)
summary(upper.4)


# Define the lower model as the Intercept model 
lower.4 <- zeroinfl(TARGET ~ 1,data=mynewdata,dist="negbin", EM = TRUE)
summary(lower.4)


m4 <- step(lower.4,scope=formula(upper.4), direction="forward",k=2)
summary(m4)



# Model 5 -- automated variable selection

mynewdata.5 <- mynewdata[,!names(mynewdata) %in% c("INDEX")]
str(mynewdata.5)


# define upper model
upper.5 <- lm(TARGET ~ .,data=mynewdata)
summary(upper.5)


# Define the lower model as the Intercept model 
lower.5 <- lm(TARGET ~ 1,data=mynewdata)
summary(lower.5)


m5 <- step(lower.5,scope=formula(upper.5), direction="forward",k=2)
summary(m5)




