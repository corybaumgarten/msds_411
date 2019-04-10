######## Model export ########

my.path.2 <- '/Users/corybaumgarten/Documents/Northwestern/2017FA_PREDICT_411-DL_SEC55/Unit 3/Wine Sales Project/';
my.file.2 <- paste(my.path.2,'wine_test.csv',sep='');
wine_test <- read.csv(my.file.2,header=TRUE);

############ insert your cleaning code here

#### Data cleaning /new variables ####


t.a_FixedAcidity <- abs(wine_test$FixedAcidity)
t.s_FixedAcidity <- sqrt(t.a_FixedAcidity)

t.a_VolatileAcidity <- abs(wine_test$VolatileAcidity)
t.s_VolatileAcidity <- sqrt(t.a_VolatileAcidity)

t.a_CitricAcid <- abs(wine_test$CitricAcid)
t.s_CitricAcid <- sqrt(t.a_CitricAcid)

t.a_ResidualSugar <- abs(wine_test$ResidualSugar)
t.s_ResidualSugar <- sqrt(t.a_ResidualSugar)

t.a_Chlorides <- abs(wine_test$Chlorides)
t.s_Chlorides <- sqrt(t.a_Chlorides)

t.a_FreeSulfurDioxide <- abs(wine_test$FreeSulfurDioxide)
t.s_FreeSulfurDioxide <- sqrt(t.a_FreeSulfurDioxide)

t.a_TotalSulfurDioxide <- abs(wine_test$TotalSulfurDioxide)
t.s_TotalSulfurDioxide <- sqrt(t.a_TotalSulfurDioxide)

t.s_Density <- sqrt(wine_test$Density)

t.a_Sulphates <- abs(wine_test$Sulphates)
t.s_Sulphates <- sqrt(t.a_Sulphates)

t.a_Alcohol <- abs(wine_test$Alcohol)

t.s_AcidIndex <- sqrt(wine_test$AcidIndex)



#### Fix missing values ####

wine_test.2 <- cbind.data.frame(wine_test,t.s_FixedAcidity,t.s_VolatileAcidity,t.s_CitricAcid,
 t.s_ResidualSugar,t.s_Chlorides,t.s_FreeSulfurDioxide,t.s_TotalSulfurDioxide,t.s_Density,
 t.s_Sulphates,t.a_Alcohol,t.s_AcidIndex)

str(wine_test.2)


t.keep.vars <- c('INDEX','TARGET','t.s_FixedAcidity','t.s_VolatileAcidity','t.s_CitricAcid',
               't.s_ResidualSugar','t.s_Chlorides','t.s_FreeSulfurDioxide','t.s_TotalSulfurDioxide',
               't.s_Density','pH','t.s_Sulphates','t.a_Alcohol','LabelAppeal','t.s_AcidIndex',
               'STARS');

# define skinny data frame
wine_test.3 <- wine_test.2[,t.keep.vars];

str(wine_test.3)
summary(wine_test.3)

# create data frame
wine_test.3 <- data.frame(wine_test.3)

anyNA(wine_test.3)


# missing values
library(mice)
t.missingvalues <- mice(wine_test.3[, !names(wine_test.3) %in% c("TARGET")], method="rf")
t.miceOutput <- complete(t.missingvalues)  # generate the completed data


mynewdata_test <- t.miceOutput
str(mynewdata_test)
anyNA(mynewdata_test)


#### Hard-coded TARGET model ####

temp <- 1.286912+
  0.007480*mynewdata_test$t.a_Alcohol+
  0.251815*mynewdata_test$LabelAppeal-
  0.100248*mynewdata_test$t.s_AcidIndex+
  0.093813*mynewdata_test$STARS

P_SCORE_ZIP_ALL = exp(temp)

temp2 <- -8.479680+
  0.010764*mynewdata_test$t.a_Alcohol+
  0.357872*mynewdata_test$LabelAppeal+
  2.899377*mynewdata_test$t.s_AcidIndex-
  0.712827*mynewdata_test$STARS

P_SCORE_ZERO <- exp(temp2)/(1+exp(temp2))

P_SCORE_ZIP <- P_SCORE_ZIP_ALL * (1-P_SCORE_ZERO)

P_TARGET <- round(P_SCORE_ZIP,0)


wine_test.final <- cbind.data.frame(mynewdata_test, P_TARGET)
wine_test.final.1 <- as.data.frame(wine_test.final[,c('INDEX','P_TARGET')])
str(wine_test.final.1)

write.csv(wine_test.final.1,file="wine_SCORED_Cory Baumgarten_FINAL.csv")

