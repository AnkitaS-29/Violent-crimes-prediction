#### Data Preprocessing ####
data = read.csv(file.choose()) # Reading the data 
str(data) 
summary(data)

na <- data.frame(apply(data,2,FUN = fun1)) #gives each column with no.of NA values
fun1 <- function(x){
  return(sum(is.na(x)))    #Function to count the no. of NA's 
} 
# Steps for deleting the variables with more than 1 NA values
name <- data.frame(row.names(na))      
name$count <- na[1]
na_col<- name[name$count>1,]
l<- c(as.character((na_col$row.names.na.))) #Gives names of the variables with NA values
data_nna <- data[!names(data) %in% l]  
sum(is.na(data_nna))  #1 record in OtherPerCap has an NA values
library(DMwR)
data_n <- centralImputation(data_nna) # Applying central imputation
data_n$state <- as.factor(data_n$state) #Type conversion of state attribute

data_n1 <- data_n[-c(1,2)]  #Deleting non-numeric columns state and community

#### data pattern ####
ggplot()+geom_density(aes(data_n1$ViolentCrimesPerPop)) #The data has a positive skew
boxcox(ViolentCrimesPerPop+0.0001 ~. ,data = train,lambda = seq(0.3,0.35,0.01)) 
ggplot()+geom_density(aes((data_n1$ViolentCrimesPerPop)^0.32))


#### Data split ####
library(caret)
rec <- createDataPartition(y = data_n1$ViolentCrimesPerPop, times=1,list = F,p = 0.7)
train <- data_n1[rec,]
valid <- data_n1[-rec,]


#### Checking for high correlations among the attributes
data_n_76_90 <- data_n1[78:88]
data_n_76_90$target <- train$ViolentCrimesPerPop
corrplot::corrplot(cor(data_n_76_90),method = "number")
# It is observed that, 

#### Attributes highly correlated with Target 
cor <- apply(train,2,FUN = fun_corr)
fun_corr <- function(x){
  cor(data_n1$ViolentCrimesPerPop,x)
}
cor_sort<-data.frame('cor_value' = sort(abs(cor),decreasing = T))
write.csv(cor_sort,"cor_sort.csv",row.names=T)



#### Model 1 - Simple multi linear regression model ####
lm_simple <- lm(ViolentCrimesPerPop ~. ,data = train )
summary(lm_simple)
regr.eval(train$ViolentCrimesPerPop,(lm_simple$fitted.values)^3.22)
regr.eval(valid$ViolentCrimesPerPop,predict(lm_simple,newdata = valid))
shapiro.test(lm_simple$residuals)
par(mfrow = c(2,2))
plot(lm_simple)


#### Model- 2 : Model built with highly correlated predictor variable ####

cor <- apply(train,2,FUN = fun_corr)
mode(cor)
fun_corr <- function(x){
  cor(train$ViolentCrimesPerPop,x)
}
cor_sort<-data.frame('cor_value' = sort(abs(cor),decreasing = T))
cor_sort$name <- row.names(cor_sort)
cor_sort$name[cor_sort$cor_value >= 0.45]
lm_cor <- lm(ViolentCrimesPerPop ~ PctKids2Par+PctIlleg+PctFam2Par+racePctWhite+
               PctYoungKids2Par+PctTeen2Par+racepctblack+pctWInvInc+pctWPubAsst+
               FemalePctDiv+TotalPctDiv+MalePctDivorce+PctPersOwnOccup+PctPopUnderPov+
               PctUnemployed+PctHousNoPhone+PctNotHSGrad+PctVacantBoarded
             +PctHousLess3BR+PctHousOwnOcc+NumIlleg+PctPersDenseHous, data = train)

summary(lm_cor)
regr.eval(train$ViolentCrimesPerPop,lm_cor$fitted.values)
regr.eval(valid$ViolentCrimesPerPop,predict(lm_cor,newdata = valid))
shapiro.test(lm_cor$residuals)
par(mfrow=c(2,2))
plot(lm_cor)


#### Model 3_1 - Reducing the skewness in data by applying box-cox transformation ####

ggplot()+geom_density(aes(data_n1$ViolentCrimesPerPop)) #The data has a positive skew
boxcox(ViolentCrimesPerPop+0.0001 ~. ,data = train,lambda = seq(0.3,0.35,0.01)) 
ggplot()+geom_density(aes((data_n1$ViolentCrimesPerPop)^0.32))

lm_simple_box <- lm((ViolentCrimesPerPop)^0.32 ~. ,data = train )
summary(lm_simple_box)
regr.eval(train$ViolentCrimesPerPop,(lm_simple_box$fitted.values)^3.125)
regr.eval(valid$ViolentCrimesPerPop,(predict(lm_simple_box,newdata = valid))^3.125)
shapiro.test(lm_simple_box$residuals)
par(mfrow =c(2,2))
plot(lm_simple_box)

#### Model 3 _2 - Reducing the skewness in data by applying box-cox transformation ####
# Highly correlated variables 
library(MASS)
boxcox(ViolentCrimesPerPop+0.00001 ~PctKids2Par+PctIlleg+PctFam2Par+racePctWhite+
         PctYoungKids2Par+PctTeen2Par+racepctblack+pctWInvInc+pctWPubAsst+
         FemalePctDiv+TotalPctDiv+MalePctDivorce+PctPersOwnOccup+PctPopUnderPov+
         PctUnemployed, data = train1 ,lambda = seq(0.25,0.38,0.01))
### Ideal power for target is 0.31 , transforming Y and building model
lm_cor_box <- lm((ViolentCrimesPerPop)^0.31 ~ PctKids2Par+PctIlleg+PctFam2Par+racePctWhite+
               PctYoungKids2Par+PctTeen2Par+racepctblack+pctWInvInc+pctWPubAsst+
               FemalePctDiv+TotalPctDiv+MalePctDivorce+PctPersOwnOccup+PctPopUnderPov+
               PctUnemployed, data = train1)

summary(lm_cor_box)
regr.eval(train1$ViolentCrimesPerPop,(lm_cor_box$fitted.values)^3.22)
regr.eval(valid$ViolentCrimesPerPop,(predict(lm_cor_box,newdata = valid))^3.22)
shapiro.test(lm_cor_box$residuals)
par(mfrow = c(2,2))
plot(lm_cor_box)
data.frame(train$ViolentCrimesPerPop,lm_cor_box$fitted.values)


#### Model 4 using step AIC ####

stepOut <- stepAIC(lm_simple_box,direction = "both")
lm_Box_step <- lm((ViolentCrimesPerPop)^0.32~population + racepctblack + racePctAsian + 
                    racePctHisp + agePct16t24 + numbUrban + pctUrban + medIncome + 
                    pctWWage + pctWInvInc + pctWRetire + medFamInc + OtherPerCap + 
                    PctPopUnderPov + PctEmploy + PctEmplManu + MalePctDivorce + 
                    MalePctNevMarr + PersPerFam + PctFam2Par + PctKids2Par + 
                    PctWorkMom + NumIlleg + PctIlleg + PctImmigRecent + PctImmigRec5 + 
                    PctRecImmig8 + PctRecImmig10 + PctNotSpeakEnglWell + PersPerOccupHous + 
                    PersPerOwnOccHous + PctHousLess3BR + HousVacant + PctVacMore6Mos + 
                    PctHousNoPhone + PctWOFullPlumb + OwnOccLowQuart + RentLowQ + 
                    MedRent + MedOwnCostPctIncNoMtg + NumStreet + PctForeignBorn + 
                    PctSameState85 + LemasPctOfficDrugUn, data = train)
summary(lm_Box_step)
regr.eval(train$ViolentCrimesPerPop,(lm_Box_step$fitted.values)^3.125)
regr.eval(valid$ViolentCrimesPerPop,(predict(lm_Box_step,newdata = valid))^3.125)
shapiro.test(lm_Box_step$residuals)
par(mfrow = c(2,2))
plot(lm_Box_step)
data.frame(train$ViolentCrimesPerPop,(lm_Box_step$fitted.values)^3.125)
summary((lm_Box_step$fitted.values)^3.125)


#### Model 5 using step AIC & VIF ####

stepOut <- stepAIC(lm_simple_box,direction = "both")
vif1 <- vif(stepOut)
vif2 <- data.frame('name'=names(sort(vif(stepOut),decreasing = T)),
                   'Vif'=sort(vif(stepOut),decreasing = T))
row.names(vif2) <- NULL
lm_box_step_vif <- lm((ViolentCrimesPerPop)^0.32~ racePctHisp+HousVacant+
                        agePct16t24 +NumIlleg+PctImmigRec5+PctHousNoPhone+
                         + PctHousLess3BR+MalePctDivorce+racepctblack+ 
                        PctImmigRecent+ pctUrban+ racePctAsian+pctWRetire +
                        PctWorkMom+PctSameState85+PctWOFullPlumb+NumStreet+ 
                        MedOwnCostPctIncNoMtg+PctVacMore6Mos+ PctEmplManu+ 
                        LemasPctOfficDrugUn+OtherPerCap, data= train)
summary(lm_box_step_vif)
regr.eval(train$ViolentCrimesPerPop,(lm_box_step_vif$fitted.values)^3.125)
regr.eval(valid$ViolentCrimesPerPop,(predict(lm_box_step_vif,newdata = valid))^3.125)
shapiro.test(lm_box_step_vif$residuals)
par(mfrow = c(2,2))
plot(lm_box_step_vif)
df <- data.frame(train$ViolentCrimesPerPop,(lm_box_step_vif$fitted.values)^3.125)
summary((lm_box_step_vif$fitted.values)^3.125)
sort(df)
View(df)



#### Residual anomality ####
# records 376, 773, 1228 seem to have high leverage and residuals
train[376,]
train[which.max()]
lev_step <- hatvalues(lm_Box_step)
res_step <- lm_Box_step$residuals
cook_step <- cooks.distance(lm_Box_step)
lr <- data.frame(lev_step,res_step,cook_step)
train[which.max(lr$lev_step),]
View(lr)




####  #####
#### Final Model new using step AIC ####
## removing numStreet and influencial point
train1<- train[-c(1033,343,1228),]
stepOut <- stepAIC(lm_simple_box,direction = "both")
lm_Box_step_final <- lm((ViolentCrimesPerPop)^0.32~population + racepctblack + racePctAsian + 
                    racePctHisp + agePct16t24 + numbUrban + medIncome + 
                    pctWWage + pctWInvInc + pctWRetire + medFamInc + OtherPerCap + 
                    PctPopUnderPov + PctEmploy + PctEmplManu + MalePctDivorce + 
                    MalePctNevMarr + PersPerFam + PctFam2Par + PctKids2Par + 
                    PctWorkMom + NumIlleg + PctIlleg + PctImmigRecent + PctImmigRec5 + 
                    PctRecImmig8 + PctRecImmig10 + PctNotSpeakEnglWell + PersPerOccupHous + 
                    PersPerOwnOccHous + PctHousLess3BR + HousVacant + PctVacMore6Mos + 
                    PctHousNoPhone + PctWOFullPlumb + OwnOccLowQuart + RentLowQ + 
                    MedRent + MedOwnCostPctIncNoMtg + PctForeignBorn + 
                    PctSameState85 + LemasPctOfficDrugUn, data = train1)
summary(lm_Box_step)

regr.eval(train1$ViolentCrimesPerPop,(lm_Box_step$fitted.values)^3.125)
regr.eval(valid$ViolentCrimesPerPop,(predict(lm_Box_step,newdata = valid))^3.125)
shapiro.test(lm_Box_step_final$residuals)
par(mfrow = c(2,2))
plot(lm_Box_step_final)
data.frame(train$ViolentCrimesPerPop,(lm_Box_step$fitted.values)^3.125)
summary((lm_Box_step$fitted.values)^3.125)

