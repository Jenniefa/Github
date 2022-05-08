 #Load data
MyData<- read.csv("diabetes2.csv")
head(MyData)
summary(MyData)

#Clean data
#Check missing values
any(is.na(MyData))
#check null values
any(is.null(MyData))
#Results is False meaning there are no missing values and no null value

#Test outliers of each variable
install.packages("outliers")
library(outliers)
outlier(MyData)
hist(MyData$Pregnancies, xlab= "Pregnancies")
hist(MyData$Glucose, xlab= "Glucose")
hist(MyData$BloodPressure, xlab= "BloodPressure")
hist(MyData$SkinThickness, xlab= "Skin Thickness")
hist(MyData$Insulin, xlab= "Insulin")
hist(MyData$BMI, xlab= "BMI")
hist(MyData$DiabetesPedigreeFunction, xlab= "Diabetes Pedigree Function")
hist(MyData$Age, xlab= "Age")


#Replace outlier with median
MyData$BloodPressure[MyData$BloodPressure %in% 0]<-median(MyData$BloodPressure)
MyData$Glucose[MyData$Glucose %in% 0]<-median(MyData$Glucose)
MyData$SkinThickness[MyData$SkinThickness%in% 0]<-median(MyData$SkinThickness)
MyData$Insulin[MyData$Insulin %in% 0]<-median(MyData$Insulin)
MyData$BMI[MyData$BMI %in% 0]<-median(MyData$BMI)
summary(MyData)

library(outliers)
outlier(MyData)

#Fit a model
model1 <- glm(Outcome ~.,family="binomial", data = MyData)
summary(model1)
#From summary we can see all independent variables are significant except BloodPressure,SkinThickness,Age and Insulin

#choose the best model

model2 <- glm(Outcome ~.-BloodPressure, family = "binomial", data = MyData)
summary(model2)
#The AIC decreases and Residual deviance increases hence we will remove BloodPressure

model3 <- glm(Outcome ~.-SkinThickness, family = "binomial", data = MyData)
summary(model3)
#The AIC decreases hence we can remove SkinThickness from dataset

model4 <- glm(Outcome ~.-Insulin, family = "binomial", data = MyData)
summary(model4)
#The residual deviance increases while AIC decreases by a very small amount hence we will not remove Insulin


model5 <- glm(Outcome ~.-Age, family = "binomial", data = MyData)
summary(model5)
#The AIC decreases hence we remove age

#Final model
model6<-glm(Outcome~.-Age-BloodPressure-SkinThickness,family="binomial", data=MyData)
summary(model6)

#goodness of fit
glm(Outcome~.-Age-BloodPressure-SkinThickness,family="binomial", data=MyData)

#oddsratio
exp(cbind(OR = coef(model6),confint(model6)))
with(Mydata, null.deviance - deviance)
    
    