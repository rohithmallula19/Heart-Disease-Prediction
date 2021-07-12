
#PROJECT: HEART DISEASE PREDICTION>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#age: The person's age in years
#sex: The person's sex (1 = male, 0 = female)
#cp: The chest pain experienced (Value 1: typical angina, Value 2: atypical angina, Value 3: non-anginal pain, Value 4: asymptomatic)
#trestbps: The person's resting blood pressure (mm Hg on admission to the hospital)
#chol: The person's cholesterol measurement in mg/dl
#fbs: The person's fasting blood sugar (> 120 mg/dl, 1 = true; 0 = false)
#restecg: Resting electrocardiographic measurement (0 = normal, 1 = having ST-T wave abnormality, 2 = showing probable or definite left ventricular hypertrophy by Estes' criteria)
#thalach: The person's maximum heart rate achieved
#exang: Exercise induced angina (1 = yes; 0 = no)
#oldpeak: ST depression induced by exercise relative to rest ('ST' relates to positions on the ECG plot. 
#slope: the slope of the peak exercise ST segment (Value 1: upsloping, Value 2: flat, Value 3: downsloping)
#ca: The number of major vessels (0-3)
#thal: A blood disorder called thalassemia (3 = normal; 6 = fixed defect; 7 = reversable defect)
#target: Heart disease (0 = no, 1 = yes)
library(pacman)
pacman::p_load(dplyr,plyr,ggplot2,caTools,polycor,corrplot,mctest,gridExtra,MASS,caret,pROC)

heart <- read.csv(file.choose())
heart$target <- as.factor(heart$target)
heart$sex <- as.factor(heart$sex)
heart$cp <- as.factor(heart$cp)
heart$fbs <- as.factor(heart$fbs)
heart$restecg <- as.factor(heart$restecg)
heart$exang <- as.factor(heart$exang)
heart$thal <- as.factor(heart$thal)
heart$slope <- as.factor(heart$slope)
heart$ca <- as.factor(heart$ca)


hetcorr <- hetcor(heart)
hetcorr
hetcor_correl <-hetcorr$correlations
hetcor_correl

corrplot(hetcor_correl, type = "upper", order = "original", 
         tl.col = "black", tl.srt = 90, method = "number")

#Positive correlations are shown in blue, and negative correlations in red. 
# And the intensity of the color is proportional to the correlation coefficients. 

#There is a negative correlation of -0.65 between the target and the variable exang.
#There is a negative correlation of -0.54 between the target and the variable oldpeak.
#There is a negative correlation of -0.46 and -0.45 between the target and the variables ca and thal respectively.
#There is a positve correlation of 0.57 between the target and the variable cp.
#There is a positve correlation of 0.52 between the target and the variable thalach.
#There is a positve correlation of 0.48 between the target and the variable slope.

#There is a negative correlation of -0.57 between the variables exang and cp.
#There is a negative correlation of -0.63 between the variables oldpeak and slope.

#EVALUATION OF MULTICOLLINEARITY >>>>>>>>>>>>>>>>>>

x<- heart[, -14]
y <- heart[, 14]

#imcdiag(x, y,method = "VIF", vif = 10, corr = FALSE)
collinearity <- imcdiag(x, y)
collinearity
mc.plot(x,y)

#The VIF diagnostics indicates that there is not multicollinearity between the variables. 
#DATA EXPLORATION ANALYSIS BASED ON CORRELATION RESULTS >>>>>>>>>>>>

sum(is.na(heart))
#There are not missing values in the dataset

#Target and chest pain (cp)
chespain1<- ggplot(heart, aes(cp, fill=target)) + geom_bar(stat="count", position= "fill") + labs(x="Chest Pain Type", fill= "Target", y="Stacked Count")
chespain
#Chestpain type 1 = Typical angina has the biggest incidence of heart disease compared to the others. However, the graph indicates that cp type 2 and 3 have an important relationship to heart disease.

#Target and Thalach (maximum heart rate achieved)
t1<- ggplot(heart, aes(thalach, col=target, fill=target))+ geom_density(alpha=0.3)+ guides(col=F)+ labs(fill="Target", x="Maximum heart rate achieved")
t2<- ggplot(heart, aes(target, thalach, fill=target))+ geom_boxplot() + labs(y="Maximum heart rate achieved", x="Target", fill="Target")

#Heart disease count is present when the thalach is over 150. The median of this variable is 153
summary(heart$thalach)

#Target and thal
thal1<- ggplot(heart, aes(thal, fill=target)) + geom_bar(stat="count", position= "fill") + labs(x="Thalium stress test level", fill= "Target", y="Stacked Count")
#The stress level of 2 indicates the major incidence to have heart disease.

#Target and ca
ca1<- ggplot(heart, aes(ca, fill=target)) + geom_bar(stat="count", position= "fill") + labs(x="Number of mayor vessels", fill= "Target", y="Stacked Count")
ca1
#0 and 3 vessels indicate a major occurences of a heart disease

#Target and oldpeak
op1<- ggplot(heart, aes(oldpeak, col=target, fill=target))+ geom_density(alpha=0.4)+ guides(col=F)+ labs(fill="Target", x="oldpeak")
op2<- ggplot(heart, aes(target, oldpeak, fill=target))+ geom_boxplot() + labs(y="Oldpeak", x="Target", fill="Target")
grid.arrange(op1, op2, nrow=1)
#Lower Oldpeak (or depression) indicates heart disease

#Target and slope
slope1 <- ggplot(heart, aes(slope, fill=target)) + geom_bar(stat="count", position= "fill") + labs(x="Slope", fill= "Target", y="Stacked Count")
#slope of Level 2 =  indication of a heart disease. 

#Target and restecg
rest1<- ggplot(heart, aes(restecg, fill=target)) + geom_bar(stat="count", position= "fill") + labs(x="Resting electrocardiographic measurement", fill= "Target", y="Stacked Count")
#1 indicates having ST-T wave abnormality. The majority of observations with a heart disease had this measure. Interestingly, some of the patients with a normal level of 0 had a heart disease event. 

#Target and exang
exa<- ggplot(heart, aes(exang, fill=target)) + geom_bar(stat="count", position= "fill") + labs(x="Exercise Indiced angina", fill= "Target", y="Stacked Count")
#The level 0=no exercise angina indicate a count with patients with heart disease compared to level 1 = yes

#Target and sex
sex<- ggplot(heart, aes(sex, fill=target)) + geom_bar(stat="count", position= "fill") + labs(x="Genre", fill= "Target", y="Stacked Count")
#women had more incidence of heart disease compared to men.

table(heart$sex)# Inthe dataset there are 207 men and 96 women

#Exang and cp
excp <- ggplot(heart, aes(cp, fill=exang)) + geom_bar(stat="count", position= "fill") + labs(x="cp", fill= "exang", y="Stacked Count")
#For a typical angina there is more incidence to have an exercise induced angina

#oldpeak and slope
o<-ggplot(heart, aes(x=slope, y=oldpeak))+ geom_point(aes(colour=target)) + labs(y="Oldpeak", x="Slope", fill="Target")
#We could identify that there is more incidence of heart disease for patients with a slope of 2 and low oldpeak 

grid.arrange(chespain, t1, t2, thal1, ca1, op1, op2, slope1, rest1, exa1, sex1, excp, o, nrow=5)


#LOGISTIC REGRESSION MODEL
#Original Logistic Regression Model
hmodel1 <- glm(target ~., family = "binomial", data = heart)
summary(hmodel1) #The AIC is 225.63

heart_mod_red <- step(hmodel1, direction = "backward")
#According to the Stepwise Algorithm, the lowest AIC is achieved by dropping the variables age, fbs, chol, and restecg
#These variables where suggested by the imndiag() function as non-significant 
#The AIC is 219.79


#newheart has only the data with required constraints/regressors
new_heart <- data.frame(exang=heart$exang, slope=heart$slope, trestbps=heart$trestbps, oldpeak=heart$oldpeak, thal=heart$thal, sex=heart$sex, cp= heart$cp, ca=heart$ca, target=heart$target)
hmodel2 <- glm(target ~., family = "binomial", data = new_heart)
summary(hmodel2)


exp(coef(hmodel2))
exp(-1.63154) #if all regressors are held at a fixed value, the odds of getting heart disease for males (sex=1) over the odds of getting heart disease for females is exp(-1.63154) = 0.1956

#ANOVA ANALYSIS 
lr.anova <- anova(hmodel1, test="Chisq")
lr.anova

#Deviance is a measure of the lack of fit between the model and the data with larger values indicating poorer fit
#The p-values are calculated using the chi-squared distribution, but like the parametric alternative they
#indicate whether or not each of the predictors has a significant effect on the probability of achieving an indicator value of 1. 
#In this model we see that the variables chol, fbs, and restecg are not significant for the model. 

lr.anova_red <- anova(hmodel2, test="Chisq")
lr.anova_red
#All the variables look significant for the model.
#We can see that this model has a lower AIC, so this is a better logistic model.

anova(hmodel1, hmodel2, test="Chisq") #Fail in reject the null hypothesis. The reduced model is better than the original model

#PREDICTION OF THE PROBABILITY OF HEART DISEASE BY A SIMPLE LOGISTIC REGRESSION MODEL WITH A BINARY RESPONSE VARIABLE
na <- sum(is.na(heart))
#zero NAs


set.seed(142)
train_data <- createDataPartition(new_heart$target, p=0.7, list=FALSE)
trainData<- new_heart[train_data,]
testData <- new_heart[-train_data, ]

table(testData$target)
table(trainData$target)

set.seed(121)
heart_mod_train <- train(target ~., method='glm', family = "binomial", data = trainData )
heart_pred <- predict(heart_mod_train, testData)
heart_pred_prob <- predict(heart_mod_train, testData, type = "prob")
head(heart_pred_prob)

confMatr <- confusionMatrix(heart_pred, testData[,9])
confMatr
#The variables from the reduced model predict the probability to have a heart disease (or not) with an accuracy of 83%

set.seed(100)
train_data2 <- createDataPartition(heart$target, p=0.7, list=FALSE)
trainData2<- heart[train_data2,]
testData2 <- heart[-train_data2, ]

table(testData2$target)
table(trainData2$target)


set.seed(11)
heart_mod_train2 <- train(target ~., method='glm', family = "binomial", data = trainData2 )
heart_pred2 <- predict(heart_mod_train2, newdata=testData2)
heart_pred_prob2 <- predict(heart_mod_train2, newdata=testData2, type = "prob")
head(heart_pred_prob2)

confMatr2 <- confusionMatrix(heart_pred2, testData2[,14])
confMatr2

##The variables from the ORIGINAL dataset(all the variables included) predict the probability to have a heart disease (or not) with an accuracy of 86%.
##The reduced model resulted in a prediction accuracy of 90%

#ACCURACY EVALUATION >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

rocCurve <- roc(response=testData2$target, predictor=heart_pred_prob2$`1`)

plot(rocCurve, legacy.axes=TRUE)x
auc(rocCurve)

rocCurve2 <- roc(response=testData$target, predictor=heart_pred_prob$`1`)

plot(rocCurve2, legacy.axes=TRUE)
auc(rocCurve)
















