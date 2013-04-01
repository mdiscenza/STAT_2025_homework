




######################################################
###### UNCOVERTED DATA - MODEL FITTING
######################################################
str(study_data)
colnames(study_data)
#do "predictors" have predictive power?
summary(study_data)
#there is no significant variation in the "know_about" variables (HOW TO QUANTIFY). So we will not include those four predictors in the regression.


#Running Logistic Regression 1 (Analysis 1) - the full model
########################################
full_model<-sex_wo_contraception~age+ethnicity+religion+highest_yr_school_completed+know_preg_peera+fathers_education+mothers_education+educational_aspiration+dont_participate_in_school_activites_1+smoking_freq+drinking_freq+drug_freq

lr1<-glm(full_model,data=study_data,family=binomial(link=logit))
summary(lr1)
AIC(lr1)
plot(lr1)



#Lasso (Analysis 2)
########################################
library(glmnet)
# to use glmnet, we need to recode the factor variables as binary
model.matrix(full_model,data=study_data,contrasts.arg=list(a=ethnicity,b=religion))
y<-as.matrix(study_data[,1])
x_mat <- model.matrix(full_model, data=study_data )
#running 
glmnetModel <- glmnet(x_mat,y,family="binomial",alpha=1)
plot(glmnetModel)
my.cv <- cv.glmnet(x_mat,y,family="binomial")
predict(glmnetModel,type="coef",s=my.cv$lambda.min)

#Leaps (Analysis 3)
########################################
library(leaps)
model_spec <- regsubsets(x_mat,y,method="backward")
model_spec

#this didn't work, need to be in matrix form
x=as.matrix(study_data[,2:15])
y=as.matrix(study_data[,1])
model_spec2 <- regsubsets(x,y,method="backward")




glmnetModel <- glmnet(x=as.matrix(ToyExample[,2:4]),y=ToyExample[,1], family="binomial")
#plot(glmnetModel)
my.cv <- cv.glmnet(x=as.matrix(ToyExample[,2:4]),y=ToyExample[,1],family="binomial")
predict(glmnetModel,type="coef",s=my.cv$lambda.min) # returns the coefficents for the best model
