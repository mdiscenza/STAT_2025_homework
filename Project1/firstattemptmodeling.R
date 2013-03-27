
str(study_data)
colnames(study_data)
#do "predictors" have predictive power?
summary(study_data)
#there is no significant variation in the "know_about" variables (HOW TO QUANTIFY). So we will not include those four predictors in the regression.


#Running Logistic Regression 1 (Analysis 1) - the full model
########################################
full_model<-sex_wo_contraception~age+ethnicity+religion+highest_yr_school_completed+know_preg_peera+fathers_education+mothers_education+educational_aspiration+dont_participate_in_school_activites_1+smoking_freq+drinking_freq+drug_freq+age_frist_drinking_a+age_frist_drinking_b+birth_control_used_by_peers

lr1<-glm(full_model,data=study_data,family=binomial(link=logit))
summary(lr1)
AIC(lr1)
plot(lr1)



#Lasso (Analysis 2)
########################################
# to use glmnet, we need to recode the factor variables as binary
model.matrix(full_model,data=study_data,contrasts.arg=list(a=ethnicity,b=religion))
x_mat <- model.matrix(full_model, data=study_data )

library(glmnet)
#creating matricies
x<-as.matrix(study_data[,2:10])
y<-as.matrix(study_data[,1])
#running 
glmnetModel <- glmnet(x_mat,y,family="binomial",alpha=1)
plot(glmnetModel)
my.cv <- cv.glmnet(x_mat,y,family="binomial")
predict(glmnetModel,type="coef",s=my.cv$lambda.min)

glmnetModel <- glmnet(x=as.matrix(ToyExample[,2:4]),y=ToyExample[,1], family="binomial")
#plot(glmnetModel)
my.cv <- cv.glmnet(x=as.matrix(ToyExample[,2:4]),y=ToyExample[,1],family="binomial")
predict(glmnetModel,type="coef",s=my.cv$lambda.min) # returns the coefficents for the best model


table(study_data$smoking_freq)

model.matrix