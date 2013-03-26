#do "predictors" have predictive power?
summary(study_data)
#there is no significant variation in the "know_about" variables (HOW TO QUANTIFY). So we will not include those four predictors in the regression.

#Running Logistic Regression 1 (Analysis 1)
lr1<-glm(sex_w_contraception~age+ethnicity+religion+highest_yr_school_completed+know_preg_peera+fathers_education+mothers_education+educational_aspiration+dont_participate_in_school_activites_1+smoking_freq+drinking_freq+drug_freq+age_frist_drinking_a+age_frist_drinking_b+birth_control_used_by_peers,data=study_data,family=binomial(link=logit))
summary(lr1)
AIC(lr1)
plot(lr1)


#Lasso (Analysis 2)
library(glmnet)
#creating matricies
x<-as.matrix(study_data[,2:10])
y<-as.matrix(study_data[,1])
#running 
cv.res<-cv.glmnet(x,y,family="binomial")
str(x)
x_new <- as.factor(x)

table(study_data$smoking_freq)

model.matrix