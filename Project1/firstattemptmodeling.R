#do "predictors" have predictive power?
summary(study_data)
#there is no significant variation in the "know_about" variables (HOW TO QUANTIFY)



#Running Logistic Regression (Analysis 1)
lr1<-glm(sex_w_contraception~know_about_pill+know_about_condoms+know_about_diaphram+know_about_withdrawl+age+ethnicity+religion+highest_yr_school_completed+know_preg_peera+birth_control_used_by_peers+fathers_education+mothers_education+educational_aspiration+dont_participate_in_school_activites_1+smoking_freq+drinking_freq+drug_freq+age_frist_drinking_a+age_frist_drinking_b+birth_control_used_by_peers,data=study_data,family=binomial(link=logit))
summary(lr1)
AIC(lr1)
plot(lr1)

#Reression with no "know_about" variables 
plotlr2<-glm(sex_w_contraception~age+ethnicity+religion+highest_yr_school_completed+know_preg_peera+birth_control_used_by_peers+fathers_education+mothers_education+educational_aspiration+dont_participate_in_school_activites_1+smoking_freq+drinking_freq+drug_freq+age_frist_drinking_a+age_frist_drinking_b+birth_control_used_by_peers,data=study_data,family=binomial(link=logit))
summary(lr2)
AIC(lr2)
#fucking around
lr3<-glm(sex_w_contraception~age+ethnicity+religion+highest_yr_school_completed,data=study_data,family=binomial(link=logit))
summary(lr3)











#Lasso (Analysis 2)
install.packages("glmnet")
library(glmnet)
#creating matricies
x<-as.matrix(study_data[,2:20])
y<-as.matrix(study_data[,1])
fit1<-glmnet(x,y)
#finding lambda.min
cv.res<-cv.glmnet(x,y,nfolds=5,family="binomial")