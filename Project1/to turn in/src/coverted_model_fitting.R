
######################################################
###### COVERTED DATA - MODEL FITTING
######################################################
str(study_data_converted)
colnames(study_data_converted)
#do "predictors" have predictive power?
summary(study_data_converted)
#there is no significant variation in the "know_about" variables (HOW TO QUANTIFY). So we will not include those four predictors in the regression.


#Running Logistic Regression 1 (Analysis 1) - the full model
########################################
full_model<-sex_wo_contraception~age+ethnicity+religion+highest_yr_school_completed+know_preg_peera+fathers_education+mothers_education+educational_aspiration+dont_participate_in_school_activites_1+smoking_freq+drinking_freq+drug_freq+age_frist_drinking

lr1c<-glm(full_model,data=study_data_converted,family=binomial(link=logit))
summary(lr1c)
#explained deviance:
(lr1c$null.deviance - lr1c$deviance)/lr1c$null.deviance

AIC(lr1c)
plot(lr1c)

partial_model<-sex_wo_contraception~age+ethnicity+highest_yr_school_completed+know_preg_peera+fathers_education+mothers_education+educational_aspiration+dont_participate_in_school_activites_1+smoking_freq+drinking_freq+drug_freq
lr2c<-glm(partial_model,data=study_data_converted,family=binomial(link=logit))
summary(lr2c)
AIC(lr1c)
plot(lr1c)

#Lasso (Analysis 2)
########################################
library(glmnet)
# to use glmnet, we need to recode the factor variables as binary
yc<-as.matrix(study_data_converted[,1])
x_matc <- model.matrix(partial_model, data=study_data_converted)
#running 
glmnetModelc_lasso <- glmnet(x_matc,yc,family="binomial",alpha=1)
plot(glmnetModelc_lasso)
my.cvc <- cv.glmnet(x_matc,yc,family="binomial")
predict(glmnetModelc_lasso,type="coef",s=my.cvc$lambda.min)
glmnetModelc_lasso$nulldev
#explained.deviance:
(glmnetModelc_lasso$nulldev - glmnetModelc_lasso$nulldev)/glmnetModelc_lasso$nulldev
my.cvc$

glmnetModelc_ridge <- glmnet(x_matc,yc,family="binomial",alpha=0)
plot(glmnetModelc_ridge)
my.cvc <- cv.glmnet(x_matc,yc,family="binomial")
predict(glmnetModelc_ridge,type="coef",s=my.cvc$lambda.min)


#Leaps (Analysis 3)
##########################################################################################
##########################################################################################
full_model<-sex_wo_contraception~age+ethnicity+religion+highest_yr_school_completed+know_preg_peera+fathers_education+mothers_education+educational_aspiration+dont_participate_in_school_activites_1+smoking_freq+drinking_freq+drug_freq
library(leaps)
###############################
# One best at each size
#############################
##########################forward
subsets_forward <- regsubsets(sex_wo_contraception~age+ethnicity+religion+highest_yr_school_completed+know_preg_peera+fathers_education+mothers_education+educational_aspiration+dont_participate_in_school_activites_1+smoking_freq+drinking_freq+drug_freq, data=study_data_converted, method="forward", nbest=1)
plot(subsets_forward)
sum_forward <- summary(subsets_forward)
sum_forward$outmat
#results:
#religion2
#smoking freq partake
# drinking freq no response
#smoking noresponse
#ethnicity 4
#mothers_educationgrad.hs.some.college

### tetsing the results ####
x_matc <- as.data.frame(model.matrix(full_model, data=study_data_converted))
leaps_5var_forward<-glm(yc~x_matc$religion2 +x_matc$smoking_freqpartake +x_matc$drug_freqno_response + x_matc$smoking_freqno_response +x_matc$ethnicity4 +x_matc$mothers_educationgrad.hs.some.college,data=study_data_converted,family=binomial(link=logit))
summary(leaps_5var_forward)

##########################Backward
subsets_backward <- regsubsets(sex_wo_contraception~age+ethnicity+religion+highest_yr_school_completed+know_preg_peera+fathers_education+mothers_education+educational_aspiration+dont_participate_in_school_activites_1+smoking_freq+drinking_freq+drug_freq, data=study_data_converted, method="backward", nbest=1)
plot(subsets_backward)
summary(subsets_backward)$outmat
#results:
#smoking
#drinking_freqno_response
#mothers_educationgrad.hs.some.college
#ethnicity4
#religion5
leaps_5var_backward<-glm(yc~x_matc$smoking_freqpartake +x_matc$drinking_freqno_response +x_matc$mothers_educationgrad.hs.some.college + x_matc$ethnicity4 +x_matc$religion5,data=study_data_converted,family=binomial(link=logit))
summary(leaps_5var_forward)


##########################
## now concerned about 5 best 1 variable models
#############################
#########################forward
subsets_forward <- regsubsets(sex_wo_contraception~age+ethnicity+religion+highest_yr_school_completed+know_preg_peera+fathers_education+mothers_education+educational_aspiration+dont_participate_in_school_activites_1+smoking_freq+drinking_freq+drug_freq, data=study_data_converted, method="forward", nbest=5)
plot(subsets_forward)
sum_forward <- summary(subsets_forward)
sum_forward$outmat
#results:
#1-religion 2
#2 -smoking_freqpartake
#3 - mothers_educationgrad.hs.some.college
# 4 -mothers_educationgrad.college.beyond
leaps_1var_forward1<-glm(yc~x_matc$religion2,family=binomial(link=logit))
summary(leaps_1var_forward1)
leaps_1var_forward2<-glm(yc~x_matc$smoking_freqpartake,family=binomial(link=logit))
summary(leaps_1var_forward2)
leaps_1var_forward3<-glm(yc~x_matc$mothers_educationgrad.hs.some.college,family=binomial(link=logit))
summary(leaps_1var_forward3)
leaps_1var_forward3<-glm(yc~x_matc$mothers_educationgrad.hs.some.college,family=binomial(link=logit))
summary(leaps_1var_forward3)
leaps_1var_forward4<-glm(yc~x_matc$mothers_educationgrad.college.beyond,family=binomial(link=logit))
summary(leaps_1var_forward4)

#########################Backward
subsets_backward <- regsubsets(sex_wo_contraception~age+ethnicity+religion+highest_yr_school_completed+know_preg_peera+fathers_education+mothers_education+educational_aspiration+dont_participate_in_school_activites_1+smoking_freq+drinking_freq+drug_freq, data=study_data_converted, method="backward", nbest=5)
plot(subsets_backward)
summary(subsets_backward)$outmat
#results:
#1- smoking_freqpartake
#2 -mothers_educationgrad.college.beyond
#3 - ethnicity2
leaps_1var_backward1<-glm(yc~x_matc$smoking_freqpartake,family=binomial(link=logit))
summary(leaps_1var_backward1)
leaps_1var_backward2<-glm(yc~x_matc$mothers_educationgrad.college.beyond,family=binomial(link=logit))
summary(leaps_1var_backward2)
leaps_1var_backward3<-glm(yc~x_matc$ethnicity2,family=binomial(link=logit))
summary(leaps_1var_backward3)


##########################################################################################
###### All models off any variables that were referenced by LEAPS ########
##########################################################################################
# fucntion to generate all possible models from combinations of variables
all_possible_models <- function(x.column.names, y.col.name){
    Cols <- x.column.names
    n <- length(Cols)
    id <- unlist(
        lapply(1:n,
               function(i)combn(1:n,i,simplify=F)
        )
        ,recursive=F)
    Formulas <- sapply(id,function(i)
        paste(y.col.name, " ~ ",paste(Cols[i],collapse="+"))
    )
    Formulas <- c(Formulas, paste( y.col.name, "~1"))
}
# fucntion that fits models based on model specification and returns model statistics 
apply_formula <- function(model, data){ #make the default value for this function 5
    fitmodel <- glm(model, family=binomial, data=data)
    explained.deviance <- (fitmodel$null.deviance - fitmodel$deviance)/fitmodel$null.deviance
    return(cbind(fitmodel$aic,explained.deviance))
}
# fucntion to generate all possible models from combinations of variables

#generating dataframe to add
x_matc <- as.data.frame(model.matrix(full_model, data=study_data_converted))
best_subset_leaps <- as.data.frame(cbind(yc,x_matc$ethnicity2, x_matc$ethnicity4, x_matc$smoking_freqpartake, x_matc$mothers_educationgrad.hs.some.college, x_matc$mothers_educationgrad.college.beyond,x_matc$religion5))
#important that we drop no.response factors, can't traget on something we don't know
colnames(best_subset_leaps) <- c("y","ethnicity2","ethnicity4","smoking_freqpartake","mothers_educationgrad.hs.some.college","mothers_educationgrad.college.beyond", "religion5")
FORMULAS <- all_possible_models(
    x.column.names=colnames(best_subset_leaps[2:dim(best_subset_leaps)[2]]),
    y.col.name=colnames(best_subset_leaps[1])
)
#costf=function(r, pi = 0) mean((r-round(pi))^2)
FORMULAS[length(FORMULAS)+1] <- "y ~ 1" # add the null model
crit <- sapply(X=FORMULAS, FUN = apply_formula, data=best_subset_leaps)
# crit <- sapply(X=FORMULAS, FUN = apply_formula)
crit <- t(crit)
model.selection <- as.data.frame(cbind(FORMULAS, crit))
colnames(model.selection)[2] <- "AIC"
colnames(model.selection)[3] <- "Explained.Deviance"
model.selection <- model.selection[order(model.selection$AIC),]
head(model.selection)
model.selection$FORMULAS = NULL
head(model.selection)

test <- cbind(rownames(model.selection),model.selection)
rownames(test) <-NULL
head(test)
test$AIC <- round(as.numeric(as.character(test$AIC)),digits=2)
test$Explained.Deviance <- round(as.numeric(as.character(test$Explained.Deviance)),digits=4)
colnames(test)[1] <- "Model"
head(test) 
xtable(head(test))

final.model <- glm(y ~ ethnicity4+smoking_freqpartake+mothers_educationgrad.hs.some.college, data=best_subset_leaps,family=binomial(link=logit))
summary(final.model)
#not useful:
#plot(final.model)

final.model3 <- glm(y ~smoking_freqpartake+mothers_educationgrad.hs.some.college, data=best_subset_leaps,family=binomial(link=logit))
summary(final.model3)
drop1(final.model3,test="F")




#Ftests
####################
drop1(final.model,test="F")
drop1(final.model3,test="F")



####Result model#####
result.model <- glm(sex_wo_contraception~smoking_freq, data=study_data_converted,family=binomial(link=logit))
summary(result.model)
plot(result.model)
plot(density(result.model$residuals))
plot(study_data_converted$sex_wo_contraception~as.factor(study_data_converted$smoking_freq))

##########################################
# Interpretation of results:

#correlation
cor(x_matc$smoking_freqpartake,y)
cor_mat <-as.data.frame(t(cor(y,x_matc)))



#plot of the correlations between the response and all of the predictors
plot(density(cor_mat[2:35]))
#deviant behaviors - the non- reference variables, so makes sense concentration above 1
colnames(cor_mat)[1] <- "Correlation"
c<- cbind(row.names(cor_mat), cor_mat)
row.names(c) <- NULL
c[order(c$Correlation,decreasing=TRUE),][1:34,]

table(x_matc$smoking_freqno_response, y)


#drop the NAs
y_noNA <- y[x_matc$smoking_freqno_response==0]
smoke_noNA <- x_matc$smoking_freqpartake[x_matc$smoking_freqno_response==0]
table(smoke_noNA,y_noNA)
table(x_matc$smoking_freqno_response)
simple.demo <- glm(y_noNA~smoke_noNA,family=binomial(link=logit))
summury(simple.demo)
exp(simple.demo$coefficients[2])